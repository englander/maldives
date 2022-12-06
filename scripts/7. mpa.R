rm(list = ls())

pacman::p_load('dplyr', 'collapse', 'lubridate', 'ggplot2', 'sf', 'readr',
               'purrr', 'furrr', 'countrycode', 'tidyr')

#Downloaded from http://readme.onemap.mv/ on Dec 6 2022
#"Protected Areas of Maldives_EPA"
mpa <- st_read("data/Protected_Areas_of_Maldives(EPA_V2/Protected_Areas_of_Maldives(EPA_V2.shp")

#Maldives MPA bbox
mbbox <- st_bbox(mpa)

#In .01 degree data, save rows that occur inside MPA
#Given date, filter .01 degree data to within bbox of Maldives EEZ
single_date_p01 <- function(mydate){
  
  #Read fishing effort csv for this date
  df <- read_csv(paste0(
    "data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-", 
    substring(mydate, 1, 4),"/",mydate)
  )
  
  #Shift point to center of cell (starting from lower left)
  df <- fmutate(df, cell_ll_lon = cell_ll_lon + 0.05, cell_ll_lat = cell_ll_lat + 0.05) %>% 
    #Fix if shifted across date line
    fmutate(cell_ll_lon = if_else(cell_ll_lon > 180, cell_ll_lon - 360, cell_ll_lon)) %>% 
    #Filter to inside Maldives MPA bbox
    fsubset(cell_ll_lon >= mbbox["xmin"] & 
              cell_ll_lon <= mbbox["xmax"] & 
              cell_ll_lat >= mbbox["ymin"] & 
              cell_ll_lat <= mbbox["ymax"])
  
  #Make df sf
  mysf <- st_multipoint(x = cbind(df$cell_ll_lon, df$cell_ll_lat)) %>% 
    st_sfc(crs = st_crs(mpa)) %>% 
    st_cast("POINT") %>% 
    st_sf(df)
  
  #Do any rows intersect MPA?
  inter <- st_intersects(mpa, mysf)
  
  #Which points are inside an MPA?
  insidepts <- unlist(inter) %>% unique()
  
  return(df[insidepts,])
}

#List fishing effort files
flist <- c(list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2016/"),
           list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2017/"),
           list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2018/"),
           list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2019/"),
           list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2020/"))


plan(multisession, workers = 10)

#Apply over dates
fishing_p01_list <- future_map(flist, function(x){
  try(single_date_p01(x))
})

fishing_mpa <- bind_rows(fishing_p01_list)

save(fishing_mpa, file = 'output/data/fishing_p01_obs_inside_mpa.Rdata')

#There are zero fishing hours inside MPAs!
fishing_mpa
