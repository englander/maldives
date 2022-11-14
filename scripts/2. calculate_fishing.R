#Produce two datasets: one at level of date-.1 degree-vessel (for fishing-KW hours and fishing vessels)
#and other at .01 degree-gear-flag level  (for fishing hours  and AIS hours)
rm(list = ls())

pacman::p_load('dplyr', 'collapse', 'lubridate', 'ggplot2', 'sf', 'readr',
               'purrr', 'furrr', 'countrycode', 'tidyr')

#Created in 1. read_shapes.R
load("output/data/maldives_eez_notprojected.Rdata")

#bbox of Maldives EEZs
mbbox <- st_bbox(eez)

#fishing vessels csv downloaded from GFW on April 7, 2022
vesselinfo <- read_csv('data/fishing-vessels-v2.csv')

#fleet-daily-csvs-100-v2 downloaded from GFW on Nov 10, 2022
#mmsi-daily-csvs-10-v2 downloaded from GFW on Sep 6, 2022

#Prepare data at level of date-.1 degree-vessel
single_date_p1 <- function(mydate){
  
  #Read fishing effort csv for this date
  df <- read_csv(paste0(
    "data/mmsi-daily-csvs-10-v2/mmsi-daily-csvs-10-v2-", 
    substring(mydate, 1, 4),"/",mydate)
  ) %>% 
    #Only care about fishing effort for these data
    fsubset(fishing_hours > 0)
  
  #Join flag onto effort
  df <- left_join(df, 
                  dplyr::select(vesselinfo, mmsi, flag_gfw, engine_power_kw_gfw, vessel_class_gfw), by = 'mmsi')
  
  #Drop rows missing flag
  df <- fsubset(df, !is.na(flag_gfw))
  
  #Shift point to center of cell (starting from lower left)
  df <- fmutate(df, cell_ll_lon = cell_ll_lon + 0.05, cell_ll_lat = cell_ll_lat + 0.05) %>% 
    #Fix if shifted across date line
    fmutate(cell_ll_lon = if_else(cell_ll_lon > 180, cell_ll_lon - 360, cell_ll_lon)) %>% 
    #Filter to inside African EEZ bbox
    fsubset(cell_ll_lon >= mbbox["xmin"] & 
              cell_ll_lon <= mbbox["xmax"] & 
              cell_ll_lat >= mbbox["ymin"] & 
              cell_ll_lat <= mbbox["ymax"]) %>% 
    #fishing-KW hours
    fmutate(fishing_kw_hours = fishing_hours * engine_power_kw_gfw)
  
  #Make df sf
  mysf <- st_multipoint(x = cbind(df$cell_ll_lon, df$cell_ll_lat)) %>% 
    st_sfc(crs = st_crs(eez)) %>% 
    st_cast("POINT") %>% 
    st_sf(df)
  
  #Which pts inside EEZ?
  inter <- st_intersects(eez, mysf)
  
  #Which points are inside EEZ
  insidepts <- unlist(inter) %>% unique()
  
  out <- df[insidepts,]
  
  return(out)
}

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
    #Filter to inside African EEZ bbox
    fsubset(cell_ll_lon >= mbbox["xmin"] & 
              cell_ll_lon <= mbbox["xmax"] & 
              cell_ll_lat >= mbbox["ymin"] & 
              cell_ll_lat <= mbbox["ymax"])
  
  #Apply date_flag over flags
  out <- map_df(unique(df$flag), function(x){
    date_flag_p01(df, x)
  }) %>% 
    #Add date as column
    mutate(date = substring(mydate, 1, 10) %>% as.Date())
  
  return(out)
}

#fishing hours and AIS hours for day-flag
date_flag_p01 <- function(mydf, myflag){
  
  #Fishing by vessels of this flag
  myfishing <- fsubset(mydf, flag == myflag)
  
  #Make myfishing sf
  mysf <- st_multipoint(x = cbind(myfishing$cell_ll_lon, myfishing$cell_ll_lat)) %>% 
    st_sfc(crs = st_crs(eez)) %>% 
    st_cast("POINT") %>% 
    st_sf(myfishing)
  
  #Apply date_flag_gear over gears
  out <- map_df(unique(mysf$geartype), function(x){
    date_flag_gear_p01(mysf, myflag, x)
  })
  
  return(out)
}

#Hours fished and AIS hours for single day-flag-gear
date_flag_gear_p01 <- function(mysf, myflag, mygear){
  
  #Filter to given gear
  mysf <- fsubset(mysf, geartype == mygear)
  
  inter <- st_intersects(eez, mysf)
  
  out <- data.frame(flag = myflag, gear = mygear)
  
  if(length(inter) == 0){
    
    fishhours <- 0
    aishours <- 0
    
  }else{
    
    #Which points are inside EEZ
    insidepts <- unlist(inter) %>% unique()
    
    fishhours <-  mysf$fishing_hours[insidepts] %>% sum()
    aishours <- mysf$hours[insidepts] %>% sum()
    
  }
  
  out <- mutate(out, fishing_hours = fishhours, ais_hours = aishours)
  
  return(out)
}


##Apply single_date_p1 over dates

#List fishing effort files
flist <- c(list.files("data/mmsi-daily-csvs-10-v2/mmsi-daily-csvs-10-v2-2016/"),
           list.files("data/mmsi-daily-csvs-10-v2/mmsi-daily-csvs-10-v2-2017/"),
           list.files("data/mmsi-daily-csvs-10-v2/mmsi-daily-csvs-10-v2-2018/"),
           list.files("data/mmsi-daily-csvs-10-v2/mmsi-daily-csvs-10-v2-2019/"),
           list.files("data/mmsi-daily-csvs-10-v2/mmsi-daily-csvs-10-v2-2020/"))

plan(multisession, workers = 10)

fishing_p1_list <- future_map(flist, function(x){
  try(single_date_p1(x))
})

#Bind rows
fishing_p1_df <- bind_rows(fishing_p1_list)

save(fishing_p1_df, file = 'output/data/fishing_p1_df.Rdata')


##Apply single_date_p01 over dates as well
plan(multisession, workers = 10)

fishing_p01_list <- future_map(flist, function(x){
  try(single_date_p01(x))
})

#Bind rows
fishing_p01_df <- bind_rows(fishing_p01_list)

#Sum to year level
flag_gear_year_df <- mutate(fishing_p01_df, year = year(date)) %>% 
  group_by(flag, gear, year) %>% 
  summarise(fishing_hours = sum(fishing_hours), ais_hours = sum(ais_hours)) %>% 
  ungroup()

save(flag_gear_year_df, file = 'output/data/flag_gear_year_df.Rdata')


#Identify domestic vessels that fish inside Maldivian waters
domves <- filter(fishing_p1_df, flag_gfw == "MDV") %>% distinct(mmsi) %>% as.matrix() %>% as.character()

filter(vesselinfo, mmsi %in% domves)

#look up names on marine traffic:
#455364000 is LEYHNU 609
#455365000 is LU 703
#455388000 is BAHARI NUSANTARA 3

filter(fishing_p1_df, mmsi %in% domves) %>% as.data.frame()


#Also calculate fishing hours and AIS hours each year so can better understand trend 
#in fishing inside Maldives EEZ
single_date_anywhere_p1 <- function(mydate){
  
  #Read fishing effort csv for this date
  df <- read_csv(paste0(
    "data/mmsi-daily-csvs-10-v2/mmsi-daily-csvs-10-v2-", 
    substring(mydate, 1, 4),"/",mydate)
  ) %>% 
    #Join on engine power onto effort
    left_join(dplyr::select(vesselinfo, mmsi, engine_power_kw_gfw), by = 'mmsi') %>% 
    mutate(fishing_kw_hours = fishing_hours * engine_power_kw_gfw) %>%
    summarise(hours = sum(hours, na.rm = TRUE), 
              fishing_hours = sum(fishing_hours, na.rm = TRUE),
              fishing_kw_hours = sum(fishing_kw_hours, na.rm = TRUE)) %>% 
    #Add date as column
    mutate(date = substring(mydate, 1, 10) %>% as.Date())
  
  return(df)
}
  
plan(multisession, workers = 10)

fishing_anywhere_p1_list <- future_map(flist, function(x){
  try(single_date_anywhere_p1(x))
})

fishing_anywhere_p1_df <- bind_rows(fishing_anywhere_p1_list) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(hours = sum(hours, na.rm = TRUE), 
            fishing_hours = sum(fishing_hours, na.rm = TRUE),
            fishing_kw_hours = sum(fishing_kw_hours, na.rm = TRUE))

save(fishing_anywhere_p1_df, file = 'output/data/fishing_anywhere_p1_df.Rdata')
