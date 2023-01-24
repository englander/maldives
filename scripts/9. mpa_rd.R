rm(list = ls())

pacman::p_load('dplyr', 'collapse', 'lubridate', 'ggplot2', 'sf', 'readr',
               'purrr', 'furrr', 'countrycode', 'tidyr')

myThemeStuff <- 
  theme(panel.background = element_rect(fill=NA),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = "black", size = 11, family="sans"),
        axis.title = element_text(color = "black", size = 12, family = "sans"),
        legend.text = element_text(color = "black", size = 11, family="sans"),
        legend.title = element_text(color = "black", size = 11, family="sans")
  )

#Downloaded from http://readme.onemap.mv/ on Dec 6 2022
#"Protected Areas of Maldives_EPA"
mpa <- st_read("data/Protected_Areas_of_Maldives(EPA_V2/Protected_Areas_of_Maldives(EPA_V2.shp")

#use same projection as atoll rim
load("output/data/maldives_atollrim_projected.Rdata")

mpa <- st_transform(mpa, crs = st_crs(atollrim))

#identify fishing within 50 km of MPAs
mpa <- st_buffer(mpa, dist = 50*1000)

#unproject
load("output/data/maldives_eez_notprojected.Rdata")

mpa <- st_transform(mpa, crs = st_crs(eez))

#bbox
mbbox <- st_bbox(mpa)

rm(eez, atollrim)

#In .01 degree data, save rows that occur within 50 km of MPA
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
  
  #Do any rows intersect 50 km buffer of MPA?
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


plan(multisession, workers = 6)

#Apply over dates
fishing_p01_list <- future_map(flist, function(x){
  try(single_date_p01(x))
})

fishing_mpa <- bind_rows(fishing_p01_list)

save(fishing_mpa, file = 'output/data/fishing_p01_obs_50_km_of_mpa.Rdata')

#Now calculate distance of each grid cell to MPA boundary
#Already know that no fishing inside MPAs from 7. mpa.R
#Recover initial mpa shapefiles
mpa <- st_read("data/Protected_Areas_of_Maldives(EPA_V2/Protected_Areas_of_Maldives(EPA_V2.shp")


#distinct grid cells
cell_locs <- distinct(fishing_mpa, cell_ll_lat, cell_ll_lon)

cell_locs <- st_multipoint(x = cbind(cell_locs$cell_ll_lon, cell_locs$cell_ll_lat)) %>% 
  st_sfc(crs = st_crs(mpa)) %>% 
  st_cast("POINT") %>% 
  st_sf(cell_locs)

#Calculate distance 
outside_dist <- st_distance(cell_locs, mpa)

cell_locs <- mutate(cell_locs, dist_km = as.numeric(outside_dist)) %>% 
  mutate(dist_km = dist_km / 1000)

#Join dist_km of each cell onto fishing df
fishing_mpa <- left_join(fishing_mpa, 
                         as.data.frame(cell_locs) %>% dplyr::select(-.), 
                         by = c("cell_ll_lat", "cell_ll_lon")) %>% 
  mutate(location = 'outside')

#Only want cells within 50 km of boundary
fishing_mpa <- filter(fishing_mpa, 
                      dist_km <= 50)

#Cut distance into 1 km widths
fishing_mpa$dist_bin <- cut(fishing_mpa$dist_km, 
                                             breaks = seq(from = 0, to = 50, by = 1))

#Sum fishing hours to year by dist bin by (inside or outside)
rddf <- group_by(fishing_mpa, year, location, dist_bin) %>%
  summarise(fishing_hours = sum(fishing_hours)) %>% ungroup() %>%
  mutate(log_fishing_hours = log(fishing_hours))

#Record midpoint of each bin
middf <- distinct(rddf, dist_bin) %>% 
  mutate(dist_bin_num = dist_bin)
middf$dist_bin_num <- as.character(middf$dist_bin_num)
middf$dist_bin_num <- gsub("\\(", "", middf$dist_bin_num)
middf$dist_bin_num <- gsub("\\]", "", middf$dist_bin_num)
middf$dist_bin_num <- gsub(",.*","",middf$dist_bin_num)
middf <- mutate(middf, dist_bin_num = as.numeric(dist_bin_num) + 0.5)

#Join onto rddf
rddf <- left_join(rddf, rename(middf, dist_bin_mid = dist_bin_num), 
                  by = 'dist_bin')

#if outside, make distance negative
rddf <- mutate(rddf, dist_bin_mid = if_else(location == "outside",
                                            -dist_bin_mid, dist_bin_mid))

#There is no fishing inside mpas, so need to manually add these rows
rddf <- bind_rows(rddf, 
                  data.frame(location = 'inside', dist_bin_mid = seq(from = 0.5, to = 49.5, by = 1),
                             fishing_hours = 0)
)

#Can similarly add zero rows for outside distance bins in case any missing
rddf <- bind_rows(rddf, 
                  data.frame(location = 'outside', dist_bin_mid = seq(from = -0.5, to = -49.5, by = 1),
                             fishing_hours = 0)
                  )

rddf$location <- as.factor(rddf$location)


#Sum over years
rddf <- group_by(rddf, dist_bin_mid, location) %>% 
  summarise(fishing_hours = sum(fishing_hours)) %>% 
  ungroup() #can't do logs because no fishing inside MPAs

#Save
save(rddf, file = 'output/data/mpa_rddf.Rdata')


ggplot(data = rddf, aes(x = dist_bin_mid, y = fishing_hours, col = location)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

rddf$location <- relevel(rddf$location, ref = "outside")

#Drop cells closest to boundary since many of them partially overlap it
#due to resolution of data
rddf <- filter(rddf, abs(dist_bin_mid) > 1)

#Output cross-sectional result (summed over years), in levels
(rdplot <- 
    ggplot(data = rddf, aes(x = dist_bin_mid, y = fishing_hours, col = location)) + 
    geom_point(alpha = 0.4) + 
    geom_smooth(method = 'lm', se = FALSE) + 
    ylab("Fishing hours\n") + 
    xlab("\n Distance to MPA boundary (km)") + 
    myThemeStuff + 
    theme(legend.position = c(0.8, 0.8)) + 
    geom_vline(xintercept=0, color = "red") + 
    scale_color_manual("", values = c("dodgerblue2", "darkorange1"), 
                       labels = c("Outside", "Inside")))


ggsave(rdplot, filename = 'output/figures/mpa_rd.png', 
       height = 4, width = 6.5, dpi = 900, units = 'in')

#Difference in intercepts: Report in text but no table for now
feols(fishing_hours ~ dist_bin_mid*location, data = rddf, se = 'hetero') %>% summary()


##Finally plot fishing hours within 50 km of MPAs

#Created in 1. read_shapes.R
load("output/data/maldives_eez_notprojected.Rdata")
load('output/data/maldives_land_projected.Rdata')

land <- st_transform(land, crs = st_crs(eez))

#Sum over years, gears, and flags
plotdf <- group_by(buf_50_foreign_fishing_hours, cell_ll_lat, cell_ll_lon) %>% 
  summarise(fishing_hours = sum(fishing_hours)) %>% ungroup()

foreignplot <- ggplot() + 
  geom_sf(data = land, fill = 'grey60', col = 'grey60') + 
  geom_tile(data = plotdf, aes(y = cell_ll_lat, x = cell_ll_lon, fill = fishing_hours),
            width = .01, height = .01) + 
  geom_sf(data = eez, fill = NA) + 
  myThemeStuff + 
  scale_fill_viridis("Fishing\nhours",trans='log', 
                     breaks = c(.05,3, 30), labels = c("0.05","3","30")) + 
  xlab("") + ylab("")


#Make sure to note in text this figure is .01 resolution, while others are .1 degree resolution
ggsave(foreignplot, filename = 'output/figures/foreign_fishing_within_50km.png', 
       height = 4, width = 4, dpi = 900, units = 'in')
