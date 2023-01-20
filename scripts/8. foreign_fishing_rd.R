#Fishing hours at .01 degree-gear-flag-year level  within 50 km of Maldives EEZ
rm(list = ls())

pacman::p_load('dplyr', 'collapse', 'lubridate', 'ggplot2', 'sf', 'readr',
               'purrr', 'furrr', 'countrycode', 'tidyr', 'rworldmap')

myThemeStuff <- 
  theme(panel.background = element_rect(fill=NA),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = "black", size = 11, family="sans"),
        axis.title = element_text(color = "black", size = 12, family = "sans"),
        legend.text = element_text(color = "black", size = 11, family="sans"),
        legend.title = element_text(color = "black", size = 11, family="sans")
  )

#Created in 1. read_shapes.R
load("output/data/maldives_eez_projected.Rdata")

#Buffer 50 km
buf <- st_buffer(eez, dist = 50*1000)

#Unproject
load("output/data/maldives_eez_notprojected.Rdata")

buf <- st_transform(buf, crs = st_crs(eez))

#Rename buf as eez so can use similar functions from 2. calculate_fishing.R
eez <- buf; rm(buf)

#bbox of Maldives EEZ 50 km buffer
mbbox <- st_bbox(eez)

#fleet-daily-csvs-100-v2 downloaded from GFW on Nov 10, 2022
single_date_p01 <- function(mydate){
  
  #Read fishing effort csv for this date
  df <- read_csv(paste0(
    "data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-", 
    substring(mydate, 1, 4),"/",mydate)
  ) %>% 
    #Only care about fishing effort for these data
    fsubset(fishing_hours > 0)
  
  #Drop rows missing flag
  df <- fsubset(df, !is.na(flag))
  
  #Shift point to center of cell (starting from lower left)
  df <- fmutate(df, cell_ll_lon = cell_ll_lon + 0.05, cell_ll_lat = cell_ll_lat + 0.05) %>% 
    #Fix if shifted across date line
    fmutate(cell_ll_lon = if_else(cell_ll_lon > 180, cell_ll_lon - 360, cell_ll_lon)) %>% 
    #Filter to inside  bbox
    fsubset(cell_ll_lon >= mbbox["xmin"] & 
              cell_ll_lon <= mbbox["xmax"] & 
              cell_ll_lat >= mbbox["ymin"] & 
              cell_ll_lat <= mbbox["ymax"])
  
  #Make df sf
  mysf <- st_multipoint(x = cbind(df$cell_ll_lon, df$cell_ll_lat)) %>% 
    st_sfc(crs = st_crs(eez)) %>% 
    st_cast("POINT") %>% 
    st_sf(df)
  
  #Which pts inside 50 km buffer around EEZ?
  inter <- st_intersects(eez, mysf)
  
  #Which points are inside EEZ
  insidepts <- unlist(inter) %>% unique()
  
  out <- df[insidepts,]
  
  return(out)
}



#List fishing effort files
flist <- c(list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2016/"),
           list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2017/"),
           list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2018/"),
           list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2019/"),
           list.files("data/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-2020/"))

##Apply single_date_p01 over dates 
plan(multisession, workers = 10)

fishing_p01_list <- future_map(flist, function(x){
  try(single_date_p01(x))
})

#Bind rows
fishing_p01_df <- bind_rows(fishing_p01_list)

#Sum over grid cells to flag-gear-year level
fishing_p01_df <- mutate(fishing_p01_df, year = year(date)) %>% 
  group_by(cell_ll_lat, cell_ll_lon, flag, geartype, year) %>% 
  summarise(fishing_hours = sum(fishing_hours), ais_hours = sum(hours)) %>% 
  ungroup()

#Drop domestic fishing
buf_50_foreign_fishing_hours <- filter(fishing_p01_df, flag != "MDV")

save(buf_50_foreign_fishing_hours, file = 'output/data/buf_50_foreign_fishing_hours.Rdata')

##Now calculate distance of each pixel to EEZ boundary
load('output/data/maldives_eez_notprojected.Rdata')

#distinct grid cells
cell_locs <- distinct(buf_50_foreign_fishing_hours, cell_ll_lat, cell_ll_lon)

cell_locs <- st_multipoint(x = cbind(cell_locs$cell_ll_lon, cell_locs$cell_ll_lat)) %>% 
  st_sfc(crs = st_crs(eez)) %>% 
  st_cast("POINT") %>% 
  st_sf(cell_locs)

#Easy to calculate distance to boundary for cells outside. 
#But what about for cells inside?
#Separate cells in inside and those outside
#For inside, subtract a smaller inner buffer of eez from the actual eez to get boundary

inside <- st_intersects(eez, cell_locs)

outside <- cell_locs[-inside[[1]],]

inside <- cell_locs[inside[[1]],]

##THIS IS WHERE I AM
#From 1, if inside pt is inside convex hull of land area, just remove its distance

#Land area of Maldives
land <- getMap(resolution = 'high')
land <- st_as_sf(land)
land <- filter(land, ADMIN.1 == "Maldives")

#The atoll rim is the convex hull of the land area
atollrim <- st_convex_hull(land)

#Give a small buffer so when union with eez it eliminates all islands
atollrim <- st_transform(atollrim, st_crs("+proj=laea +lon_0=73.2"))
atollrim <- st_buffer(atollrim, dist = 10*1000)

eez <- st_transform(eez, st_crs(atollrim))

#Use this version of eez to derive boundary
#It has no islands so distance will be distance to boundary, not distance to island
eez <- st_union(eez, atollrim)

#Confirm no islands
ggplot(data = eez) + geom_sf()

#Now calculate boundary
outerbuf <- st_buffer(eez, dist = 100)

boundary <- st_difference(outerbuf, eez)

ggplot(data = boundary) + geom_sf(fill = 'blue')

#Make eez and boundary same crs as cell locations
eez <- st_transform(eez, crs = st_crs(cell_locs))
boundary <- st_transform(boundary, crs = st_crs(cell_locs))

#Calculate distance to boundary of cells inside eez
inside_dist <- st_distance(inside, boundary)

#Add distance as column
inside <- mutate(inside, dist_km = as.numeric(inside_dist)) %>% 
  mutate(dist_km = dist_km / 1000)

#Calculate distance to eez of cells outside eez
outside_dist <- st_distance(outside, eez)

outside <- mutate(outside, dist_km = as.numeric(outside_dist)) %>% 
  mutate(dist_km = dist_km / 1000)

#bind inside and outside cells together
cell_locs <- bind_rows(
  mutate(outside, location = 'outside'), 
  mutate(inside, location = 'inside')
)

#Join dist_km of each cell onto fishing df
buf_50_foreign_fishing_hours <- left_join(buf_50_foreign_fishing_hours, 
                            as.data.frame(cell_locs) %>% dplyr::select(-.), 
                            by = c("cell_ll_lat", "cell_ll_lon"))

#Only want cells within 50 km of boundary
buf_50_foreign_fishing_hours <- filter(buf_50_foreign_fishing_hours, 
                                       dist_km <= 50)

#Cut distance into 1 km widths
buf_50_foreign_fishing_hours$dist_bin <- cut(buf_50_foreign_fishing_hours$dist_km, 
                                             breaks = seq(from = 0, to = 50, by = 1))

#Sum fishing hours to year by dist bin by (inside or outside)
rddf <- group_by(buf_50_foreign_fishing_hours, year, location, dist_bin) %>%
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

rddf$location <- as.factor(rddf$location)

ggplot(data = rddf, aes(x = dist_bin_mid, y = log_fishing_hours, col = location)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

ggplot(data = rddf, aes(x = dist_bin_mid, y = fishing_hours, col = location)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

#Sum over years
rddf <- group_by(rddf, dist_bin_mid, location) %>% 
  summarise(fishing_hours = sum(fishing_hours)) %>% 
  ungroup() %>% mutate(log_fishing_hours = log(fishing_hours))

ggplot(data = rddf, aes(x = dist_bin_mid, y = log_fishing_hours, col = location)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

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
    xlab("\n Distance to Maldives' EEZ boundary (km)") + 
      myThemeStuff + 
    theme(legend.position = c(0.8, 0.8)) + 
    geom_vline(xintercept=0, color = "red") + 
      scale_color_manual("", values = c("dodgerblue2", "darkorange1"), 
                         labels = c("Outside", "Inside")))


ggsave(rdplot, filename = 'output/figures/foreign_fishing_rd.png', 
       height = 4, width = 6.5, dpi = 900, units = 'in')
