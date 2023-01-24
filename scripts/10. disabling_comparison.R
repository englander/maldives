#In 4, counted number of loitering and encounter events within 25 km of Maldives EEZ
#Here as comparison count events within 10 km vs 
#events 10-20 km outside or 10-20 km inside
rm(list = ls())

pacman::p_load('dplyr', 'collapse', 'lubridate', 'ggplot2', 'sf', 'readr',
               'purrr', 'furrr', 'countrycode', 'tidyr', 'flextable', 'viridis',
               'stringr', 'rworldmap')

myThemeStuff <- 
  theme(panel.background = element_rect(fill=NA),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = "black", size = 11, family="sans"),
        axis.title = element_text(color = "black", size = 12, family = "sans"),
        legend.text = element_text(color = "black", size = 11, family="sans"),
        legend.title = element_text(color = "black", size = 11, family="sans")
  )

set_flextable_defaults(font.family = "Calibri (Body)", 
                       font.size = "12")

formNum <- function(num, dig){
  
  #Round
  roundnum <- round(num, dig) %>% as.character()
  
  #If rounded to integer, need to add "." to end
  if(length(grep("\\.",roundnum))==0){
    roundnum <- paste0(roundnum, ".")
  }
  
  #Add an extra zero beyond the decimal point if needed to get same length
  #Do num first
  roundnum <- sapply(seq_len(length(roundnum)), function(x){
    if(gsub(".*\\.","",roundnum[x]) %>% nchar() < dig){
      #Needed length
      zerosneeded <- dig - gsub(".*\\.","",roundnum[x]) %>% nchar()
      roundnum[x] <- paste0(roundnum[x],paste0(rep(0,zerosneeded),collapse=""))
    } else{
      roundnum[x]
    }
  })
  
  #Add commas if necessary
  roundnum <- prettyNum(roundnum, ",")
  
  return(roundnum)
}

#Downloaded on Nov 14, 2022 from GFW
disab <- read_csv("data/ais_disabling_events.csv")

#Created in 1. read_shapes.R
load("output/data/maldives_eez_notprojected.Rdata")
load('output/data/maldives_land_projected.Rdata')

#Make sf
disab <- st_multipoint(x = cbind(disab$gap_start_lon, disab$gap_start_lat)) %>% 
  st_sfc(crs = st_crs(eez)) %>% 
  st_cast("POINT") %>% 
  st_sf(disab)

#Created in 1. read_shapes.R
load('output/data/maldives_eez_projected.Rdata')

#25 km buffer
eez <- st_buffer(eez, dist = 25*1000)

#Project into lat lon to match disab
eez <- st_transform(eez, st_crs(disab))

#Which gap events start within 25 km of EEZ?
inter <- st_intersects(eez, disab)

disab <- disab[inter[[1]],]

#Rename eez as buffer
buf <- eez

#Reload eez so can plot both buffer and eez
load("output/data/maldives_eez_notprojected.Rdata")

#Also plot locations where disabling events end
enddisab <- read_csv("data/ais_disabling_events.csv")

enddisab <- st_multipoint(x = cbind(enddisab$gap_end_lon, enddisab$gap_end_lat)) %>% 
  st_sfc(crs = st_crs(eez)) %>% 
  st_cast("POINT") %>% 
  st_sf(enddisab)

#Which gap events end within 25 km of EEZ?
inter <- st_intersects(buf, enddisab)

enddisab <- enddisab[inter[[1]],]

#Bind start and end disabling together so can make type color
togdisab <- bind_rows(
  mutate(disab, type = "Start"),
  mutate(enddisab, type = "End")
)

#Now calculate distance to boundary
load('output/data/maldives_eez_notprojected.Rdata')

#distinct points
cell_locs <- togdisab

#Easy to calculate distance to boundary for cells outside. 
#But what about for cells inside?
#Separate cells in inside and those outside
#For inside, subtract a smaller inner buffer of eez from the actual eez to get boundary

inside <- st_intersects(eez, cell_locs)

outside <- cell_locs[-inside[[1]],]

inside <- cell_locs[inside[[1]],]

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

#Compare number of disabling events and gap hours for events that
#begin or end within 10 km vs 10-20 km away from boundary
comparedf <- as.data.frame(cell_locs) %>% dplyr::select(-.)

#Drop events farther than 20 km
comparedf <- filter(comparedf, dist_km <= 20)

#events within 10km
comparedf <- mutate(comparedf, within10 = if_else(dist_km <= 10, 1, 0))

#more events begin and end within 10 km vs within 10-20 km. 
#but in terms of gap hours similar (start) or less (end)
group_by(comparedf, type, within10) %>% 
  summarise(n(), gaphours = sum(gap_hours))
# type  within10 `n()` gaphours
# <chr>    <dbl> <int>    <dbl>
#   1 End          0    19    1809.
# 2 End          1    42    1485.
# 3 Start        0    30    1372.
# 4 Start        1    40    1465.