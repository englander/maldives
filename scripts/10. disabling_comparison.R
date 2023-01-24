#In 4, counted number of loitering and encounter events within 25 km of Maldives EEZ
#Here as comparison count events within 10 km vs 
#events 10-20 km outside or 10-20 km inside
rm(list = ls())

pacman::p_load('dplyr', 'collapse', 'lubridate', 'ggplot2', 'sf', 'readr',
               'purrr', 'furrr', 'countrycode', 'tidyr', 'flextable', 'viridis',
               'stringr', 'rworldmap', 'fixest')

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

#50 km buffer
eez <- st_buffer(eez, dist = 50*1000)

#Project into lat lon to match disab
eez <- st_transform(eez, st_crs(disab))

#Which gap events start within 50 km of EEZ?
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

#Which gap events end within 50 km of EEZ?
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
comparedf <- cell_locs 

#Drop events farther than 20 km
comparedf <- filter(comparedf, dist_km <= 20)

#events within 10km
comparedf <- mutate(comparedf, within10 = if_else(dist_km <= 10, 1, 0))

#more events begin and end within 10 km vs within 10-20 km. 
#but in terms of gap hours similar (start) or less (end)
as.data.frame(comparedf) %>% dplyr::select(-.) %>% 
  group_by(type, within10) %>% 
  summarise(n(), gaphours = sum(gap_hours))
# type  within10 `n()` gaphours
# <chr>    <dbl> <int>    <dbl>
#   1 End          0    19    1809.
# 2 End          1    42    1485.
# 3 Start        0    30    1372.
# 4 Start        1    40    1465.


#Also make RD plot for number of disabling events even though doesn't really
#make sense. where gap begins
#Only want cells within 50 km of boundary
rddf <- filter(cell_locs, type == "Start" & 
                      dist_km <= 50)

#Cut distance into 1 km widths
rddf$dist_bin <- cut(rddf$dist_km, 
                            breaks = seq(from = 0, to = 50, by = 1))

#Sum fishing hours dist bin by (inside or outside)
rddf <- group_by(rddf, location, dist_bin) %>%
  summarise(begin_gap_events = n()) %>% ungroup() 

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

#if missing events, add zero rows
rddf <- bind_rows(rddf, 
                  data.frame(location = 'inside', dist_bin_mid = seq(from = 0.5, to = 49.5, by = 1),
                             begin_gap_events = 0)
)

#Can similarly add zero rows for outside distance bins in case any missing
rddf <- bind_rows(rddf, 
                  data.frame(location = 'outside', dist_bin_mid = seq(to = -0.5, from = -49.5, by = 1),
                             begin_gap_events = 0)
)

rddf$location <- as.factor(rddf$location)


#Sum again to dist_bin (get rid of zero rows for which there was already positive fishing hours)
rddf <- group_by(rddf, dist_bin_mid, location) %>% 
  summarise(begin_gap_events = sum(begin_gap_events)) %>% 
  ungroup()

#Save
save(rddf, file = 'output/data/begin_gap_events_rddf.Rdata')


ggplot(data = rddf, aes(x = dist_bin_mid, y = begin_gap_events, col = location)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE)

rddf$location <- relevel(rddf$location, ref = "outside")

#Drop cells closest to boundary since many of them partially overlap it
#due to resolution of data
rddf <- filter(rddf, abs(dist_bin_mid) > 1)

#Output cross-sectional result (summed over years), in levels
(rdplot <- 
    ggplot(data = rddf, aes(x = dist_bin_mid, y = begin_gap_events, col = location)) + 
    geom_point(alpha = 0.4) + 
    geom_smooth(method = 'lm', se = FALSE) + 
    ylab("Number of suspected disabling events\n") + 
    xlab("\n Distance to Maldives' EEZ boundary (km)") + 
    myThemeStuff + 
    theme(legend.position = c(0.8, 0.8)) + 
    geom_vline(xintercept=0, color = "red") + 
    scale_color_manual("", values = c("dodgerblue2", "darkorange1"), 
                       labels = c("Outside", "Inside")))


ggsave(rdplot, filename = 'output/figures/begin_gap_events_rd.png', 
       height = 4, width = 6.5, dpi = 900, units = 'in')

#Difference in intercepts: Report in text but no table for now
feols(begin_gap_events ~ dist_bin_mid*location, data = rddf, se = 'hetero') %>% summary()
# OLS estimation, Dep. Var.: begin_gap_events
# Observations: 98 
# Standard-errors: Heteroskedasticity-robust 
# Estimate Std. Error   t value   Pr(>|t|)    
# (Intercept)                  2.598265   0.366838  7.082872 2.5447e-10 ***
#   dist_bin_mid                 0.002653   0.013639  0.194515 8.4619e-01    
# locationinside              -1.517143   0.531957 -2.852001 5.3417e-03 ** 
#   dist_bin_mid:locationinside -0.032245   0.017604 -1.831653 7.0170e-02 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# RMSE: 1.23854   Adj. R2: 0.44199