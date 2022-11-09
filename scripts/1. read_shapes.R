#Save shapes: Maldives EEZ, Maldives atoll rim, and Maldives land area

rm(list = ls())

pacman::p_load('dplyr', 'sf', 'ggplot2', 'rworldmap')

#World EEZ v11 downloaded from https://www.marineregions.org/downloads.php on September 23, 2021
eez <- st_read("data/World_EEZ_v11_20191118/eez_v11.shp")
eez <- filter(eez, GEONAME == "Maldives Exclusive Economic Zone")

#Land area of Maldives
land <- getMap(resolution = 'high')
land <- st_as_sf(land)
land <- filter(land, ADMIN.1 == "Maldives")

#The atoll rim is the convex hull of the land area
atollrim <- st_convex_hull(land)

#Give a small buffer to ensure transshippment events are definitely outside rim
atollrim <- st_transform(atollrim, st_crs("+proj=laea +lon_0=73.2"))
atollrim <- st_buffer(atollrim, dist = 10*1000)

#Will want to measure transshipping outside of atoll rim, so subtract atoll rim from EEZ
#Give same projection
eez <- st_transform(eez, st_crs("+proj=laea +lon_0=73.2"))

atollrim <- st_difference(eez, atollrim)

#Give land same projection as well
land <- st_transform(land, st_crs("+proj=laea +lon_0=73.2"))

#Save all three objects
save(land, file = 'output/data/maldives_land.Rdata')
save(eez, file = 'output/data/maldives_eez.Rdata')
save(atollrim, file = 'output/data/maldives_atollrim.Rdata')
