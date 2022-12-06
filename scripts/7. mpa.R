rm(list = ls())

pacman::p_load('dplyr', 'collapse', 'lubridate', 'ggplot2', 'sf', 'readr',
               'purrr', 'furrr', 'countrycode', 'tidyr')

#Downloaded from http://readme.onemap.mv/ on Dec 6 2022
#"Protected Areas of Maldives_EPA"
mpa <- st_read("data/Protected_Areas_of_Maldives(EPA_V2/Protected_Areas_of_Maldives(EPA_V2.shp")

ggplot(data = mpa) + geom_sf(col = NA, fill = 'blue')
