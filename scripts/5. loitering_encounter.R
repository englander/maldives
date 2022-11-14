#Count number of loitering and encounter events within Maldives EEZ
#There are no encounter events within 25 km of Maldives EEZ
rm(list = ls())

pacman::p_load('dplyr', 'collapse', 'lubridate', 'ggplot2', 'sf', 'readr',
               'purrr', 'furrr', 'countrycode', 'tidyr', 'flextable', 'viridis',
               'stringr')

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
loit <- read_csv("data/loitering.csv")

#Created in 1. read_shapes.R
load("output/data/maldives_atollrim_projected.Rdata")
load("output/data/maldives_eez_notprojected.Rdata")

atollrim <- st_transform(atollrim, st_crs(eez))

#make loitering sf
loit <- st_multipoint(x = cbind(loit$lon, loit$lat)) %>% 
  st_sfc(crs = st_crs(eez)) %>% 
  st_cast("POINT") %>% 
  st_sf(loit)

#Projected EEZ so can create 25 km buffer. Created in 1. read_shapes.R
load('output/data/maldives_eez_projected.Rdata')

buf <- st_buffer(eez, dist = 25*1000)

buf <- st_transform(buf, st_crs(loit))

#Which gap events start outside of atoll rim, within 25 km EEZ?
atollinter <- st_intersects(atollrim, loit)
bufinter <- st_intersects(buf, loit)

#loitering events both outside atoll rim and within 25 km of EEZ
loit <- loit[base::intersect(atollinter[[1]], bufinter[[1]]), ]


#Also plot locations of encounter events
encounter <- read_csv("data/encounter.csv")

encounter <- st_multipoint(x = cbind(encounter$lon, encounter$lat)) %>% 
  st_sfc(crs = st_crs(buf)) %>% 
  st_cast("POINT") %>% 
  st_sf(encounter)

#Which gap events end within 25 km of EEZ?
atollinter <- st_intersects(atollrim, encounter)
bufinter <- st_intersects(buf, encounter)
base::intersect(atollinter[[1]], bufinter[[1]])

#There are no encounter events within Maldives EEZ, so cannot plot

encounter <- encounter[inter[[1]],]

#Bind start and end disabling together so can make type color
togdf <- bind_rows(
  mutate(loit, type = "Start"),
  mutate(encounter, type = "End")
)

togdisab$type <- as.factor(togdisab$type)
togdisab$type <- relevel(togdisab$type, ref = "Start")

(disabplot <- ggplot() + 
    geom_sf(data = eez, fill = NA, size = 0.25) + 
    geom_sf(data = land, fill = 'grey60', col = 'grey60') + 
    geom_sf(data = togdisab, aes(col = type), alpha = 0.35) + 
    scale_color_manual("Disabling event", values = c("dodgerblue2", "darkorange1")) + 
    myThemeStuff + 
    ggtitle("Disabling events within 25 km, 2017-2019"))

ggsave(disabplot, filename = 'output/figures/disabling_events.png', 
       height = 4, width = 4, dpi = 900, units = 'in')