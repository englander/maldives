#Count number of loitering and encounter events within Maldives EEZ
#There are no encounter events within 25 km of Maldives EEZ
#There are no loitering events outside Maldives EEZ within 25 km
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
load('output/data/maldives_land_projected.Rdata')

atollrim <- st_transform(atollrim, st_crs(eez))

#make loitering sf
loit <- st_multipoint(x = cbind(loit$lon, loit$lat)) %>% 
  st_sfc(crs = st_crs(eez)) %>% 
  st_cast("POINT") %>% 
  st_sf(loit)

inter <- st_intersects(atollrim, loit)

#loitering events outside atoll rim
loit <- loit[inter[[1]], ]

#Loitering events between 2016 and 2020
loit <- mutate(loit, start = as.Date(start), end = as.Date(end)) %>% 
  mutate(year = year(start)) %>% 
  filter(year >= 2016 & year <= 2020)

(loitplot <- ggplot() + 
    geom_sf(data = atollrim, fill = NA) + 
    geom_sf(data = eez, fill = NA) + 
    geom_sf(data = land, fill = 'grey60', col = 'grey60') + 
    geom_sf(data = loit, aes(col = loitering.loitering_hours), alpha = 0.5) + 
    scale_color_viridis("Loitering\nhours", trans='log', breaks = c(1.1, 3, 7, 20, 55), 
                        labels = as.character(c(1, 3, 7, 20, 55))) + 
    myThemeStuff)

ggsave(loitplot, filename = 'output/figures/loitering_events.png', 
       height = 4, width = 4, dpi = 900, units = 'in')


loit <- as.data.frame(loit) %>% dplyr::select(-`.`) %>% as_tibble()

##Table: year, loitering events
yeardf <- group_by(loit, year) %>% 
  summarise(loitering_events = n(),
            loitering_hours = sum(loitering.loitering_hours)) %>% 
  ungroup() %>% 
  mutate(year = as.character(year))

#Add Total row (and 2016, since none that year)
yeardf <- bind_rows(
  data.frame(year = "2016", loitering_events = 0, loitering_hours = 0),
  yeardf,
  data.frame(year = "Total", loitering_events = nrow(loit), 
             loitering_hours = sum(loit$loitering.loitering_hours))
) 

yeardf$loitering_hours <- sapply(yeardf$loitering_hours, function(x){
  formNum(x, 1)
})

names(yeardf) <- c("Year", "Loitering events", "Loitering hours")

(yeartab <- flextable(yeardf) %>% 
    theme_booktabs() %>%
    set_caption(caption = "Table 9: Loitering events by year") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 5, j = 1:3) %>% 
    autofit()
)

save_as_docx(yeartab, path = 'output/tables/loitering_year.docx')


#Cannot make table by gear because all gears are carrier

##Table: flag, loitering events
flagdf <- group_by(loit, vessel.flag) %>% 
  summarise(loitering_events = n(), 
            loitering_hours = sum(loitering.loitering_hours)) %>% 
  ungroup() %>% 
  rename(flag = vessel.flag) %>% 
  mutate(flag = as.character(flag))

#Replace iso3 code with countryname
flagdf$flag <- countrycode(flagdf$flag, origin = 'iso3c', destination = 'country.name')

flagdf <- arrange(flagdf, desc(loitering_events))

#Add Total row
flagdf <- bind_rows(
  flagdf,
  data.frame(flag = "Total", loitering_events = nrow(loit), 
             loitering_hours = sum(loit$loitering.loitering_hours))
)


flagdf$loitering_hours <- sapply(flagdf$loitering_hours, function(x){
  formNum(x, 1)
})

names(flagdf) <- c("Flag", "Loitering events", "Loitering hours")

(flagtab <- flextable(flagdf) %>% 
    theme_booktabs() %>%
    set_caption(caption = "Table 10: Loitering events by flag") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 7, j = 1:3) %>% 
    autofit()
)

save_as_docx(flagtab, path = 'output/tables/loitering_flag.docx')
