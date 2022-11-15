#Figure and tables summarizing foreign fishing in Maldivian waters
rm(list = ls())

pacman::p_load('dplyr', 'lubridate', 'ggplot2', 'sf', 
               'countrycode', 'tidyr', 'viridis', 'flextable', 
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


#Created in 1. read_shapes.R
load("output/data/maldives_eez_notprojected.Rdata")
load('output/data/maldives_land_projected.Rdata')

#Created in 2. calculate_fishing.R
load('output/data/fishing_p1_df.Rdata')
load('output/data/flag_gear_year_df.Rdata')

#fishing_kw_hours by domestic unauthorized vessels summed over years and gears
#I confirmed that the three Maldivian vessels are not on the authorized name list by searching their MMSIs
#which gave me their names
plotdf <- filter(fishing_p1_df, flag_gfw == "MDV") %>% 
  group_by(cell_ll_lat, cell_ll_lon) %>% 
  summarise(fishing_kw_hours = sum(fishing_kw_hours)) %>% 
  ungroup()

(domesticplot <- ggplot() + 
  geom_sf(data = eez, fill = NA) + 
    geom_sf(data = land, fill = 'grey60', col = 'grey60') + 
  geom_tile(data = plotdf, aes(y = cell_ll_lat, x = cell_ll_lon, fill = fishing_kw_hours),
             width = .1, height = .1) + 
  myThemeStuff + 
  scale_fill_viridis("Fishing-kW\nhours",trans='log',
                      breaks = c(150, 1000, 20000), labels = c("150", "1,000", "20,000")) +
  xlab("") + ylab("")
)

ggsave(domesticplot, filename = 'output/figures/domestic_fishingkwhours.png', 
       height = 4, width = 4, dpi = 900, units = 'in')


##Table: year, fishing-kw hours, fishing hours, fishing vessels
yeardf <- left_join(
  filter(flag_gear_year_df, flag == "MDV") %>% 
    group_by(year) %>% 
    summarise(fishing_hours = sum(fishing_hours)) %>% 
    ungroup(),
  filter(fishing_p1_df, flag_gfw == "MDV") %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(fishing_kw_hours = sum(fishing_kw_hours)) %>% 
  ungroup(), 
  by = 'year') %>% 
  left_join(
    filter(fishing_p1_df, flag_gfw == "MDV") %>% 
      mutate(year = year(date)) %>% 
      distinct(year, mmsi) %>% 
      group_by(year) %>% 
      count(), 
    by = 'year'
  ) %>% 
  dplyr::select(year, fishing_kw_hours, fishing_hours, n) %>% 
  mutate(year = as.character(year))

#Add total row and 2019 and 2020 no fishing
yeardf <- bind_rows(yeardf, 
                    data.frame(year = c(2019, 2020, "Total"), 
                               fishing_hours = c(0, 0, sum(flag_gear_year_df$fishing_hours[flag_gear_year_df$flag == "MDV"])),
                               fishing_kw_hours = c(0, 0, sum(fishing_p1_df$fishing_kw_hours[fishing_p1_df$flag_gfw == "MDV"])),
                               n = c(0, 0, unique(fishing_p1_df$mmsi[fishing_p1_df$flag_gfw == "MDV"]) %>% length()))
)


  
names(yeardf) <- c("Year", "Fishing-kW hours", "Fishing hours", "Fishing vessels")

yeardf$`Fishing-kW hours` <- formNum(yeardf$`Fishing-kW hours`, 0)
yeardf$`Fishing hours` <- formNum(yeardf$`Fishing hours`, 0)

(yeartab <- flextable(yeardf) %>% 
    theme_booktabs() %>%
  set_caption(caption = "Table 4: Apparent domestic fishing by year") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 5, j = 1:4) %>%
    autofit()
)

save_as_docx(yeartab, path = 'output/tables/domestic_year.docx')



## Table by gear
geardf <- left_join(
  filter(flag_gear_year_df, flag == "MDV") %>% 
    group_by(gear) %>% 
    summarise(fishing_hours = sum(fishing_hours)) %>% 
    ungroup() %>% 
    arrange(desc(fishing_hours)),
  filter(fishing_p1_df, flag_gfw == "MDV") %>% 
    group_by(vessel_class_gfw) %>% 
    summarise(fishing_kw_hours = sum(fishing_kw_hours)) %>% 
    ungroup() %>% 
    rename(gear = vessel_class_gfw), 
  by = 'gear') %>% 
  left_join(
    filter(fishing_p1_df, flag_gfw == "MDV") %>% 
      distinct(vessel_class_gfw, mmsi) %>% 
      group_by(vessel_class_gfw) %>% 
      count() %>% 
      rename(gear = vessel_class_gfw), 
    by = 'gear'
  ) %>% 
  dplyr::select(gear, fishing_kw_hours, fishing_hours, n)

geardf <- mutate(geardf, gear = str_replace_all(gear, "_", " ") %>% 
                   str_to_sentence())

geardf$gear[geardf$gear == "Fishing"] <- "Other fishing"

#Arrange by fishing kw hours
geardf <- arrange(geardf, desc(fishing_kw_hours))

geardf <- bind_rows(geardf, 
                    data.frame(gear = "Total", fishing_hours = sum(flag_gear_year_df$fishing_hours[flag_gear_year_df$flag == "MDV"]),
                               fishing_kw_hours = sum(fishing_p1_df$fishing_kw_hours[fishing_p1_df$flag_gfw == "MDV"]),
                               n = unique(fishing_p1_df$mmsi[fishing_p1_df$flag_gfw == "MDV"]) %>% length()))


names(geardf) <- c("Gear", "Fishing-kW hours", "Fishing hours", "Fishing vessels")

geardf$`Fishing-kW hours` <- formNum(geardf$`Fishing-kW hours`, 0)
geardf$`Fishing hours` <- formNum(geardf$`Fishing hours`, 0)

(geartab <- flextable(geardf) %>% 
    theme_booktabs() %>%
    set_caption(caption = "Table 5: Apparent domestic fishing by gear") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 2, j = 1:4) %>% 
    autofit()
)

save_as_docx(geartab, path = 'output/tables/domestic_gear.docx')



