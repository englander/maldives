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

#fishing_kw_hours by foreign vessels summed over years, flags, and gears
plotdf <- filter(fishing_p1_df, flag_gfw != "MDV") %>% 
  group_by(cell_ll_lat, cell_ll_lon) %>% 
  summarise(fishing_kw_hours = sum(fishing_kw_hours)) %>% 
  ungroup()

(foreignplot <- ggplot() + 
  geom_sf(data = eez, fill = NA) + 
    geom_sf(data = land, fill = 'grey60', col = 'grey60') + 
  geom_tile(data = plotdf, aes(y = cell_ll_lat, x = cell_ll_lon, fill = fishing_kw_hours),
             width = .1, height = .1) + 
  myThemeStuff + 
  scale_fill_viridis("Fishing-kW\nhours",trans='log', 
                      breaks = c(50, 1000, 20000), labels = c("50", "1,000", "20,000")) + 
  xlab("") + ylab("")
)

ggsave(foreignplot, filename = 'output/figures/foreign_fishingkwhours.png', 
       height = 4, width = 4, dpi = 900, units = 'in')


##Table: year, fishing-kw hours, fishing hours, fishing vessels
yeardf <- left_join(
  filter(flag_gear_year_df, flag != "MDV") %>% 
    group_by(year) %>% 
    summarise(fishing_hours = sum(fishing_hours)) %>% 
    ungroup(),
  filter(fishing_p1_df, flag_gfw != "MDV") %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(fishing_kw_hours = sum(fishing_kw_hours)) %>% 
  ungroup(), 
  by = 'year') %>% 
  left_join(
    filter(fishing_p1_df, flag_gfw != "MDV") %>% 
      mutate(year = year(date)) %>% 
      distinct(year, mmsi) %>% 
      group_by(year) %>% 
      count(), 
    by = 'year'
  ) %>% 
  dplyr::select(year, fishing_kw_hours, fishing_hours, n) %>% 
  mutate(year = as.character(year))

yeardf <- bind_rows(yeardf, 
                    data.frame(year = "Total", fishing_hours = sum(flag_gear_year_df$fishing_hours[flag_gear_year_df$flag != "MDV"]),
                               fishing_kw_hours = sum(fishing_p1_df$fishing_kw_hours[fishing_p1_df$flag_gfw != "MDV"]),
                               n = unique(fishing_p1_df$mmsi[fishing_p1_df$flag_gfw != "MDV"]) %>% length()))


  
names(yeardf) <- c("Year", "Fishing-kW hours", "Fishing hours", "Fishing vessels")

yeardf[is.na(yeardf)] <- 0

yeardf$`Fishing-kW hours` <- formNum(yeardf$`Fishing-kW hours`, 0)
yeardf$`Fishing hours` <- formNum(yeardf$`Fishing hours`, 0)

(yeartab <- flextable(yeardf) %>% 
    theme_booktabs() %>%
  set_caption(caption = "Table 1: Apparent unauthorized foreign fishing by year") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 5, j = 1:4) %>%
    add_footer_row(values = "Note: In 2016, fishing-kW hours and fishing vessels are zero while fishing hours is not because the first two variables come from data with a .1 degree resolution while fishing hours data have a resolution of .01 degrees.",
                   colwidths = 4) %>%
    autofit()
)

save_as_docx(yeartab, path = 'output/tables/foreign_year.docx')



## Table by gear
geardf <- left_join(
  filter(flag_gear_year_df, flag != "MDV") %>% 
    group_by(gear) %>% 
    summarise(fishing_hours = sum(fishing_hours)) %>% 
    ungroup() %>% 
    arrange(desc(fishing_hours)),
  filter(fishing_p1_df, flag_gfw != "MDV") %>% 
    group_by(vessel_class_gfw) %>% 
    summarise(fishing_kw_hours = sum(fishing_kw_hours)) %>% 
    ungroup() %>% 
    rename(gear = vessel_class_gfw), 
  by = 'gear') %>% 
  left_join(
    filter(fishing_p1_df, flag_gfw != "MDV") %>% 
      distinct(vessel_class_gfw, mmsi) %>% 
      group_by(vessel_class_gfw) %>% 
      count() %>% 
      rename(gear = vessel_class_gfw), 
    by = 'gear'
  ) %>% 
  dplyr::select(gear, fishing_kw_hours, fishing_hours, n)

#pole and line fishing hours = 0, but not fishing kw hours and n
#probably due to difference in resolution. Since fishing hours higher res, set 
#fishing kw hours and n = 0 (that fishing probably occurred outside EEZ)
geardf$fishing_kw_hours[geardf$gear == 'pole_and_line'] <- 0
geardf$n[geardf$gear == 'pole_and_line'] <- 0

geardf <- mutate(geardf, gear = str_replace_all(gear, "_", " ") %>% 
                   str_to_sentence())

geardf$gear[geardf$gear == "Fishing"] <- "Other fishing"

#Arrange by fishing kw hours
geardf <- arrange(geardf, desc(fishing_kw_hours))

geardf <- bind_rows(geardf, 
                    data.frame(gear = "Total", fishing_hours = sum(flag_gear_year_df$fishing_hours[flag_gear_year_df$flag != "MDV"]),
                               fishing_kw_hours = sum(fishing_p1_df$fishing_kw_hours[fishing_p1_df$flag_gfw != "MDV"]),
                               n = unique(fishing_p1_df$mmsi[fishing_p1_df$flag_gfw != "MDV"]) %>% length()))



names(geardf) <- c("Gear", "Fishing-kW hours", "Fishing hours", "Fishing vessels")

geardf[is.na(geardf)] <- 0

geardf$`Fishing-kW hours` <- formNum(geardf$`Fishing-kW hours`, 0)
geardf$`Fishing hours` <- formNum(geardf$`Fishing hours`, 0)

(geartab <- flextable(geardf) %>% 
    theme_booktabs() %>%
    set_caption(caption = "Table 2: Apparent unauthorized foreign fishing by gear") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 11, j = 1:4) %>% 
    add_footer_row(values = "Notes: In the tuna purse seine row, fishing-kW hours and fishing vessels are zero while fishing hours is not because the first two variables come from data with a .1 degree resolution while fishing hours data have a resolution of .01 degrees. Other fishing refers to instances when Global Fishing Watch predicts a vessel is a fishing vessel, but cannot predict a vessel's specific fishing method (gear).",
                   colwidths = 4) %>%
    autofit()
)

save_as_docx(geartab, path = 'output/tables/foreign_gear.docx')



##Table by flag
flagdf <- left_join(
  filter(flag_gear_year_df, flag != "MDV") %>% 
    group_by(flag) %>% 
    summarise(fishing_hours = sum(fishing_hours)) %>% 
    ungroup() %>% 
    arrange(desc(fishing_hours)),
  filter(fishing_p1_df, flag_gfw != "MDV") %>% 
    group_by(flag_gfw) %>% 
    summarise(fishing_kw_hours = sum(fishing_kw_hours)) %>% 
    ungroup() %>% 
    rename(flag = flag_gfw), 
  by = 'flag') %>% 
  left_join(
    filter(fishing_p1_df, flag_gfw != "MDV") %>% 
      distinct(flag_gfw, mmsi) %>% 
      group_by(flag_gfw) %>% 
      count() %>% 
      rename(flag = flag_gfw), 
    by = 'flag'
  ) %>% 
  dplyr::select(flag, fishing_kw_hours, fishing_hours, n) %>% 
  filter(fishing_hours != 0)

#.268 hours of Spanish fishing likely just from one vessel; that vessel must have been missing
#identifying information so it was excluded from .1 degree level data
#Since missing identifying information, and fishing hours also zero for Spain, 
#drop Spain from table
flagdf <- filter(flagdf, flag != "ESP")

#Arrange by fishing kw hours
flagdf <- arrange(flagdf, desc(fishing_kw_hours))

#Replace iso3 code with countryname
flagdf$flag <- countrycode(flagdf$flag, origin = 'iso3c', destination = 'country.name')

flagdf <- bind_rows(flagdf, 
                    data.frame(flag = "Total", fishing_hours = sum(flag_gear_year_df$fishing_hours[flag_gear_year_df$flag != "MDV"]),
                               fishing_kw_hours = sum(fishing_p1_df$fishing_kw_hours[fishing_p1_df$flag_gfw != "MDV"]),
                               n = unique(fishing_p1_df$mmsi[fishing_p1_df$flag_gfw != "MDV" & fishing_p1_df$fishing_kw_hours > 0]) %>% length()))

names(flagdf) <- c("Flag", "Fishing-kW hours", "Fishing hours", "Fishing vessels")

flagdf$`Fishing-kW hours` <- formNum(flagdf$`Fishing-kW hours`, 0)
flagdf$`Fishing hours` <- formNum(flagdf$`Fishing hours`, 0)

(flagtab <- flextable(flagdf) %>% 
    theme_booktabs() %>%
    set_caption(caption = "Table 3: Apparent unauthorized foreign fishing by flag") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 5, j = 1:4) %>% 
    autofit()
)

save_as_docx(flagtab, path = 'output/tables/foreign_flag.docx')
