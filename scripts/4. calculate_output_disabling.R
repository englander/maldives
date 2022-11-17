#Count number of disabling events and gap hours within 25 km of Maldives EEZ
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

togdisab$type <- as.factor(togdisab$type)
togdisab$type <- relevel(togdisab$type, ref = "Start")

(disabplot <- ggplot() + 
    geom_sf(data = eez, fill = NA) + 
    geom_sf(data = land, fill = 'grey60', col = 'grey60') + 
  geom_sf(data = togdisab, aes(col = type), alpha = 0.35) + 
  scale_color_manual("Event", values = c("dodgerblue2", "darkorange1")) + 
  myThemeStuff)

ggsave(disabplot, filename = 'output/figures/disabling_events.png', 
       height = 4, width = 4, dpi = 900, units = 'in')



#Created in 2. calculate_fishing.R
load('output/data/flag_gear_year_df.Rdata')

disab <- as.data.frame(disab) %>% dplyr::select(-`.`) %>% as_tibble()

disab <- mutate(disab, year = year(gap_start_timestamp))

##Table: year, disabling events, gap hours, fraction of time lost
yeardf <- left_join(
  group_by(disab, year) %>% 
    summarise(disabling_events = n(), 
              gap_hours = sum(gap_hours)) %>% 
    ungroup(),
  flag_gear_year_df %>% 
    group_by(year) %>% 
    summarise(ais_hours = sum(ais_hours)) %>% ungroup(),
  by = 'year'
) %>% 
  mutate(year = as.character(year))


#Add Total row
yeardf <- bind_rows(
  yeardf,
  data.frame(year = "Total", disabling_events = sum(yeardf$disabling_events), 
             gap_hours = sum(yeardf$gap_hours), 
             ais_hours = sum(yeardf$ais_hours)
  )
)%>% 
  mutate(fraction_hours_lost = gap_hours / ais_hours) %>% 
  dplyr::select(year, disabling_events, gap_hours, fraction_hours_lost) %>% 
  #convert fraction to percent
  mutate(fraction_hours_lost = fraction_hours_lost * 100)

names(yeardf) <- c("Year", "Disabling events", "Gap hours", "Hours lost")

yeardf$`Gap hours` <- formNum(yeardf$`Gap hours`, 0)
yeardf$`Hours lost` <- paste0(formNum(yeardf$`Hours lost`, 1), "%")

(yeartab <- flextable(yeardf) %>% 
    theme_booktabs() %>%
    set_caption(caption = "Table 6: Suspected disabling events by year") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 3, j = 1:4) %>% 
    add_footer_row(values = "Notes: This table includes suspected disabling events that begin within 25 km of the Maldives' Exclusive Economic Zone boundary. Gap hours refers to the time elapsed between the beginning and end of suspected disabling events. Hours lost is gap hours relative to the time vessels spend with their transponders on.",
                   colwidths = 4) %>%
    autofit()
)

save_as_docx(yeartab, path = 'output/tables/disabling_year.docx')


##Table: gear, disabling events, gap hours, fraction of time lost
geardf <- left_join(
  flag_gear_year_df %>% 
    filter(year >= 2017 & year <= 2019) %>% 
    group_by(gear) %>% 
    summarise(ais_hours = sum(ais_hours)) %>% ungroup(),
  group_by(disab, vessel_class) %>% 
    summarise(disabling_events = n(), 
              gap_hours = sum(gap_hours)) %>% 
    ungroup() %>% 
    rename(gear = vessel_class),
  by = 'gear'
) %>% 
  mutate(gear = as.character(gear)) %>% 
  arrange(desc(disabling_events))
  
geardf[is.na(geardf)] <- 0

#Add Total row
geardf <- bind_rows(
  geardf,
  data.frame(gear = "Total", disabling_events = sum(geardf$disabling_events, na.rm = TRUE), 
             gap_hours = sum(geardf$gap_hours, na.rm = TRUE), 
             ais_hours = sum(geardf$ais_hours, na.rm = TRUE)
  )
)%>% 
  mutate(fraction_hours_lost = gap_hours / ais_hours) %>% 
  dplyr::select(gear, disabling_events, gap_hours, fraction_hours_lost) %>% 
  #convert fraction to percent
  mutate(fraction_hours_lost = fraction_hours_lost * 100)

geardf <- mutate(geardf, gear = str_replace_all(gear, "_", " ") %>% 
                   str_to_sentence())

names(geardf) <- c("Gear", "Disabling events", "Gap hours", "Hours lost")

geardf$`Gap hours` <- formNum(geardf$`Gap hours`, 0)
geardf$`Hours lost`[!is.nan(geardf$`Hours lost`)] <- paste0(formNum(geardf$`Hours lost`[!is.nan(geardf$`Hours lost`)], 1), "%")

geardf$`Hours lost`[geardf$`Hours lost` == "NaN"] <- ""

geardf$Gear[geardf$Gear == "Fishing"] <- "Other fishing"

(geartab <- flextable(geardf) %>% 
    theme_booktabs() %>%
    set_caption(caption = "Table 7: Suspected disabling events by gear") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 10, j = 1:4) %>% 
    add_footer_row(values = "Notes: This table includes suspected disabling events that begin between 2017 and 2019 within 25 km of the Maldives' Exclusive Economic Zone (EEZ) boundary. Gap hours refers to the time elapsed between the beginning and end of suspected disabling events. Hours lost is gap hours relative to the time vessels spend with their transponders on. Other fishing refers to instances when Global Fishing Watch predicts a vessel is a fishing vessel, but cannot predict a vessel's specific fishing method (gear). Hours lost is blank for the pole and line and pots and traps rows because there are no vessels observed with this gear inside the Maldives' EEZ in Global Fishing Watch data.",
                   colwidths = 4) %>%
    autofit()
)

save_as_docx(geartab, path = 'output/tables/disabling_gear.docx')


##Table: flag, disabling events, gap hours, fraction of time lost
flagdf <- left_join(
  group_by(disab, flag) %>% 
    summarise(disabling_events = n(), 
              gap_hours = sum(gap_hours)) %>% 
    ungroup(),
  flag_gear_year_df %>% 
    filter(year >= 2017 & year <= 2019) %>% 
    group_by(flag) %>% 
    summarise(ais_hours = sum(ais_hours)) %>% ungroup(),
  by = 'flag'
) %>% 
  mutate(flag = as.character(flag)) %>% 
  arrange(desc(disabling_events))

#Replace iso3 code with countryname
flagdf$flag <- countrycode(flagdf$flag, origin = 'iso3c', destination = 'country.name')

#Add Total row
flagdf <- bind_rows(
  flagdf,
  data.frame(flag = "Total", disabling_events = sum(flagdf$disabling_events, na.rm = TRUE), 
             gap_hours = sum(flagdf$gap_hours, na.rm = TRUE), 
             ais_hours = sum(flag_gear_year_df$ais_hours[flag_gear_year_df$year >= 2017 & flag_gear_year_df$year <= 2019])
  )
)%>% 
  mutate(fraction_hours_lost = gap_hours / ais_hours) %>% 
  dplyr::select(flag, disabling_events, gap_hours, fraction_hours_lost) %>% 
  #convert fraction to percent
  mutate(fraction_hours_lost = fraction_hours_lost * 100)

flagdf <- mutate(flagdf, flag = str_replace_all(flag, "_", " ") %>% 
                   str_to_sentence())

names(flagdf) <- c("Flag", "Disabling events", "Gap hours", "Hours lost")

flagdf$`Gap hours` <- formNum(flagdf$`Gap hours`, 0)
flagdf$`Hours lost`[!is.na(flagdf$`Hours lost`)] <- sapply(flagdf$`Hours lost`[!is.na(flagdf$`Hours lost`)], function(x){
  formNum(x, 1) %>% 
    paste0("%")
})
  

flagdf <- mutate(flagdf, Flag = str_to_title(Flag))

flagdf$Flag[is.na(flagdf$Flag)] <- "Missing"

(flagtab <- flextable(flagdf) %>% 
    theme_booktabs() %>%
    set_caption(caption = "Table 8: Suspected disabling events by flag") %>% 
    align(align = "center", part = "all") %>% 
    flextable::hline(i = 6, j = 1:4) %>% 
    add_footer_row(values = "Notes: This table includes suspected disabling events that begin between 2017 and 2019 within 25 km of the Maldives' Exclusive Economic Zone boundary. Gap hours refers to the time elapsed between the beginning and end of suspected disabling events. Hours lost is gap hours relative to the time vessels spend with their transponders on. Hours lost is blank for the row in which flag is missing because Global Fishing Watch removes these observations from the data this report uses to calculate time vessels spend with their transponders on.",
                   colwidths = 4) %>%
    autofit()
)

save_as_docx(flagtab, path = 'output/tables/disabling_flag.docx')


