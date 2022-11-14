#Figure and tables summarizing foreign fishing in Maldivian waters
rm(list = ls())

pacman::p_load('dplyr', 'lubridate', 'ggplot2', 'sf', 
               'countrycode', 'tidyr', 'viridis', 'flextable')

myThemeStuff <- 
  theme(panel.background = element_rect(fill=NA),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(color = "black", size = 11, family="sans"),
        axis.title = element_text(color = "black", size = 12, family = "sans"),
        legend.text = element_text(color = "black", size = 11, family="sans"),
        legend.title = element_text(color = "black", size = 11, family="sans")
  )

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
  xlab("") + ylab("") + 
    ggtitle("Foreign fishing-kW hours, 2016-2020")
)

ggsave(foreignplot, filename = 'output/figures/foreign_fishingkwhours.png', 
       height = 4, width = 4, dpi = 900, units = 'in')
