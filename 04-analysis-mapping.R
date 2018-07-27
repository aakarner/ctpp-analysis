library(tigris)
library(viridis)
library(sf)
library(ggplot2)

harris_tracts <- tracts("TX", county = "Harris County")

res_transit <- res_mode_minority %>%
  filter(lineno %in% c(5, 6)) %>%
  group_by(geoid10) %>%
  summarize(residence_trips = sum(est))

pow_transit <- pow_mode_minority %>%
  filter(lineno %in% c(5, 6)) %>%
  group_by(geoid10) %>%
  summarize(pow_trips = sum(est))

metro_lines <- 
  read_sf("C:/Users/alexk/Dropbox/Work/FTA connectivity/data/Metro_routes.shp")

# place of residence
harris_plot1 <- left_join(harris_tracts, res_transit, by = c("GEOID" = "geoid10"))
harris_plot1$residence_trips[is.na(harris_plot1$residence_trips)] <- 0 

harris_plot1$brks <- 
  cut(harris_plot1$residence_trips, 
      breaks=c(0, 15, 50, 110, 745), right = FALSE, include.lowest = TRUE,
      labels=c("0 - 15", "16 - 50", "51 - 110", "111 - 745"))

ggplot() + 
  geom_sf(data = harris_plot1, aes(fill = brks, col = brks)) + 
  scale_fill_viridis("origin trips", discrete = TRUE, na.value = "grey75") + 
  scale_color_viridis("origin trips", discrete = TRUE, na.value = "grey75") + 
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.background = element_blank(),
        legend.position = "bottom") 

+ 
  geom_sf(data = metro_lines, col = "grey50", alpha = 0.5)

ggsave("output/Harris_restrips.png", width = 4, height = 4)


# place of work
harris_plot2 <- left_join(harris_tracts, pow_transit, by = c("GEOID" = "geoid10"))
harris_plot2$pow_trips[is.na(harris_plot2$pow_trips)] <- 0 

harris_plot2$brks <- 
  cut(harris_plot1$residence_trips, 
      breaks=c(0, 20, 60, 500, 22115), right = FALSE, include.lowest = TRUE,
      labels=c("0 - 20", "21 - 60", "61 - 500", "500 - 22115"))

ggplot() + 
  geom_sf(data = harris_plot2, aes(fill = brks, col = brks)) + 
  scale_fill_viridis("pow trips", discrete = TRUE, na.value = "grey75") + 
  scale_color_viridis("pow trips", discrete = TRUE, na.value = "grey75") + 
  theme_bw() + 
  theme(panel.background = element_rect(fill = "white", color = "grey50"), 
        panel.grid.major = element_line(color = NA),
        axis.text = element_blank(), axis.ticks = element_blank(), 
        legend.background = element_blank(),
        legend.position = "bottom") 

+ 
  geom_sf(data = metro_lines, col = "grey50", alpha = 0.5)

ggsave("output/Harris_powtrips.png", width = 4, height = 4)
