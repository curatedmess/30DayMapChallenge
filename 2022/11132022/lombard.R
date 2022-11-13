# 30DayMapChallenge | November 13, 2022 | 5 Minute Map
# Lombard Street, San Francisco
# Data sources - OpenStreetMap

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(osmdata)
library(sf)

# add font ----------------------------------------------------------------
font_add_google(name = "Pacifico", family = "Pacifico")
font_add_google(name = "Inter", family = "Inter")
font1 <- "Pacifico"
font2 <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get open street data for city -------------------------------------------
sf_bb <- getbb("San Francisco")

# primary roads -------------------------------------------------------------
streets <- sf_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

# small streets -----------------------------------------------------------
small_streets <- sf_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()


# create circle buffer------------------------------------------------------
point <- data.frame(Lat = c(37.801944), Long = c(-122.418889))

point_sf <- point %>%
  st_as_sf(coords = c('Long', 'Lat')) %>%
  st_set_crs(4326) 

buffer <- st_buffer(point_sf, units::as_units(0.125, "miles"))

# create buffer per osm ---------------------------------------------------
circle_crop_streets <- st_intersection(buffer, streets$osm_lines)
circle_crop_small_streets <- st_intersection(buffer, small_streets$osm_lines)

# highlight for Lombard Street -------------------------------------------
highlight <- circle_crop_small_streets %>% 
  filter(name == "Lombard Street")

highlight2 <- circle_crop_streets %>% 
  filter(name == "Lombard Street")

# create plot -------------------------------------------------------------
ggplot() + 
  geom_sf(data = circle_crop_streets, inherit.aes = FALSE, color = "#FAFAFA", size = .55)  +
  geom_sf(data = circle_crop_small_streets, inherit.aes = FALSE, color = "#E0E0E0", size = .25) +
  geom_sf(data = highlight, color = "#7C4DFF", size = 1.1) +
  geom_sf(data = highlight2, color = "#7C4DFF", size = 1.1) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 22, hjust = 0.5, face = "bold", color = "#7C4DFF"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 10, hjust = 0.5, color = "#F2F2F2"),
        plot.caption = element_text(family = font2, hjust = 0.5, size = 8, color = "#F2F2F2"),
        plot.caption.position = "plot",
        plot.margin = margin(0.5, 0, 0.5, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#121212"),
        panel.background = element_rect(color = NA, fill = "#121212")) +
  labs(title = "Lombard Street",
       subtitle = "San Francisco, California",
       caption = "\n\n#30DayMapChallenge | OpenStreetMap | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("lombard_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)




