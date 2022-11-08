# 30DayMapChallenge | November 8, 2022 | OpenStreetMap
# Spaghetti Junction, Atlanta Georgia
# Data sources - OpenStreetMap

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(osmdata)
library(sf)

# add font ----------------------------------------------------------------
font_add_google(name = "Work Sans", family = "Work Sans")
font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"
font <- "Work Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get open street data for capital city -----------------------------------
atl_bb <- getbb("Atlanta")

# primary roads -------------------------------------------------------------
streets <- atl_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

# primary links -------------------------------------------------------------
links <- atl_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("primary_link", "motorway_link")) %>%
  osmdata_sf()

# medium streets ----------------------------------------------------------
med_streets <- atl_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

# small streets -----------------------------------------------------------
small_streets <- atl_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

# create circle buffer------------------------------------------------------
point <- data.frame(Long = c(-84.259164), Lat = c(33.891744))

point_sf <- point %>%
  st_as_sf(coords = c('Long', 'Lat')) %>%
  st_set_crs(4326) 

buffer <- st_buffer(point_sf, units::as_units(0.75, "miles"))

# create buffer per osm ---------------------------------------------------
circle_crop_streets <- st_intersection(buffer, streets$osm_lines)
circle_crop_links <- st_intersection(buffer, links$osm_lines)
circle_crop_med_streets <- st_intersection(buffer, med_streets$osm_lines)
circle_crop_small_streets <- st_intersection(buffer, small_streets$osm_lines)

# create plot -------------------------------------------------------------
ggplot() + 
  geom_sf(data = circle_crop_streets, inherit.aes = FALSE, color = "#121212", size = 1.25)  +
  geom_sf(data = circle_crop_links, inherit.aes = FALSE, color = "#121212", size = .75)  +
  geom_sf(data = circle_crop_med_streets, inherit.aes = FALSE, color = "#121212", size = .5) +
  geom_sf(data = circle_crop_small_streets, inherit.aes = FALSE, color = "#121212", size = .25) +
  annotate(geom = "text", y = 33.902, x = -84.285, label = "TOM MORELAND INTERCHANGE", hjust = "left", family = font, fontface = "bold", size = 5.85, color = "#000000") +
  annotate(geom = "text", y = 33.9005, x = -84.285, label = '"Spaghetti Junction"', hjust = "left", family = font, size = 7, color = "#000000") +
  annotate(geom = "text", y = 33.8994, x = -84.285, label = "According to Wikipedia, the interchange\nin Atlanta, Georgia supports approx.\n300,000 cars per day. The map\ndiameter is 3/4 mile.", hjust = "left", vjust = "top", family = font, size = 3.25, color = "#000000") +
  #geom_point(aes(y = 33.891744, x = -84.259164)) +
  coord_sf(xlim = c(-84.285, NA), clip = "off") +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 8),
        plot.caption.position = "plot",
        plot.margin = margin(0.25, 0, 0.25, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(caption = "\n\n#30DayMapChallenge | OpenStreetMap | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("spaghetti_junction_atl_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)




