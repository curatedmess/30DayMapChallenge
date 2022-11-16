# 30DayMapChallenge | November 16, 2022 | Minimal
# California Highways
# Data sources - OpenStreetMap and Wikipedia

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(osmdata)
library(sf)

# add font ----------------------------------------------------------------
font_add_google(name = "Work Sans", family = "Work Sans")
font_add_google(name = "Tourney", family = "Tourney")
font2 <- "Work Sans"
font1 <- "Tourney"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get open street data for capital city -----------------------------------
ca_bb <- getbb("California")

ca_boundary <- getbb("California", format_out = "sf_polygon")$multipolygon

# road network -------------------------------------------------------------
roads <- ca_bb %>%
  opq(timeout = 120) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk", "primary")) %>%
  osmdata_sf()

ca_roads_sf <- st_intersection(ca_boundary, roads$osm_lines)

# link network -------------------------------------------------------------
links <- ca_bb %>%
  opq(timeout = 120) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway_link", "trunk_link", "primary_link")) %>%
  osmdata_sf()

ca_links_sf <- st_intersection(ca_boundary, links$osm_lines)

# create plot -------------------------------------------------------------
ggplot() + 
  geom_sf(data = ca_boundary, color = "#28282B", fill = "#28282B") +
  geom_sf(data = ca_roads_sf, color = "#F2F2F2", size = .1) +
  geom_sf(data = ca_links_sf, color = "#F2F2F2", size = .1) +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 30, hjust = 0.5, face = "bold", color = "#28282B"),
        plot.title.position = "plot",
        plot.caption = element_markdown(family = font2, hjust = 0.5, size = 9, color = "#28282B"),
        plot.caption.position = "plot",
        plot.margin = margin(0.5, 0, 0.5, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "California Highways",
       caption = "\n\n#30DayMapChallenge | OpenStreetMap | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("ca_highways_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 5, height = 7)




