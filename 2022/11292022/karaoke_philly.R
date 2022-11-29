# 30DayMapChallenge | November 29, 2022 | "Out of my comfort zone"
# Karaoke
# Building shape data source is https://www.opendataphilly.org
# Best Karaoke Bars source  is https://philly.eater.com/maps/best-karaoke-bars-philadelphia

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(sf)
library(osmdata)
library(tidygeocoder)
library(MetBrewer)

# add font ----------------------------------------------------------------
font_add_google(name = "Bitter", family = "Bitter")
font_add_google(name = "Inter", family = "Inter")
font1 <- "Bitter"
font2 <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load shp file for buildings ---------------------------------------------
raw_buildings <- read_sf("LI_BUILDING_FOOTPRINTS.shp")

# set crs -----------------------------------------------------------------
buildings <- raw_buildings 
st_crs(buildings) <- 4326

# get open street boundary data  ------------------------------------------
philly_bbox <- st_bbox(c(xmin = -75.164, ymin = 39.951, xmax = -75.152, ymax = 39.958), crs = 4326)
philly_bb <- st_as_sfc(philly_bbox)

# major streets -----------------------------------------------------------
philly_big <- philly_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", 
                            "secondary", "tertiary", "trunk")) %>% 
  osmdata_sf()

# links --------------------------------------------------------------------
philly_links <- philly_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway_link", "primary_link", 
                            "secondary_link", "tertiary_link", "trunk_link")) %>% 
  osmdata_sf()

# minor streets -----------------------------------------------------------
philly_small <- philly_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "pedestrian", "road", "footway")) %>% 
  osmdata_sf()

# location of spots ----------------------------------------------------------
karaoke <- data.frame(name = c("K-Top", "Yakitori Boy", "Tango"),
                      address = c("911 Race Street, Philadelphia, PA 19107", "211 N 11th St, Philadelphia, PA 19107", "1021 Arch St, Philadelphia, PA 19107"))

# get geo codes for locations ---------------------------------------------
locations <- karaoke %>% 
  geocode(address, method = "osm",
                     lat = y, long = x)

points_sf = st_as_sf(locations, coords = c("x", "y"), crs="EPSG:4326")

# create plot -------------------------------------------------------------
ggplot() + 
  geom_sf(data = philly_big$osm_lines, inherit.aes = FALSE, color = "#F2F2F2", size = 0.75) +
  geom_sf(data = philly_links$osm_lines, inherit.aes = FALSE, color = "#F2F2F2", size = 0.5) +
  geom_sf(data = philly_small$osm_lines, inherit.aes = FALSE, color = "#F2F2F2", size = 0.25) +
  geom_sf(data = buildings, inherit.aes = FALSE, color = "#8B8B81") +
  geom_sf(data = points_sf, aes(color = name), size = 6, alpha = 0.8) +
  scale_color_manual(values = met.brewer("Klimt", 3)) +
  #coord_sf() +
  coord_sf(xlim = c(-75.164, -75.151), ylim = c(39.950, 39.958)) +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 28, hjust = 0.5, face = "bold", color = "#F2F2F2"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 10, hjust = 0.5, color = "#F2F2F2"),
        plot.caption = element_text(family = font2, hjust = 0.5, size = 8, color = "#F2F2F2"),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(family = font2, hjust = 0.5, size = 7, color = "#F2F2F2"),
        plot.margin = margin(0.5, 0.25, 0.5, 0.25,"cm"),
        plot.background = element_rect(color = NA, fill = "#28282B"),
        panel.background = element_rect(color = NA, fill = "#28282B")) +
  labs(title = "Karaoke",
       subtitle = "Hot Spots in the Chinatown Neighborhood of Philadelphia, PA.\n",
       caption = "\n\n\n#30DayMapChallenge | Data: philly.eater.com | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("karaoke_philly_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

