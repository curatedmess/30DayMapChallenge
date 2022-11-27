# 30DayMapChallenge | November 27, 2022 | Music
# Hollywood Bowl
# Building shape data source is https://geohub.lacity.org

# code is slow and could probably be cleaned up with more time

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(sf)
library(osmdata)

# add font ----------------------------------------------------------------
font_add_google(name = "Corben", family = "Corben")
font_add_google(name = "Inter", family = "Inter")
font1 <- "Corben"
font2 <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load shp file for buildings ---------------------------------------------
raw_buildings <- read_sf("Building_Footprints.shp")

# set crs -----------------------------------------------------------------
buildings <- raw_buildings 
st_crs(buildings) <- 4326

# get open street boundary data  ------------------------------------------
la_bbox <- st_bbox(c(xmin = -118.342, ymin = 34.109, xmax = -118.3325, ymax = 34.115), crs = 4326)
la_bb <- st_as_sfc(la_bbox)

# major streets -----------------------------------------------------------
la_big <- la_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", 
                            "secondary", "tertiary", "trunk")) %>% 
  osmdata_sf()

# links --------------------------------------------------------------------
la_links <- la_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway_link", "primary_link", 
                            "secondary_link", "tertiary_link", "trunk_link")) %>% 
  osmdata_sf()

# minor streets -----------------------------------------------------------
la_small <- la_bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "pedestrian", "road", "footway")) %>% 
  osmdata_sf()

la_nature <- la_bb %>%
  opq()%>%
  add_osm_feature(key = "natural") %>% 
  osmdata_sf()

# location of bowl ----------------------------------------------------------
point <- data.frame(y = c(34.112778), x = c(-118.338889))

point_sf <- point %>%
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(4326)

# create plot -------------------------------------------------------------
ggplot() + 
  geom_sf(data = la_big$osm_lines, inherit.aes = FALSE, color = "#8B8B81", size = 1.25) +
  geom_sf(data = la_links$osm_lines, inherit.aes = FALSE, color = "#8B8B81", size = 0.75) +
  geom_sf(data = la_small$osm_lines, inherit.aes = FALSE, color = "#8B8B81", size = 0.25) +
  geom_sf(data = la_nature$osm_polygons, inherit.aes = FALSE, color = "#8B8B81", size = 0.25) +
  geom_sf(data = buildings, inherit.aes = FALSE, color = "#8B8B81") +
  geom_sf(data = point_sf, color = "#28282B", size = 0.5) +
  annotate(geom = "text", y = 34.1148, x = -118.338889, label = "2301 N Highland Ave\nLos Angeles, CA 90068", hjust = "center", family = font2, size = 3, color = "#28282B") +
  annotate(geom = "segment", x = -118.338889, y = 34.1144, xend = -118.338889, yend = 34.112778, size = 0.5,  color = "#28282B") +
  #coord_sf() +
  coord_sf(xlim = c(-118.3325, -118.342), ylim = c(34.109, 34.115)) +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 28, hjust = 0.5, face = "bold", color = "#28282B"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 10, hjust = 0.5, color = "#28282B"),
        plot.caption = element_text(family = font2, hjust = 0.5, size = 8, color = "#28282B"),
        plot.caption.position = "plot",
        plot.margin = margin(1, 1, 1, 1,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "Hollywood Bowl",
       subtitle = "Live music in Southern California since 1922\n",
       caption = "\n\n\n#30DayMapChallenge | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("hollywood_bowl_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

