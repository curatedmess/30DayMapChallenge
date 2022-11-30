# 30DayMapChallenge | November 12, 2022 | Scale
# Rivalry
# Carolina vs Duke

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)
library(sf)
library(osmdata)
library(geosphere)

# add font ----------------------------------------------------------------
font_add_google(name = "Ultra", family = "Ultra")
font_add_google(name = "Inter", family = "Inter")
font1 <- "Ultra"
font2 <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get open street boundary data  ------------------------------------------
bbox <- st_bbox(c(xmin = -79.09, ymin = 35.87, xmax = -78.89, ymax = 36.03), crs = 4326)
bb <- st_as_sfc(bbox)

# major streets -----------------------------------------------------------
rivalry_big <- bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", 
                            "secondary", "tertiary", "trunk")) %>% 
  osmdata_sf()

# links --------------------------------------------------------------------
rivalry_links <- bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("motorway_link", "primary_link", 
                            "secondary_link", "tertiary_link", "trunk_link")) %>% 
  osmdata_sf()

# minor streets -----------------------------------------------------------
rivalry_small <- bb %>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "pedestrian", "road", "footway")) %>% 
  osmdata_sf()

# buildings ---------------------------------------------------------------
rivalry_buildings <- bb %>%
  opq()%>%
  add_osm_feature(key = "building") %>% 
  osmdata_sf()

# location of arenas -------------------------------------------------------
arenas <- data.frame(name = c("Dean Smith Center", "Cameron Indoor Stadium"),
                      x = c("-79.043889", "-78.9422"),
                      y = c("35.899722", "35.9976"))

points_sf = st_as_sf(arenas, coords = c("x", "y"), crs="EPSG:4326")

# calculate distance ------------------------------------------------------
# found this code from stackoverflow, but now unable to find the url to
# reference in the code for attribution
points <- matrix(c("-79.043889", "-78.9422", "35.899722", "35.9976"), nrow =2)
colnames(points) <- c("x", "y")
rownames(points) <- c("Dean Smith Center", "Cameron Indoor Stadium")

dist <- distGeo(points)/1609.35
print(dist)

# create plot -------------------------------------------------------------
ggplot() + 
  geom_sf(data = rivalry_big$osm_lines, inherit.aes = FALSE, color = "#9899a6", size = 0.75) +
  geom_sf(data = rivalry_links$osm_lines, inherit.aes = FALSE, color = "#9899a6", size = 0.5) +
  geom_sf(data = rivalry_small$osm_lines, inherit.aes = FALSE, color = "#9899a6", size = 0.25) +
  geom_sf(data = rivalry_buildings$osm_polygons, inherit.aes = FALSE, fill = "#a7aaa4", color = "#a7aaa4", size = 0.1) +
  geom_sf(data = points_sf, aes(color = name), size = 8, alpha = 0.8) +
  annotate(geom = "segment", x = -79.043889, y = 35.899722, xend = -78.9422, yend = 35.9976, size = 1,  color = "#28282B") +
  geom_sf(data = points_sf, color = "#28282B", size = 2) +
  annotate(geom = "text", x = -78.99, y = 35.95, hjust = 0.5, vjust = -1, label = "8.83 miles", size = 5,  color = "#28282B", angle = 50, family = font2, fontface = "bold") +
  annotate(geom = "label", x = -79.037, y = 35.899722, hjust = 0, label = "Dean Smith Center", size = 2.5,  color = "#28282B", fill = "#7BAFD4", family = font1) +
  annotate(geom = "label", x = -78.95, y = 35.9976, hjust = 1, label = "Cameron Indoor Stadium", size = 2.5,  color = "#F2F2F2", fill = "#00539B", family = font1) +
  scale_color_manual(values = c("#00539B", "#7BAFD4")) +
  #coord_sf() +
  coord_sf(xlim = c(-79.06, -78.92), ylim = c(35.89, 36.01)) +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 22, hjust = 0.5, face = "bold", color = "#28282B"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(family = font2, size = 9, hjust = 0.5, color = "#28282B", lineheight = 1.2),
        plot.caption = element_text(family = font2, hjust = 0.5, size = 8, color = "#28282B"),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "RIVALRY",
       subtitle = "With courts seperated by less than 10 miles, the <b>scale</b> of the Carolina vs. Duke<br>men's college basketball rivalry is one of the sport's biggest. The score at the<br>end of the 2022 season is <span style='color:#7BAFD4;'><b>Carolina</b></span> with 143 wins and <span style='color:#00539B;'><b>Duke</b></span> with 115 wins.<br>",
       caption = "\n\n\n#30DayMapChallenge | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("rivalry_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

