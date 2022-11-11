# 30DayMapChallenge | November 11, 2022 | Red Friday
# Washington DC - Firestations
# Data source is DC Open Data & OpenStreetMap

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(osmdata)

# add font ----------------------------------------------------------------
font_add_google(name = "Ultra", family = "Ultra")
font_add_google(name = "Roboto", family = "Roboto")
font <- "Ultra"
font2 <- "Roboto"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load shapefiles ---------------------------------------------------------
dc_raw <- readOGR("./dc_data", layer = "Fire_Stations")


# need to come back and fix this...it works for now,  but it's an  --------
proj4string(dc_raw) <- CRS("+proj=longlat +datum=WGS84")

# create data frame from shape files for plotting -------------------------
df <- data.frame(dc_raw)

# create bb ---------------------------------------------------------------
dc_bb <- getbb("Washington DC")

# create boundary shape ---------------------------------------------------
boundary <- dc_bb %>%
  opq() %>%
  add_osm_feature(key = "admin_level", 
                  value = c("6")) %>%
  osmdata_sf() %$% 
  osm_multipolygons %>% 
  filter(name == "Washington") 

# streets -----------------------------------------------------------------
streets <- dc_bb %>%
  opq()%>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# water -------------------------------------------------------------------
water <- dc_bb %>% 
  opq() %>% 
  add_osm_feature(key = "natural",
                  value = "water") %>% 
  osmdata_sf()

water_polygon <- water$osm_multipolygons

# create buffer per osm ---------------------------------------------------
crop_streets <- st_intersection(boundary, streets$osm_lines)
crop_water <- st_intersection(boundary, water$osm_multipolygons)

# create plot --------------------------------------------------------------
  ggplot() +
  geom_sf(data = crop_water, fill = "#91a7bc") +
  geom_sf(data = crop_streets, inherit.aes = FALSE, color = "#8B8B81", size = .25)  +
  geom_point(data = df, aes(x = coords.x1, y = coords.x2), color = "red") +
  annotate(geom = "text", y = 38.86, x = -77.14, label = "FIRE\nSTATIONS", hjust = "left", family = font, size = 10, color = "red", lineheight = 0.75) +
  annotate(geom = "text", y = 38.845, x = -77.14, label = "Washington, D.C.", hjust = "left", family = font2, size = 5.5, color = "#000000") +
  coord_sf() +
  theme_void() +
  theme(plot.caption = element_text(family = font2, hjust = 0.5, size = 10),
        plot.caption.position = "plot",
        legend.position = "Null",
        plot.margin = margin(0, 0, 0.5, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(
       caption = "\n#30DayMapChallenge | Data: Open Data DC & OpenStreetMap | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("fire_stations_dc_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 8)





