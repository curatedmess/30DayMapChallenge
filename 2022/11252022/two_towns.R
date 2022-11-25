# 30DayMapChallenge | November 25, 2022 | 2 Colors
# Two River Towns

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(osmdata)
library(raster)
library(sf)
library(patchwork)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

rr_bb <- getbb("Red River, New Mexico")


# TOWN OF BLUE RIVER,  COLORADO -------------------------------------------

# get open street boundary data  ------------------------------------------
sc_bb <- getbb("Summit County, Colorado")

# streets -----------------------------------------------------------------
sc_streets <- sc_bb %>%
  opq()%>%
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

# create circle buffer for ------------------------------------------------
br_point <- data.frame(x = c(-106.036774), y = c(39.448494))

br_point_sf <- br_point %>%
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(4326) 

sc_buffer <- st_buffer(br_point_sf, units::as_units(4, "miles"))

# create buffer ------------------------------------------------------------
sc_circle <- st_intersection(sc_buffer, sc_streets$osm_lines)

# TOWN OF RED RIVER,  NEW MEXICO -------------------------------------------

# get open street boundary data  ------------------------------------------
tc_bb <- getbb("Taos County, New Mexico")

# streets -----------------------------------------------------------------
tc_streets <- tc_bb %>%
  opq()%>%
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

# create circle buffer for ------------------------------------------------
rr_point <- data.frame(x = c(-105.405278), y = c(36.706389))

rr_point_sf <- rr_point %>%
  st_as_sf(coords = c('x', 'y')) %>%
  st_set_crs(4326) 

rr_buffer <- st_buffer(rr_point_sf, units::as_units(4, "miles"))

# create buffer ------------------------------------------------------------
tc_circle <- st_intersection(rr_buffer, tc_streets$osm_lines)


# get town shapes ----------------------------------------------------------
rr_bb_sf <- getbb("Red River, New Mexico", format_out = "sf_polygon")
br_bb_sf <- getbb("Blue River, Colorado", format_out = "sf_polygon")

# create rr plot -----------------------------------------------------------
rr <- ggplot() + 
  geom_sf(data = rr_bb_sf, fill = "#ff6b6b", color = NA) +
  geom_sf(data = tc_circle, inherit.aes = FALSE, color = "#F2F2F2", size = 0.25) +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 20, hjust = 0.5, face = "bold", color = "#ff6b6b"),
    plot.title.position = "plot",
    plot.subtitle = element_text(family = font, size = 14, hjust = 0.5, color = "#F2F2F2")) +
labs(title = "Town of Red River",
     subtitle = "Taos County, New Mexico\n")

# create br plot -----------------------------------------------------------
br <- ggplot() + 
  geom_sf(data = br_bb_sf$multipolygon, fill = "#54a0ff", color = NA) +
  geom_sf(data = sc_circle, inherit.aes = FALSE, color = "#F2F2F2", size = 0.25) +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 20, hjust = 0.5, face = "bold", color = "#54a0ff"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 14, hjust = 0.5, color = "#F2F2F2")) +
  labs(title = "Town of Blue River",
       subtitle = "Summit County, Colorado\n")

# combine plots -----------------------------------------------------------
final <- br | plot_spacer() | rr

final + plot_layout(widths =c(5, 0.5, 5)) + plot_annotation(caption = "\n#30DayMapChallenge | Design: Ryan Hart") &
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5,"cm"),
        plot.background = element_rect(color = NA, fill = "#28282B"),
        panel.background = element_rect(color = NA, fill = "#28282B"),
        plot.caption = element_text(size = 10, hjust = 0.5, family = font, color = "#F2F2F2")) 

# save plot ---------------------------------------------------------------
ggsave(paste0("two_towns_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 6)

