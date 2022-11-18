# 30DayMapChallenge | November 18, 2022 | Blue
# Pittsburgh - Three Rivers
# Data sources - OpenStreetMap

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(osmdata)
library(ggtext)
library(sf)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get open street data for capital city -----------------------------------
pitt_bb <- getbb("Pittsburgh")

# highway -----------------------------------------------------------------
highway <- pitt_bb %>%
  opq() %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

# water -------------------------------------------------------------------
water <- pitt_bb %>% 
  opq() %>% 
  add_osm_feature(key = "natural", value = "water") %>% 
  osmdata_sf()

water_polygon <- water$osm_multipolygons

# create plot -------------------------------------------------------------
ggplot() + 
  geom_sf(data = water_polygon, fill = "#3C73A8") +
  geom_sf(data = highway$osm_lines, inherit.aes = FALSE, color = "#D2D3D3", size = .25)  +
  annotate("text", x = -80.0325, y = 40.45, label = "Ohio River", family = font, size = 2, color = "#FFFFFF", fontface = "bold", angle = "-45") +
  annotate("text", x = -79.982, y = 40.4335, label = "Monongahela River", family = font, size = 2, color = "#FFFFFF", fontface = "bold", angle = "5") +
  annotate("text", x = -79.985, y = 40.456, label = "Allegheny River", family = font, size = 2, color = "#FFFFFF", fontface = "bold", angle = "45") +
  coord_sf(ylim = c(40.42, 40.46), xlim = c(-80.05, -79.95), expand = TRUE) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 28, hjust = 0.5, face = "bold", color = "#FFFFFF"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 16, hjust = 0.5, color = "#FFFFFF"),
        plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#FFFFFF"),
        plot.caption.position = "plot",
        plot.margin = margin(0, 0, 0, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#28282B"),
        panel.background = element_rect(color = NA, fill = "#28282B")) +
  labs(title = "THREE RIVERS",
       subtitle = "Pittsburgh, Pennsylvania\n",
       caption = "\n\n#30DayMapChallenge | OpenStreetMap | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("three_rivers_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)




