# 30DayMapChallenge | November 15, 2022 | Food and Drink
# MLB Stadiums - Food and Drink
# Data source: NHDSC

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)
library(rnaturalearth)

# add font ----------------------------------------------------------------
font_add_google(name = "Roboto", family = "Roboto")
font <- "Roboto"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data stadium locatin data ------------------------------------------
dogs_raw <- readr::read_csv("stadium_dogs.csv")

# add CRS to points -------------------------------------------------------
points_sf = st_as_sf(dogs_raw, coords = c("x", "y"), crs="EPSG:4326")

# get country shape data  --------------------------------------
usa_sf <- ne_states(country = "United States of America", returnclass = "sf") %>% 
  filter(name %in% dogs_raw$State)

ca_sf <- ne_states(country = "Canada", returnclass = "sf") %>% 
  filter(name %in% dogs_raw$State)

# create plot -------------------------------------------------------------
ggplot() +
  geom_sf(data = usa_sf, color = "#708090", fill = "#FFFFFF") +
  geom_sf(data = ca_sf, color = "#708090", fill = "#FFFFFF") +
  geom_sf(data = points_sf, size = 3, alpha = 0.75) +
  annotate(geom = "richtext", y = 48.5, x = -107, label = "<span style='color:#FFFFFF;'>During the<br>2022 basesball season,<br>19.1 million hot dogs<br>and 5 million sausages<br>were sold at the<br><span style='color:#000000;'><b>thirty MLB stadiums</b></span><br>across the<br>United States and Canada.", hjust = "center", family = font, size = 5, fill = NA, label.color = NA, lineheight = 1.2) +
  coord_sf() +
  theme_void() +
    theme(plot.caption = element_markdown(family = font, hjust = 0.5, size = 10, color = "#FFFFFF"),
          plot.caption.position = "plot",
          plot.margin = margin(0, 0, 0.55, 0,"cm"),
          plot.background = element_rect(color = NA, fill = "#708090"),
          panel.background = element_rect(color = NA, fill = "#708090")) +
    labs(caption = "<br><span style='color:#041e42;'>#30DayMapChallenge</span> | Data: NHDSC | <span style='color:#bf0d3e;'>Design: Ryan Hart</span>")
  
# save plot ---------------------------------------------------------------
ggsave(paste0("mlb_stadiums_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)

