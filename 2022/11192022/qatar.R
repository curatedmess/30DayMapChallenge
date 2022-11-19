# 30DayMapChallenge | November 19, 2022 | Globe
# Qatar

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(sf)
library(rnaturalearth)
library(ggtext)
library(ggfx)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get world shape data ---------------------------------------------------
world <- ne_countries(scale = "small", returnclass = "sf")

# thanks to https://github.com/gkaramanis ---------------------------------
# for the cheat code on creating a globe ----------------------------------

# ocean detail ------------------------------------------------------------
ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lat_0=20 +lon_0=55")

# create plot -------------------------------------------------------------
ggplot() +
  with_shadow(geom_sf(data = ocean, fill = "#BBDEFB", color = NA), sigma = 30, x_offset = 25, y_offset = 25, color = "#58595d") +
  geom_sf(data = world, aes(fill = ifelse(name == "Qatar", "yes", "no")), color = "#BDBDBD", size = 0.25) +
  scale_fill_manual(values = c("#E0E0E0", "#8a1538")) +
  coord_sf(crs = "+proj=ortho +lat_0=20 +lon_0=55") +
  theme_void() +
  theme(plot.caption = element_markdown(family = font, hjust = 0.5, size = 11),
        plot.caption.position = "plot",
        legend.position = "Null",
        plot.margin = margin(0, 0, 0.5, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(caption = "<br>#30DayMapChallenge | <span style='color:#8a1538;'><b>Qatar</b></span> | Design: Ryan Hart")  

# save plot ---------------------------------------------------------------
ggsave(paste0("qatar_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 8)

 