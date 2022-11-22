# 30DayMapChallenge | November 22, 2022 | NULL
# Shops with zero in the postal code
# The literary definition of null is zero and the insights from this map are zero
# Data Source is https://download.osmdata.xyz

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(sf)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load into R -------------------------------------------------------------
raw_shops <- read_sf("shop_EPSG4326.gpkg")

# code for robinson projection modified from ------------------------------
# https://michaelminn.net/tutorials/r-projections/index.html

# robinson projection -----------------------------------------------------
robinson <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

# shops with a postal code that contains a "0" ----------------------------
shops_df <- raw_shops %>% 
  filter(grepl("0", addr_postcode)) %>% 
  st_transform(shops_df, crs = robinson)

# create the robinson graticules ------------------------------------------
graticules <- st_graticule(lat = seq(-80, 80, 10), lon = seq(-180, 180, 10)) %>% 
  st_transform(graticules, crs = robinson)

# create plot -------------------------------------------------------------
ggplot() +
  geom_sf(data = graticules$geometry, size = 0.1, color = "#7B7B7C") +
  geom_sf(data = shops_df, size = 0.05, color = "#28282B") +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_markdown(family = font, size = 16, hjust = 0.5, color = "#28282B"),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#28282B"),
        plot.caption.position = "plot",
        plot.margin = margin(0, 0, 0, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "Shops with a <i><b>zero</b></i> in the postal code.<br>",
       caption = "\n\n#30DayMapChallenge | Data: OpenStreetMap: key=shop | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("zeros_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

