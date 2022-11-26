# 30DayMapChallenge | November 26, 2022 | Island(s)
# Java, Indonesia

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(sf)
library(osmdata)
library(elevatr)
library(raster)
library(scico)

# add font ----------------------------------------------------------------
font_add_google(name = "Inika", family = "Inika")
font_add_google(name = "Inter", family = "Inter")
font1 <- "Inika"
font2 <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get shape ---------------------------------------------------------------
# java boundary -----------------------------------------------------------
java_boundary <- getbb("Java", format_out = "sf_polygon")$multipolygon

# get the elevation data --------------------------------------------------
dem_area <- get_elev_raster(locations = java_boundary, z = 9, clip = "locations", neg_to_na = "TRUE")

# create a data frame to allow for plotting -------------------------------
df <- as.data.frame(dem_area, xy = TRUE) %>% 
  rename_with(.cols = 3, ~"elevation") %>% 
  drop_na(elevation)

# create plot -------------------------------------------------------------
ggplot() +
  geom_sf(data = java_boundary, size = 0.2, color = "#28282B", fill = NA) +
  geom_raster(data = df, aes(x = x, y = y, fill = elevation)) +
  geom_contour(data = df, aes(x = x, y = y, z = elevation), color = "#F2F2F2", size = 0.07) +
  annotate("text", x = 107, y = -8.25, label = "JAVA", hjust = "center", fontface = "bold", family = font1, size = 10, color = "#28282B") +
  annotate("text", x = 107, y = -8.65, label = "Indonesia", hjust = "center", family = font2, size = 5.4, color = "#28282B") +
  annotate("text", x = 107, y = -8.9, label = "Elevation in Meters", hjust = "center", family = font2, size = 2.6, color = "#28282B") +
  scale_fill_scico(palette = "devon") +
  coord_sf(ylim = c(-9.25, -5.25)) +
  theme_void() +
  theme(plot.caption = element_text(family = font2, hjust = 0.5, size = 6, color = "#28282B"),
        plot.caption.position = "plot",
        #legend.position = "none",
        legend.position = c(0.214, 0.05),
        legend.direction = "horizontal",
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.key = element_rect(color = "#28282B"),
        legend.title = element_blank(),
        legend.text = element_text(family = font2, hjust = 0.5, size = 5, color = "#28282B"),
        plot.margin = margin(0, 0, 0, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#DDDDDD"),
        panel.background = element_rect(color = NA, fill = "#DDDDDD")) +
  labs(caption = "\n\n\n#30DayMapChallenge | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("java_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 7, height = 4)
