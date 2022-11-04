# 30DayMapChallenge | November 4, 2022 | Green
# Vermont - The Green Mountain State

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(sf)
library(rnaturalearth)
library(elevatr)
library(ggridges)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get country shape data for Vermont -------------------------------------
sf_usa <- ne_states(country = "United States of America", returnclass = "sf") 

sf_vermont <- sf_usa %>% 
  filter(name == "Vermont")

# get the elevation data for vermont --------------------------------------
dem_vermont <- get_elev_raster(locations = sf_vermont, z = 11, clip = "locations", neg_to_na = "TRUE")

# reduced the number of rows ----------------------------------------------
reduced <- aggregate(dem_vermont, fact = 30)

# create a data frame to allow for plotting -------------------------------
df <- as.data.frame(reduced, xy = TRUE) %>% 
  rename_with(.cols = 3, ~"elevation") %>% 
  drop_na(elevation) 

# create plot -------------------------------------------------------------
ggplot() + 
  geom_density_ridges(data = df, aes(x = x, y = y, group = y, height = elevation), stat = "identity", size = 0.1, scale = 20, color = "#F2F2F2", fill = NA) +
  annotate(geom = "text", y = 43.05, x = -72.25, label = "Vermont", hjust = "left", family = font, fontface = "bold", size = 12, color = "#F2F2F2") +
  annotate(geom = "text", y = 43.35, x = -72.25, label = "The\nGreen\nMountain State", hjust = "left", family = font, size = 5, color = "#F2F2F2") +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 9, color = "#F2F2F2"),
    plot.caption.position = "plot",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5,"cm"),
    plot.background = element_rect(color = NA, fill = "#2E7D32"),
    panel.background = element_rect(color = NA, fill = "#2E7D32")) +
labs(caption = "\n#30DayMapChallenge | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("green_mountains_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

