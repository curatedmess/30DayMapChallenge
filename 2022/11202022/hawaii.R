# 30DayMapChallenge | November 20, 2022 | My favorite...
# Hawaiian Islands

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(raster)
library(sf)
library(rnaturalearth)
library(elevatr)
library(metR)
library(scico)

# add font ----------------------------------------------------------------
font_add_google(name = "Karla", family = "Karla")
font <- "Karla"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get island shape data for Hawaiian Islands -------------------------------
sf_islands <- ne_states(country = "United States of America", returnclass = "sf") %>% 
        filter(name %in% c("Hawaii"))

# get the elevation data --------------------------------------------------
dem_islands <- get_elev_raster(locations = sf_islands, z = 6, clip = "locations", neg_to_na = "TRUE")

# create a data frame to allow for plotting -------------------------------
df <- as.data.frame(dem_islands, xy = TRUE) %>% 
  rename_with(.cols = 3, ~"elevation") %>% 
  drop_na(elevation)

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x, y)) + 
  geom_contour_fill(aes(z = elevation)) +
  geom_contour_tanaka(aes(z = elevation )) +
  scale_fill_scico(palette = "batlowK") +
  annotate("text", x = -160, y = 19.6, label = "Hawaiian Islands", hjust = "left", family = font, size = 6, color = "#F2F2F2") +
  annotate("text", x = -160, y = 19.35, label = "Elevation in meters", hjust = "left", family = font, size = 2.5, color = "#F2F2F2") +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 6, color = "#F2F2F2"),
        plot.caption.position = "plot",
        legend.position = c(0.175, 0.1),
        legend.direction = "horizontal",
        legend.key.height = unit(0.20, 'cm'),
        legend.key.width = unit(0.75, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(family = font, hjust = 0.5, size = 5, color = "#F2F2F2"),
        plot.margin = margin(0.75, 0, 0.75, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#020405"),
        panel.background = element_rect(color = NA, fill = "#020405")) +
  labs(caption = "\n\n#30DayMapChallenge | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("hawaii_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 4)


