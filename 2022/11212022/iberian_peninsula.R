# 30DayMapChallenge | November 21, 2022 | Kontur Population Data
# Iberian Peninsula
# Data Source in https://data.humdata.org/dataset/kontur-population-dataset

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(sf)
library(rnaturalearth)
library(ggtext)
library(viridis)
library(scico)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get world shape data ---------------------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

# load into R -------------------------------------------------------------
raw_andorra <- read_sf("kontur_population_AD_20220630.gpkg")
raw_spain <- read_sf("kontur_population_FR_20220630.gpkg")
raw_france <- read_sf("kontur_population_ES_20220630.gpkg")
raw_portugal <- read_sf("kontur_population_PT_20220630.gpkg")
raw_algeria <- read_sf("kontur_population_DZ_20220630.gpkg")
raw_morocco <- read_sf("kontur_population_MA_20220630.gpkg")

# combine and update crs --------------------------------------------------
df <- rbind(raw_andorra, raw_spain, raw_france, raw_portugal, raw_algeria, raw_morocco) %>% 
  st_transform(df, crs = 4326)

# create plot -------------------------------------------------------------
ggplot() +
  geom_sf(data = world, size = 0.3, color = "#28282B", fill = "#F2F2F2") +
  geom_sf(data = df, aes(fill = population), color = NA, size = 0.1) +
  scale_fill_viridis(option = "C", trans = "log", direction = -1,
                     breaks = c(min(df$population), max(df$population)),
                     labels = c("Less", "More")) +
  coord_sf(xlim = c(-9.5, 3.5), ylim = c(35, 44), expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 20, hjust = 0.5, face = "bold", color = "#28282B"),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#28282B"),
        plot.caption.position = "plot",
        legend.position = c(0.1, 0.05),
        legend.direction = "horizontal",
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.5, 'cm'),
        legend.title = element_blank(),
        legend.text = element_text(family = font, hjust = 0.5, size = 5, color = "#28282B"),
        plot.margin = margin(0.5, 0.25, 0.5, 0.25,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "Iberian Peninsula Population\n",
       caption = "\n\n#30DayMapChallenge | Data: HDX | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("iberian_peninsula_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

