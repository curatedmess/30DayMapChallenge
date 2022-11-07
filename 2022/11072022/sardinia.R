# 30DayMapChallenge | November 7, 2022 | Raster
# Sardinia, Italy - Blue Zone (Perdasdefogu)
# Data sources - italoamericano.org

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(raster)
library(sf)
library(rnaturalearth)
library(elevatr)
library(scico)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Halant", family = "Halant")
font_add_google(name = "Proza Libre", family = "Proza Libre")
font_add_google(name = "Dancing Script", family = "Dancing Script")
font <- "Halant"
font2 <- "Proza Libre"
font3 <- "Dancing Script"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get country shape data for italy ---------------------------------------
sf_italy <- ne_states(country = "italy", returnclass = "sf") 

# get country shape data for sardinia ------------------------------------
sf_sardinia <- sf_italy %>% 
  filter(region == "Sardegna")

# create bluezone df ------------------------------------------------------
sf_bluezones <- data.frame(x = 9.433333, y = 39.683333, name = "Perdasdefogu")

# get the elevation data for sardinia --------------------------------------
dem_sardinia <- get_elev_raster(locations = sf_sardinia, z = 11, clip = "locations", neg_to_na = "TRUE")

# create a data frame to allow for plotting -------------------------------
df <- as.data.frame(dem_sardinia, xy = TRUE) %>% 
  rename_with(.cols = 3, ~"elevation") %>% 
  drop_na(elevation)

# create plot -------------------------------------------------------------
ggplot() + 
  annotate(geom = "text", y = 41.35, x = 9.0, label = "SARDINIA", hjust = "center", family = font, fontface = "bold", size = 23, color = "#F2F2F2") +
  annotate(geom = "text", y = 41.08, x = 10.3, label = "Italy", hjust = "center", family = font3, size = 9, color = "#F2F2F2") +
  annotate(geom = "text", y = 40.40, x = 9.93, label = "BLUE ZONE", hjust = "left", family = font, fontface = "bold", size = 5.75, color = "#F2F2F2") +
  annotate(geom = "richtext", y = 40.06, x = 9.905, label = "Located in the more sparsely<br>populated mountainous region<br>of the island of Sardinia is a high<br>concentration of centenarians<br>and supercentenarians. For<br>example, the small village of<br><b>Perdasdefogu</b> with 1,700 people<br>boasts ten centenarians (as of<br>March 2022).", hjust = "left", family = font2, size = 2.1, color = "#F2F2F2", fill = NA, label.color = NA, lineheight = 1.4) +
  geom_sf(data = sf_sardinia, color = "#001959", fill = "#001959", size = 1) +
  geom_raster(data = df, aes(x = x, y = y, fill = elevation)) +
  geom_point(data = sf_bluezones, aes(x = x, y = y), color = "#F2F2F2", size = 1.1) +
  annotate(geom = "curve", x = 10.3, y = 39.72, xend = 9.75, yend = 39.65, curvature = -0.35, size = 0.3,  arrow = arrow(length = unit(1.5, "mm")), color = "#F2F2F2") +
  coord_sf(xlim = c(7.0, 11), ylim = c(38.75, 41.65), expand = FALSE) +
  scale_fill_scico(palette = "batlowW", direction = 1, name = "Elevation (m)") +
  theme_void() +
  theme(plot.caption = element_text(family = font2, hjust = 0.5, size = 8, color = "#F2F2F2"),
    plot.caption.position = "plot",
    legend.position = c(0.13, 0.25),
    legend.title = element_text(family = font2, hjust = 0.5, size = 6, color = "#F2F2F2"),
    legend.text = element_text(family = font2, hjust = 0.5, size = 6, color = "#F2F2F2"),
    plot.margin = margin(0.5, 0.25, 0.5, 0.25,"cm"),
    plot.background = element_rect(color = NA, fill = "#003399"),
    panel.background = element_rect(color = NA, fill = "#003399")) +
labs(caption = "\n#30DayMapChallenge | Data: italoamericano.org | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("sardinia_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

