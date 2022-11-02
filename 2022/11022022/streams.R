# 30DayMapChallenge | November 2, 2022 | Lines
# Mecklenburg County Streams
# Data source is Charlotte Open Data and keepingwatch.org
# https://keepingwatch.org/programming/creeks/up-the-creeks

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(rgdal)
library(broom)
library(sf)
library(ggtext)
library(osmdata)

# add font ----------------------------------------------------------------
font_add_google(name = "Caveat", family = "Caveat")
font_add_google(name = "Inter", family = "Inter")
font2 <- "Caveat"
font <- "Inter"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# get boundary shape for the county ---------------------------------------
mc_bb <- getbb("Mecklenburg County")

# get polygon -------------------------------------------------------------
boundary <- mc_bb %>%
  opq() %>%
  add_osm_feature(key = "admin_level", 
                  value = c("6")) %>%
  osmdata_sf() %$% 
  osm_multipolygons %>% 
  filter(name == "Mecklenburg County") %>% 
  select(geometry) %>% 
  st_transform(4326)

# load shapefiles ---------------------------------------------------------
streams_spdf <- readOGR("./streams_data", layer = "Streams")

# create data frame from shape files for plotting -------------------------
streams_spdf_fortified <- tidy(streams_spdf)

# fix the coordinates/projection ------------------------------------------
# https://stackoverflow.com/questions/70524765/converting-projected-coordinates-into-decimals
df <- streams_spdf_fortified %>%
  st_as_sf(coords = c("long", "lat"), dim = "XY") %>%
  st_set_crs(6543) %>%
  st_transform(4326) %>%
  as.data.frame() %>%
  extract(geometry,
          c('long', 'lat'),
          '\\((.*), (.*)\\)',
          convert = TRUE)

# create plot -------------------------------------------------------------
ggplot() +
  geom_sf(data = boundary, fill = "NA", color = "#F2F2F2") +
  geom_path(data = df, aes(x = long, y = lat, group = group), color = "#448AFF", size = 0.35) +
  annotate(geom = "text", y = 35.47, x = -80.76, label = "Mecklenburg County, NC", hjust = "left", family = font2, size = 6, fontface = "bold", color = "#F2F2F2") +
  annotate(geom = "richtext", y = 35.412, x = -80.73, label = "End-to-end, the <b><span style='color: #448AFF;'>streams</span></b> in<br>Mecklenburg County could<br>stretch 3,000 miles.", hjust = "left", family = font2, size = 4.75, color = "#F2F2F2", fill = NA, label.color = NA) +
  coord_sf(xlim = c(-81.15, -80.5), label_graticule = "SW", expand = TRUE) +
  theme_minimal() +
  theme(plot.caption = element_text(family = font2, hjust = 0.5, size = 10, color = "#F2F2F2"),
    plot.caption.position = "plot",
    plot.margin = margin(0.5, 0.5, 0.75, 0.5,"cm"),
    axis.title = element_blank(),
    panel.grid = element_line(color = "#212121", size = 0.25),
    axis.text = element_text(family = font2, hjust = 0.5, size = 8.5, color = "#F2F2F2"),
    plot.background = element_rect(color = NA, fill = "#000000"),
    panel.background = element_rect(color = NA, fill = "#000000")) +
 labs(caption = "\n#30DayMapChallenge | Data: charlottenc.gov & keepingwatch.org | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("streams_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)




