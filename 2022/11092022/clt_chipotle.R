# 30DayMapChallenge | November 9, 2022 | Space
# Spacing of Chipotle locations in Charlotte, NC
# Data sources - Chipotle

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(tidygeocoder)
library(ggvoronoi)
library(sf)

# add font ----------------------------------------------------------------
font_add_google(name = "Red Hat Display", family = "Red Hat Display")
font_add_google(name = "Red Hat Text", family = "Red Hat Text")

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# create font variable ----------------------------------------------------
font1 <- "Red Hat Display"
font2 <- "Red Hat Text"

# read in csv file of chipotle locations in Charlotte,  NC ----------------
locations <- read.csv("chipotle_clt.csv", stringsAsFactors = FALSE)

# get geo codes for locations ---------------------------------------------
osm_locations <- geo(address = locations$Address, method = "osm",
  lat = y, long = x)

# get Charlotte shape -----------------------------------------------------
clt_bb <- getbb("Charlotte, North Carolina")

boundary <- clt_bb %>%
  opq() %>%
  add_osm_feature(key = "admin_level", 
                  value = c("8")) %>%
  osmdata_sf() %$% 
  osm_multipolygons %>% 
  filter(name == "Charlotte") 

# create Charlotte boundary for clip ------------------------------------------------
boundary_sp <- sf:::as_Spatial(boundary$geometry)

# create plot -------------------------------------------------------------
osm_locations %>% 
ggplot() +
  geom_voronoi(aes(x = x, y = y), outline = boundary_sp, fill = "#9E9E9E", color = "#BDBDBD") +
  geom_point(aes(x = x, y = y), color = "#AC2318") +
  coord_map() +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 24, hjust = 0.5, face="bold", color = "#F2F2F2"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 10, hjust = 0.5, color = "#F2F2F2"),
        plot.caption = element_text(family = font2, hjust = 0.5, size = 8, color = "#F2F2F2"),
        plot.caption.position = "plot",
        plot.margin = margin(0.5, 0.5, 0.5, 0.5,"cm"),
        plot.background = element_rect(color = NA, fill = "#121212"),
        panel.background = element_rect(color = NA, fill = "#121212")) +
  labs(title = "WHERE TO GET A BURRITO?",
       subtitle = "Partition of space based on the minimal distance between the 14 Chipotle\nrestaurant locations within the city boundary of Charlotte, North Carolina.",
       caption = "\n#30DayMapChallenge | Data: chipotle.com | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("clt_chipotle_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)







