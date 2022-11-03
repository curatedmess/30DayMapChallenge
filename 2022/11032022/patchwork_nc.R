# 30DayMapChallenge | November 3, 2022 | Polygons
# North Carolina - Patchwork Quilt
# Data source is nconemap.gov


# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(rgdal)
library(broom)
library(ggpattern)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load old QGIS shapefiles ------------------------------------------------
nc_spdf <- readOGR("./nc_data", layer = "North_Carolina_State_and_County_Boundary_Polygons")


# create data frames from shape files for plotting ------------------------
nc_spdf_fortified_raw <- tidy(nc_spdf)

# set seed variable -------------------------------------------------------
set.seed(74)

# create df with some random variables for the pattern diversity ----------
df <- nc_spdf_fortified_raw %>% 
  group_by(id) %>% 
  mutate(ran = sample(1:100,1)) %>% 
  mutate(ran2 = sample(1:100,1)) %>% 
  mutate(ran3 = sample(1:100,1)) %>% 
  mutate(ran4 = sample(1:100,1)) %>% 
  mutate(ran5 = sample(1:100,1)) %>% 
  mutate(ran6 = sample(1:100,1)) %>% 
  ungroup()

# create plot --------------------------------------------------------------
df %>% 
ggplot() +
  geom_polygon_pattern(aes(x = long, y = lat, group = group, 
                           pattern = as.factor(ran), 
                           pattern_fill = as.factor(ran3),
                           pattern_angle = as.factor(ran2),
                           pattern_spacing = as.factor(ran4),
                           pattern_density = as.factor(ran5), 
                           pattern_color = as.factor(ran6)), fill = "#F2F2F2", color = "#000000", size = 0.8) +
  coord_fixed() +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 34, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 10, hjust = 0.5),
        plot.caption = element_text(family = font, hjust = 0.5, size = 9,),
        plot.caption.position = "plot",
        legend.position = "Null",
        plot.margin = margin(0, 0, 0, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "North Carolina",
      subtitle = "One state and a patchwork of a hundred counties.\n",
      caption = "\n\n#30DayMapChallenge | Data: nconemap.gov | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("patchwork_nc_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 5)





