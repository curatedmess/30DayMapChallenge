# 30DayMapChallenge | November 1, 2022 | Points
# City Lights - Street Lights
# Data source is https://data.lacity.org
# focal point for the zoom in was San Antonio Winery area of downtown LA
# y = 34.0637 x = -118.2239
# cool story about the history of street lights in LA
# https://la.curbed.com/2018/8/1/17635608/streetlamps-urban-light-history-design

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(showtext)
library(ggshadow)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Limelight", family = "Limelight")
font_add_google(name = "Merriweather", family = "Merriweather")

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# create font variable ----------------------------------------------------
font <- "Limelight"
font2 <- "Merriweather"

# load data ---------------------------------------------------------------
lights_raw <- readr::read_csv('STLIGHT.csv')

# create data frame and do some data cleanup ------------------------------
df <- lights_raw %>% 
  clean_names() %>%
  separate(the_geom, c("shape", "x", "y"), " ") %>% 
  mutate(x = sub('.', '', x)) %>% 
  mutate(y = gsub('.$', '', y)) %>% 
  mutate(x = as.numeric(as.character(x))) %>% 
  mutate(y = as.numeric(as.character(y)))
  
# create plot -------------------------------------------------------------
ggplot() +
  geom_glowpoint(data = df, aes(x = x, y = y), size = 0.1, alpha = 0.9, color = "#FFFFFF", shadowsize = 0.2, shadowalpha = 0.025, shadowcolor = "#FFECB3") +
  annotate(geom = "text", y = 34.007, x = -118.235, label = "LOS ANGELES", hjust = "left", family = font2, size = 3.0, color = "#F2F2F2") +
  annotate(geom = "text", y = 34.002, x = -118.235, label = "Streetlights", hjust = "left", family = font, fontface = "bold", size = 7, color = "#F2F2F2") +
  annotate(geom = "text", y = 33.994, x = -118.235, label = "61,120 lights illuminate this\nsection of downtown", hjust = "left", family = font2, size = 2.5, color = "#F2F2F2") +
  xlim(-118.35, -118.18) +
  ylim(33.99, 34.10) +
  theme_void() +
  theme(plot.caption.position = "plot",
      plot.caption = element_text(size = 7.5, family = font2, color = "#F2F2F2", hjust = 0.5),
      plot.margin = unit(c(0.25, 0.25, 0.75, .25), "cm"),
      plot.background = element_rect(color = NA, fill = "#262626")) +
  labs(caption = "\n#30DayMapChallenge • Data: Los Angeles Open Data • Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("streetlights_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

