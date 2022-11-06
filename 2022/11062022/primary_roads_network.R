# 30DayMapChallenge | November 6, 2022 | Network
# U.S. Primary Roads
# Data sources - data.gov

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)
library(sf)

# add font ----------------------------------------------------------------
font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load shapefiles ---------------------------------------------------------
roads_raw <- read_sf("./highway_data/tl_2019_us_primaryroads.shp")

# create roads data frame -------------------------------------------------
df <- roads_raw %>%
  filter(RTTYP %in% c("I", "S", "U", "M")) %>% 
  st_as_sf()

# create list of facet labels ---------------------------------------------
names <- c("I" = "Interstate", "S" = "State", "M" = "Common Name", "U" = "U.S. Numbered")

# create plot -------------------------------------------------------------
ggplot() +
  geom_sf(data = df, aes(geometry = geometry), color = "#F2F2F2", size = 0.2) +
  coord_sf(xlim = c(-127, -70), ylim = c(21, 55), expand = FALSE) +
  theme_void() +
  facet_wrap(~ RTTYP, labeller = as_labeller(names)) +
  theme(text = element_text(size = 10, family = font, color = "#F2F2F2"),
        plot.title = element_text(family = font, size = 32, hjust = 0.5, face="bold", color = "#F2F2F2"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 12, hjust = 0.5, color = "#F2F2F2"),
        plot.caption = element_text(family = font, hjust = 0.5, size = 10),
        plot.caption.position = "plot",
        strip.text.x = element_text(size = 10, color = "#F2F2F2", hjust = 0.5, family = font),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5,"cm"),
        plot.background = element_rect(color = NA, fill = "#202A44"),
        panel.background = element_rect(color = NA, fill = "#202A44")) +
  labs(title = "U.S. PRIMARY ROADS",
       subtitle = "The United States uses a network of different primary road types to\nprovide transportation coverage across the country (2019 Census data).\n\n",
       caption = "\n\n#30DayMapChallenge | Data: data.gov | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("primary_road_network_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 7)

