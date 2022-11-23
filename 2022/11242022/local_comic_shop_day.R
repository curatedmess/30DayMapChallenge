# 30DayMapChallenge | November 24, 2022 | Fantasy
# Local Comic Shop Day
# Data Source is localcomicshopday.com

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(rvest)
library(tidygeocoder)
library(rnaturalearth)
library(sf)

# add font ----------------------------------------------------------------
font_add_google(name = "Bangers", family = "Bangers")
font_add_google(name = "Comic Neue", family = "Comic Neue")
font1 <- "Bangers"
font2 <- "Comic Neue"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# WEB SCRAPE -----------------------------------------------------
# get data using rvest for screen scraping html ---------------------------
# url
url <- "http://localcomicshopday.com/list-of-stores/"

web_data <- read_html(url)

# get data and create df --------------------------------------------------
comic_stores_raw <- web_data %>%
  html_nodes(xpath = '//*[@id="tablepress-15"]') %>%
  html_table()

# create df, clean up and wrangle with geocode ----------------------------
df <- data.frame(comic_stores_raw) %>% 
  filter(Country %in% c("UNITED STATES", "MEXICO", "CANADA")) %>% 
  select(CITY, STATE, Country) %>% 
  map_df(str_replace, pattern = ",", replacement = "") %>% 
  mutate(CITY = replace(CITY, CITY == "PORTKAND", "PORTLAND")) %>%
  mutate(CITY = replace(CITY, CITY == "OAKALND", "OAKLAND")) %>%
  mutate(CITY = replace(CITY, CITY == "QUINCYX", "QUINCY")) %>%
  mutate(CITY = replace(CITY, CITY == "MORGNATOWN", "MORGANTOWN")) %>%
  mutate(CITY = replace(CITY, CITY == "CREVE COUER", "CREVE COEUR")) %>%
  mutate(CITY = replace(CITY, CITY == "SWARTHMOR", "SWARTHMORE")) %>%
  mutate(STATE = replace(STATE, STATE == "AB", "ALBERTA")) %>%
  mutate(STATE = replace(STATE, STATE == "BC", "BRITISH COLUMBIA")) %>%
  mutate(STATE = replace(STATE, STATE == "SK", "SASKATCHEWAN")) %>%
  mutate(STATE = replace(STATE, CITY == "BEDFORD", "NOVA SCOTIA")) %>%
  mutate(STATE = replace(STATE, CITY == "STONEY CREEK", "ONTARIO")) %>%
  mutate(STATE = replace(STATE, STATE == "QC", "QUEBEC")) %>%
  mutate(STATE = replace(STATE, STATE == "MB", "MANITOBA")) %>%
  mutate(STATE = replace(STATE, STATE == "ON", "ONTARIO")) %>%
  mutate(STATE = replace(STATE, CITY == "QUERETARO", "QUERETARO")) %>%
  mutate(STATE = replace(STATE, CITY == "COAPA", "MEXICO CITY")) %>%
  mutate(STATE = replace(STATE, CITY == "QUERETARO", "JALISCO")) %>%
  mutate(STATE = replace(STATE, CITY == "MEXICO CITY", "MEXICO CITY")) %>%
  mutate(STATE = replace(STATE, CITY == "MONTERREY", "NUEVO LEON")) %>%
  mutate(ADDRESS = paste(CITY,",", STATE, ",", Country)) %>% 
  geocode(ADDRESS, method = 'osm', lat = y , long = x)

# create sf for the points -----------------------------------------------
df_sf <- st_as_sf(df, coords = c("x", "y"), crs = 4326)

# get world shape data ---------------------------------------------------
world <- ne_countries(scale = "large", returnclass = "sf")

# thanks to https://github.com/gkaramanis ---------------------------------
# for the cheat code on creating a globe ----------------------------------

# ocean detail ------------------------------------------------------------
ocean <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = "+proj=ortho +lat_0=35 +lon_0=-100")

# create plot -------------------------------------------------------------
ggplot() +
  geom_sf(data = ocean, fill = "#0393da", color = NA) +
  geom_sf(data = world, fill = "#cadb2a", color = "#28282B", size = 0.25) +
  geom_sf(data = df_sf, color = "#be1a87", size = 0.4) +
  coord_sf(crs = "+proj=ortho +lat_0=35 +lon_0=-100") +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 32, hjust = 0.5, color = "#28282B"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 12, family = font2, color = "#28282B", hjust = 0.5),
        plot.caption = element_text(size = 11, family = font2, color = "#28282B", hjust = 0.5),
        plot.caption.position = "plot",
        legend.position = "Null",
        plot.margin = margin(0.75, 0, 0.75, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "Local Comic Shop Day",
       subtitle = "November 23, 2022",
       caption = "\n\n#30DayMapChallenge | Data: localcomicshopday.com | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("local_comic_shop_day_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)




