# 30DayMapChallenge | November 14, 2022 | Hexmaps
# Gas Prices
# Data source is www.aaa.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(scales)
library(rvest)
library(janitor)
library(geojsonio)
library(sf)
library(broom) 
library(rgeos)
library(viridis)

# add font ----------------------------------------------------------------
font_add_google(name = "Bungee", family = "Bungee")
font_add_google(name = "Open Sans", family = "Open Sans")

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# create font variable ----------------------------------------------------
font <- "Bungee"
font2 <- "Open Sans"

# WEB SCRAPE -----------------------------------------------------

# get data using rvest for screen scraping html ---------------------------

# url
url <- "https://gasprices.aaa.com/state-gas-price-averages/"

web_data <- read_html(url)

# get data and create df --------------------------------------------------
gas_prices_raw <- web_data %>%
  html_nodes(xpath = '//*[@id="sortable"]') %>%
  html_table()

gas_prices_raw_df <- data.frame(gas_prices_raw) %>%
  clean_names() %>% 
  select(state, regular) %>% 
  mutate(regular = gsub("\\$", "", regular)) %>% 
  mutate(regular = as.numeric(as.character(regular)))

# load hex bin shapes -----------------------------------------------------
hex_states <- geojson_read("us_states_hexgrid.geojson", what = "sp")

# clean up state names ----------------------------------------------------
hex_states@data = hex_states@data %>% 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# create data frame for hex plot ------------------------------------------
hex_states_fortify <- tidy(hex_states, region = "google_name")

df_temp <- hex_states_fortify %>% 
  left_join(. , gas_prices_raw_df, by = c("id" = "state")) %>% 
  mutate(id = state.abb[match(id, state.name)])

# fix DC ------------------------------------------------------------------
df_temp$id[df_temp$group == "District of Columbia.1"] <- "DC"
  
# create labels for state -------------------------------------------------
labels <- cbind.data.frame(data.frame(gCentroid(hex_states, byid = TRUE), id = hex_states@data$iso3166_2))

# combine hex and labels for df -------------------------------------------
df <- df_temp %>% 
  left_join(. , labels, by = "id") 

# create plot -------------------------------------------------------------
df %>% 
ggplot () +
  geom_polygon(aes(x = long, y = lat, group = group, fill = regular), color="#000000", size = 0.5) +
  scale_fill_viridis(option = "F", direction = -1, labels = scales::dollar_format()) +
  coord_map() +
  geom_text(aes(x = x, y = y, label = id, color = regular <= 4), family = font2, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("#F2F2F2", "#000000")) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 29, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 10, hjust = 0.5),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, family = font2, color = "#000000", hjust = 0.5),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 8, family = font2, color = "#000000"),
        legend.key.height = unit(0.25, 'cm'),
        legend.key.width = unit(1.5, 'cm'),
        plot.margin = unit(c(0, 0.5, 0, 0.5), "cm"),
        plot.background = element_rect(color = "#F2F2F2", fill = "#F2F2F2")) +
  labs(title = "NATIONAL GAS PRICES",
       subtitle = "The average price of a gallon of regular gasoline by state (as of 11/14/2022).\n\n",
       caption = "\n\n#30DayMapChallenge | Data: aaa.com | Design: Ryan Hart")


# save plot ---------------------------------------------------------------
ggsave(paste0("gas_prices_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)


