# 30DayMapChallenge | November 10, 2022 | Bad Map
# BADminton - Major US cities with dedicated badminton courts
# Data sources - worldbadminton.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(scales)
library(rvest)
library(maps)
library(ggpattern)
library(tidygeocoder)
library(ggimage)

# add font ----------------------------------------------------------------
font_add_google(name = "Red Hat Display", family = "Red Hat Display")
font_add_google(name = "Red Hat Text", family = "Red Hat Text")

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# create font variable ----------------------------------------------------
font1 <- "Red Hat Display"
font2 <- "Red Hat Text"

# WEB SCRAPE -----------------------------------------------------

# get data using rvest for screen scraping html ---------------------------

# url
url <- "https://www.worldbadminton.com/whereToPlay/unitedStates/major_us_cities.htm"

web_data <- read_html(url)

# get data and create df --------------------------------------------------
cities <- web_data %>%
 html_elements("strong") %>% 
  html_text %>% 
  data.frame() %>% 
  slice(-c(1, 2))

names(cities) <- ("city")



# get geo codes for locations ---------------------------------------------
osm_locations <- geo(city = cities$city, method = "osm",
                     lat = y, long = x)

# some data cleanup -------------------------------------------------------
df <- cities %>% 
  filter(!city %in% c("Charlotte", "Washington", "Albuquerque", "Tucson", "Miami", "Raleigh"))

# get background map image ------------------------------------------------
# load flag image ---------------------------------------------------------
racket <- magick::image_read("https://s0.geograph.org.uk/geophotos/04/46/78/4467817_62c364fb.jpg")
racket_img <- magick::image_write(racket, path = "racket.img", format = "png")

# get point image ------------------------------------------------
# load shuttlecock image ---------------------------------------------------------
shuttlecock <- magick::image_read("https://upload.wikimedia.org/wikipedia/commons/e/eb/Badminton_Shuttlecock_159415.png") %>%
  magick::image_trim()

shuttlecock2_img <- magick::image_write(shuttlecock, path = "shuttlecock2.img", format = "png")


# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_polygon_pattern(data = map_data("usa"), aes(x = long, y = lat, group = group), pattern_filename = racket_img, pattern_type = "expand", pattern = "image") +
  geom_image(data = osm_locations, aes(x, y, image = shuttlecock2_img), nudge_x = 0.6, nudge_y = 0.6) +
  coord_map(clip="off") +
  theme_void() +
  theme(plot.title = element_text(family = font1, size = 32, hjust = 0.5, face="bold", color = "#121212"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 12, hjust = 0.5, color = "#121212"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 10, family = font2, color = "#000000", hjust = 1),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.background = element_rect(color = "#F2F2F2", fill = "#F2F2F2")) +
  labs(title = "BADMINTON",
       subtitle = "Major cities in the United States with dedicated badminton courts.",
    caption = "\n\n#30DayMapChallenge | Data: worldbadminton.com | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("badminton_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 6)



  
