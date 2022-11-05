# 30DayMapChallenge | November 5, 2022 | Ukraine
# Ukraine Flag

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpattern)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load flag image ---------------------------------------------------------
flag <- magick::image_read("https://upload.wikimedia.org/wikipedia/commons/6/6a/Flag_of_Ukraine_%281-1%29.png")
flag_img <- magick::image_write(flag, path = "flag.png", format = "png")

# create base map ---------------------------------------------------------
map <- ne_countries(scale = 50, returnclass = 'sf')

# create plot -------------------------------------------------------------
map %>% 
ggplot() +
  geom_sf_pattern(aes(pattern = ifelse(name == "Ukraine", "yes", "no"), geometry = geometry), pattern_filename = flag_img, pattern_type = "expand", size = 0.6, color = "#000000") +
  coord_sf(xlim = c(10, 52), ylim = c(40, 63.5), expand = FALSE) +
  scale_pattern_manual(values = c("none", "image")) +
  theme_void() +
  theme(plot.caption = element_text(family = font, hjust = 0.5, size = 12),
        plot.caption.position = "plot",
        legend.position = "Null",
        plot.margin = margin(0, 0, 0.55, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#E3F2FD")) +
  labs(
    caption = "\n\n#30DayMapChallenge | Ukraine | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("ukraine_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 8)






