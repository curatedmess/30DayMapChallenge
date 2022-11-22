# TidyTuesday | November 22, 2022 | Museums
# 30DayMapChallenge | November 23, 2022 | Movement
# Data Source is the Mapping Museums project

# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidytuesdayR)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Courier Prime", family = "Courier Prime")
font <- "Courier Prime"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# load data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2022-11-22')
museums <- tuesdata$museums

# remove the channel islands museum ---------------------------------------
df <- museums %>% 
  filter(!museum_id == "mm.New.88") %>% 
  group_by(Size) %>% 
  mutate(Museum_Size = case_when(
    Size %in% c("huge", "large") ~ "Huge and Large",
    Size %in% c("medium", "small") ~ "Medium and Small",
    Size == "unknown" ~ "It's a Surprise"))

# get basic base map of UK ------------------------------------------------
map <- map_data(map = "world", region = "UK")

# re-order factor ---------------------------------------------------------
df$Museum_Size <- factor(df$Museum_Size, levels=c("Huge and Large", "Medium and Small", "It's a Surprise"))

# create plot -------------------------------------------------------------
ggplot() +
  geom_polygon(data = map, aes(x = long, y = lat, group = group), fill = "#4c4f55") +
  geom_point(data = df, aes(x = Longitude, y = Latitude, color = Museum_Size), size = 0.6, alpha = 0.75) +
  coord_map(xlim = c(-13, NA), clip = "off") +
  scale_color_manual(values = c("#a4c5da", "#bdad9b", "#a15c57"), name = "Museum Size") +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  theme_void() +
  theme(plot.title = element_text(family = font, size = 16, hjust = 0.5, color = "#28282B"),
        plot.title.position = "plot",
        plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#28282B"),
        plot.caption.position = "plot",
        legend.position = c(0.1, 0.2),
        legend.title = element_text(family = font, hjust = 0.5, size = 11, color = "#28282B"),
        legend.text = element_text(family = font, size = 8, color = "#28282B"),
        plot.margin = margin(0.5, 0, 0.5, 0,"cm"),
        plot.background = element_rect(color = NA, fill = "#F2F2F2"),
        panel.background = element_rect(color = NA, fill = "#F2F2F2")) +
  labs(title = "4,190 MUSEUMS ACROSS THE UK",
       caption = "\n\n#TidyTuesday & #30DayMapChallenge | Data: MuseWeb | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("uk_museums_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

