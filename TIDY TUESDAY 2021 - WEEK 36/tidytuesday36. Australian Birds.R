library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(magrittr)
library(visdat)
library(lemon)
library(purrr)
library(forcats)
library(ggplot2)
library(ggthemes)
library(scales)
library(colorspace)
library(ggpol)
library(tidytext)
library(gtable)
library(ggtext)
library(showtext)
library(grid)
library(ragg)


# DATA

tuesdata <- tidytuesdayR::tt_load(2021, week = 36)

bird_baths_raw<- tuesdata$bird_baths

# EXPLORING DATA

bird_baths_raw %>% 
  count(survey_year, sort = TRUE)

bird_baths_raw %>% 
  count(urban_rural, sort = TRUE)

bird_baths_raw %>% 
  count(bioregions, sort = TRUE)

bird_baths_raw %>% 
  count(bird_type, sort = TRUE)

# FINDING NAS

vis_dat(bird_baths_raw, sort_type = FALSE)

vis_miss(bird_baths_raw)


# CLEANING NA AND ADDING SEASON COLUMN

bird_baths <- bird_baths_raw %>%  
  filter(!is.na(survey_year) & bird_count > 0) %>% 
  mutate(season = case_when(survey_year == "2014" ~ "Winter",
                            survey_year == "2015" ~ "Summer"))

# Filtering top bird_type species
top_birth_type <- bird_baths %>% 
  group_by(bird_type) %>% 
  summarise(freq = sum(bird_count)) %>% 
  ungroup() %>% 
  arrange(-freq)

top_21_bird_type <- top_birth_type %>% 
  top_n(n = 21, freq) %>% 
  pull(bird_type)

top_21_data <- bird_baths %>% 
  filter(bird_type %in% top_21_bird_type)

# Creating plots data

year_urban_rural <- top_21_data %>%
  group_by(survey_year, season, urban_rural, bird_type) %>%
  summarise(freq = sum(bird_count)) %>%
  arrange(freq, .by_group = TRUE) %>% 
  ungroup() %>% 
  mutate(freq = ifelse(survey_year == 2014, freq*-1, freq))


# Let's graph!

# Legend colors
cols <- c("Rural" = "#E4002B", "Urban" = "#012169")

# Facet labels 
labs_survey_year <- c("2014" = "Winter", "2015" = "Summer")

# Importing font
font_add(family = "copper", regular = "Copperplate.ttc")
showtext_auto()

p <- year_urban_rural %>% 
  ggplot(aes(bird_type, freq, fill = urban_rural)) +
  geom_col(position = "dodge") + 
  scale_fill_manual(values = cols, name = "Area:") +
  scale_x_discrete(position = "bottom") +
  scale_y_continuous(breaks = seq(-150, 150, 30), labels = abs(seq(-150, 150, 30)), position = "left") +
  coord_flex_flip() +
  theme(text = element_text(family = "copper"),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.text = element_text(size = 12, colour = "#012169"),
        legend.title = element_text(face = "bold", size = 15, colour = "#012169"),
        panel.grid = element_line(size = 0.1, color = "#012169"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks.y = element_line(color = "#012169", size = 0),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 12, 
                                   color = "#012169"),
        axis.text.y = element_text(size = 18, face = "bold", 
                                   color = "#012169"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15, face = "bold", hjust = 0.5, angle = 0, 
                                    colour = "#E4002B", margin = margin(0, 0, 0.3, 0, "cm")),
        strip.background = element_blank(),
        strip.text = element_text(size = 22, face = "bold", 
                                  color = "#012169", margin = margin(0, 0, 1, 0, "cm")),
        plot.title = element_text(size = 30, face = "bold", color = "#012169", hjust = 0.5),
        plot.subtitle = element_text(size = 20, colour = "#012169", 
                                     hjust = 0.5),
        plot.caption = element_text(size = 14, colour = "#012169", 
                                    margin = margin(0.5, 0, 0, 0, "cm")),
        plot.margin = margin(2, 2, 1, 1, "cm")) +
  facet_share(~ survey_year, dir = "h",  scales = "free_x", 
              labeller = labeller(survey_year = labs_survey_year), strip.position = "top") +
  labs(title = "Australian Birds", 
       subtitle = "Birds types over 100 sightings per season and per area",
       y = "Bird Type", caption = "Data Source: Cleary et al, 2016. Dataviz by Marco Arellano")
p

# Converting p to ggplotGrob object to eliminate white space in the left margin
gp <- ggplotGrob(p)

# Helps to understand the gtable objecT
gp$layout
gtable_show_layout(gp)

# You can modify this to your liking
gp$widths[4] <- unit(1, 'cm')

# grid.newpage()
grid.draw(gp)

ggsave("ausbirds.plot.jpg", plot = gp, scale = 1.5, 
       device = agg_png(width = 9, height = 7, units = "in", res = 300))

