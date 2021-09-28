library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(magrittr)
library(lubridate) # Work with dates
library(stringr) # Work with strings
library(forcats) # Work with factors
library(ggplot2)
library(scales) 
library(showtext) # Import and use fonts
library(ragg) 

# Loading data
tuesdata <- tidytuesdayR::tt_load(2021, week = 38, download_files = "billboard")

billboard_raw <- tuesdata$billboard

billboard <- billboard_raw %>% 
  group_by(song) %>% 
  mutate(week_id = mdy(week_id),
         year = year(week_id)) %>% 
  select(performer, song, year, week_id, week_position, previous_week_position:weeks_on_chart) %>% 
  arrange(song, week_id, .by_group = TRUE)
  

## coldplay
# Subsetting to only get data of Coldplay

coldplay_raw <- billboard %>% 
  filter(grepl("Coldplay", performer))

# Findings the dates which the songs reach their best position 

coldplay  <- coldplay_raw %>% 
  mutate(
    FlagBP = min(peak_position), # Get song best position
    FlagP = week_position == peak_position & week_position == FlagBP, # Week where song reach its best position
    FlagW = case_when(FlagP == TRUE ~ weeks_on_chart, # Get the number of week of the best position
                      TRUE ~ as.numeric(NA)),
    colorP  = case_when(
      FlagP == TRUE ~ "#FFD700", # Best Position color
      FlagP == FALSE & week_position <= 33 ~  "#cc4d98", # Upper Third color
      FlagP == FALSE & week_position <= 67  ~ "#1669da", # Medium Third color
      TRUE ~ "#a2e665"), # Lower Third Color
    colorP = factor(colorP, levels = c("#FFD700", "#cc4d98", "#1669da", "#a2e665")),
    shapeP = ifelse(performer == "Coldplay", 19, 15)) # Assinging a shape if the song was a collaboration or not

# Adding the y axis label by song and year 

y_text <- coldplay_raw %>%
  group_by(song) %>% 
  summarise(year_lab = (min(year))) %>% 
  arrange(year_lab) %>% 
  mutate(song_year = paste(song, year_lab, sep = "\n"))

coldplay_plot <-  coldplay %>% 
  left_join(y_text, by = "song") 


# adding font
font_add(family = "roboto", regular = "RobotoSlab-VariableFont_wght.ttf")
showtext_auto()


# plotting!

plot <- coldplay_plot %>% 
  ggplot(aes(y = reorder(song_year, -year_lab), color = colorP, shape = shapeP)) +
  geom_point(aes(x = FlagW), size = 6.5) +
  geom_point(aes(x = weeks_on_chart), size = 6.5) +
  geom_point(data = . %>% filter(performer != "Coldplay"),
             aes(x = FlagW), size = 6.5) +
  geom_point(data = . %>% filter(performer != "Coldplay"),
             aes(x = weeks_on_chart), size = 6.5) +
  geom_text(aes(x = weeks_on_chart, label = week_position),
            hjust = 0.5, vjust = 0.5, size = 3.5, colour = "black") +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_x_continuous(breaks = seq(1, 51, 2), labels = seq(1, 51, 2)) +
  scale_color_identity(guide = "legend", 
                       labels = c("#FFD700" = "Best Position",
                                  "#cc4d98" = "Upper Third",
                                  "#1669da" = "Medium Third",
                                  "#a2e665" = "Lower Third")) +
  scale_shape_identity(guide = "legend", breaks = c(19, 15), 
                       labels = c("19" = "Band", "15" = "Collaboration")) +
  guides(color = guide_legend(title = "Chart Position:" , title.hjust = 1, order = 1), 
         shape = guide_legend(title = "Performer:", title.hjust = 1, order = 2, 
                              override.aes = list(color = "#8d8d8d"))) +
  theme(
      text = element_text(family = "roboto", colour = "#eb854c"),
      legend.position = "top",
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.box.margin = margin(0, 2.3, 0.5, 0, "cm"),
      legend.key = element_blank(),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      panel.grid = element_line(color = "#eb854c", size = 0.2, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 12, colour = "#eb854c", 
                                 margin = margin(1, 0, 0, 0, "cm")),
      axis.text.y = element_text(size = 11, face = "bold", color = "#eb854c",
                                     margin = margin(0, 0.7, 0, 0, "cm")),
      axis.title.x = element_text(size = 11, face = "bold", hjust = 0.5, angle = 0, 
                                  margin = margin(0.7, 0, 0, 0, "cm")),
      axis.title.y = element_blank(),
      plot.background = element_rect(fill = "#242424", color = "#242424"),
      plot.title = element_text(size = 30, face = "bold", hjust = -0),
      plot.subtitle = element_text(size = 20, face = "bold", 
                                   hjust = -0, margin = margin(0, 0, 0.5, 0, "cm")),
      plot.caption = element_text(size = 12, margin = margin(0.8, 0, 0, 0, "cm")),
      plot.margin = margin(2, 2, 1, 1.5, "cm")) +
  labs(title = "Billboard top 100: Coldplay songs",
       subtitle = "Chart week position, Weeks spent in the chart, and Song best performance",
       x = "Number of Weeks",
       caption = "Data Source: Data.World, Billboard.com. Dataviz by Marco Arellano")

png(file="billboard.png", res=600, width=9600, height=6600, pointsize=10,
    type="quartz", antialias="none")
plot

dev.off()









