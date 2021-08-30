library(tidytuesdayR)
library(magrittr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(colorspace)

# Data
tuesdata <- tidytuesdayR::tt_load(2021, week = 35)
lemur <- tuesdata$lemur_data
taxonomy <- tuesdata$taxonomy

glimpse(lemur)

# Adding genus names 

taxonomy %<>% 
  mutate(genus_family = 
           case_when(latin_name == "Lemur catta" ~ "Lemur",
                     latin_name ==  "Cheirogaleus medius" ~ "Cheirogaleus or Dwarf Lemur",
                     latin_name == "Daubentonia madagascariensis" 
                     ~ "Daubentonia or Aye-aye",
                     latin_name %in% c("Eulemur albifrons", "Eulemur collaris","Eulemur coronatus", 
                                       "Eulemur Eulemur","Eulemur flavifrons", "Eulemur fulvus",
                                       "Eulemur macaco", "Eulemur mongoz", "Eulemur rubriventer",
                                       "Eulemur rufus", "Eulemur sanfordi") 
                     ~ "Eulemur or Brown Lemur",
                     latin_name == "Galago moholi" 
                     ~ "Galago or Mohol bushbaby",
                     latin_name == "Hapalemur griseus griseus" 
                     ~ "Hapalemur or Bamboo Lemur",
                     latin_name == "Loris tardigradus" 
                     ~ "Slender loris",
                     latin_name == "Mircocebus murinus" ~ "Microcebus or Grey Mouse Lemur",
                     latin_name == "Mirza coquereli" 
                     ~ "Mirza or Giant Mouse Lemur",
                     latin_name %in% c("Nycticebus coucang", "Nycticebus pygmaeus") 
                     ~ "Nycticebus or Slow Lemur",
                     latin_name == "Otolemur garnettii garnettii" 
                     ~ "Otolemur or Northern Greater Galago",
                     latin_name == "Perodicticus potto" 
                     ~ "Potto",
                     latin_name == "Propithecus coquereli"
                     ~ "Propithecus or Coquerel's Sifaka",
                     latin_name %in% c("Varecia rubra", "Varecia Varecia",
                                       "Varecia variegata variegata") 
                     ~ "Varecia or Ruffed Lemur"))

# Joining lemur and taxonomy datasets

lemur_taxon <- lemur %>% 
  inner_join(taxonomy, by = "taxon")

# Grouping data by genus_family, birth_type and sex

lemur_genus_birth_sex_<- lemur_taxon %>% 
  filter(sex != "ND") %>% 
  group_by(genus_family, birth_type, sex) %>% 
  summarise(age_max_live_or_dead_y = mean(age_max_live_or_dead_y)) %>% 
  arrange(desc(age_max_live_or_dead_y))
  
#Let's graph!

# Colors Values and Labels
labels <- c("CB" = "Captive-Born", "WB" = "Wild-Born", "Unk" = "Unknown")
cols <- c("CB" = "lightpink3", "WB" = "steelblue3", "Unk" = "lightgreen")

# Shape Values and Labels
shape_value <- c("F" = "circle", "M" = "square")
shape_labs <- c("F" = "Female", "M" = "Male")

  
lemur_plot <- lemur_genus_birth_sex_ %>% 
  ggplot(aes(genus_family, age_max_live_or_dead_y, color = birth_type, shape = sex)) +
  geom_point(size = 8, alpha = 0.8) + 
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 40, by = 5)) +
  scale_color_manual(values = cols, labels = labels, name = "Place of birth:") +
  scale_shape_manual(name = "Sex:", values = shape_value, labels = shape_labs) + 
  theme_wsj() +
  theme(axis.title.x = element_text(face = "bold", size = 15, margin = margin(0.3, 0, 0.3, 0, "cm")),
        axis.title.y = element_text(face = "bold", size = 15, margin = margin(0, 1, 0, 1, "cm")),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13),
        legend.position = "bottom",
        plot.title = element_text(margin = margin(1,0,0,0, "cm"), hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 15, hjust = 0.5, 
                                     margin = margin(0.5, 0, 1, 0, "cm")),
        plot.caption = element_text(size = 12, margin = margin(0.5, 0, 0, 0, "cm")),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  labs(title = "Average lemurs lifespan",
       subtitle = "by genus, sex and place of birth",
       x = "Lemurs Genus", y = "Years", 
       caption = "Source: Duke Lemur Center, Dataviz by Marco Arellano")

lemur_plot

ggsave("lemurplot.jpg", plot = lemur_plot, scale = 1.5, dpi = "retina")



