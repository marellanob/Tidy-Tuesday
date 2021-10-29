library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(magrittr)
library(stringr)
library(forcats)
library(predictrace)
library(ggplot2)
library(scales)
library(ggtext)
library(glue)
library(showtext)
library(patchwork)


#DATA
emmy_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')

kaggle_emmy_raw <-
  read_csv("https://raw.githubusercontent.com/NearAndDistant/data_science_with_r/main/Tidy_Tuesday/2021/Week%2039%20%3A%20Emmy%20Awards/the_emmy_awards.csv")


# WHICH BIG CATEGORY HAS MORE GENDER DISTRIBUTION? 

tidy_emmy <- emmy_raw %>% 
  filter(year > 2019 ) %>% 
  mutate(category = str_remove(category, "\\ -.*"),
         category = str_to_title(category)) %>% 
  distinct() %>% 
  select(year, category, production, type)


kaggle_emmy <- kaggle_emmy_raw %>% 
  filter(year >= 2000) %>% 
  rename("title" = "nominee", "production" = "staff", "type" = "win", "distributor" = "company") %>% 
  mutate(type = ifelse(type == TRUE, "Winner", "Nominee"),
         category = str_to_title(category)) %>% 
  distinct() %>% 
  select(year, category, production, type)
  

emmys <- rbind(tidy_emmy , kaggle_emmy) %>%  
  drop_na(production)


# CLEANING PRIMETIME EMMYS NO GENDER CATEGORIES 

# Producing 

producing <- c(
  "Outstanding Comedy Series",
  "Outstanding Drama Series",
  "Outstanding Miniseries",
  "Outstanding Limited",
  "Outstanding Reality | Competition Program",
  "Outstanding Reality-Competition Program",
  "Outstanding Competition Program",
  "Outstanding Television Movie",
  "Outstanding Variety")

directing <- c(
  "Outstanding Directing For A Comedy Series",
  "Outstanding Directing For A Drama Series",
  "Outstanding Directing For A Miniseries",
  "Outstanding Directing For A Limited",
  "Outstanding Directing For A Variety")

writing <- c(
  "Outstanding Writing For A Comedy Series",
  "Outstanding Writing For A Drama Series",
  "Outstanding Writing For A Miniseries",
  "Outstanding Writing For A Limited",
  "Outstanding Writing For A Variety")

emmy_PrimeCat <- emmys %>%
  mutate(prime_category = 
           case_when(
             str_detect(category, paste(producing, collapse = "|")) ~ "Producing",
             str_detect(category, paste(directing, collapse = "|")) ~ "Directing",
             str_detect(category, paste(writing, collapse = "|")) ~ "Writing",
             TRUE ~ category)) %>%
  filter(prime_category %in% c("Producing", "Directing", "Writing")) %>% 
  mutate(name = str_remove(production, pattern = "\\,.*"),
         name = str_remove(name, pattern = "\\ .*"))

# CREATING GENDER FUNCTION TO PREDICT THE WINNER OR NOMINEE GENDER

gender <- function(data){
  predict_gender(data$name, probability = FALSE) %>% 
    select(-c(name,match_name)) %>%
    pull(likely_gender)
}

emmy_gender <- emmy_PrimeCat %>% 
  mutate(gender = gender(emmy_PrimeCat),
         gender = replace_na(gender, "notDefined"))

# IDENTIFY NAMES NOT FOUND FOR THE GENDER FUNCTION

emmy_gender %>% 
  count(gender)


# GOOGLING THE NAMES THAT ARE NOT IDENTIFY BY THE GENDER FUNCTION AND REPLACE THE GENDER 

emmy_long <- emmy_gender %>% 
  mutate(gender = case_when(
    name == "Yahlin" ~ "female", name == "Shantira" ~ "female", name == "Shenovia" ~ "female", name == "Opus" ~ "male", name == "Sudi" ~ "female",
    name == "Our" ~ "female", name == "Zhubin" ~ "male", name == "E." ~ "male", name == "Caleeb" ~ "male", name == "Shihan" ~ "female",
    name == "Raphaël" ~ "male", name == "Pavun" ~ "male", name == "Tanase" ~ "male", name == "Mekita" ~ "female", name == "Andrij" ~ "male", 
    name == "Dime" ~ "female", name == "Sono" ~ "female", name == "Nige" ~ "male", name == "X" ~ "female", name == "Alisar" ~ "female", 
    name == "Beyoncé" ~ "female", name == "Huw" ~ "male", name == "Jenji" ~ "female", name == "Jaffe" ~ "male", name == "D.V." ~ "male", 
    name == "Tig" ~ "female", name == "Semi" ~ "female", name == "Pierre-Ange" ~ "male", name == "Jehane" ~ "female", name == "LtCol" ~ "male", 
    name == "Dearbhla" ~ "female", name == "Full" ~ "female", str_detect(name, "Tim") ~ "male", name == "Moisés" ~ "male", name == "Keegan-Michael" ~ "male",
    name == "Raamla" ~ "female", name == "Dahvi" ~ "female", name == "Boo" ~ "female", name == "T." ~ "male", name == "Bjoern" ~ "male", 
    name == "Zoë" ~ "female", name == "Cat" ~ "female", name == "RuPaul" ~ "male", name == "Thairin" ~ "male", name == "Stijn" ~ "male",
    name == "Richleigh" ~ "male", name == "Lin-Manuel" ~ "male", name == "M." ~ "female", name == "Boltenko" ~ "male", name == "Kahane" ~ "female",
    TRUE ~ gender)) %>% 
  mutate(gender = factor(gender, levels = c("female", "male")))

# CATEGORIES PLOT DATASET 

emmy_CatNoms_long <- emmy_long %>% 
  group_by( year, prime_category, gender) %>% 
  summarise( n = n())

emmy_CatNoms_Wide <-  emmy_CatNoms_long %>%
  pivot_wider(names_from = c(gender, prime_category), values_from = n) %>% 
  replace(is.na(.), 0) %>%
  mutate(Producing_total = sum(female_Producing, male_Producing, na.rm = TRUE),
         Directing_total = sum(female_Directing, male_Directing, na.rm = TRUE),
         Writing_total = sum(female_Writing, male_Writing, na.rm = TRUE))

emmy_CatNoms_total <- emmy_CatNoms_Wide %>% 
  pivot_longer(cols = Producing_total:Writing_total, 
               names_to = "prime_category", values_to = "count_cat") %>% 
  select(year, prime_category, count_cat) %>% 
  mutate(prime_category = str_remove(prime_category, pattern = "\\_.*"))

emmy_CatNoms_full <- emmy_CatNoms_long %>% 
  full_join(emmy_CatNoms_total, by = c("year", "prime_category")) %>% 
  mutate(prop_cat = round(n/count_cat, 2),
         prime_category = factor(prime_category, levels = c("Producing", "Directing", "Writing"))) %>% 
  drop_na()

#  WINS CATEGORIES PLOT DATASET 

emmy_CatWins_long<- emmy_long %>% 
  filter(type == "Winner") %>% 
  group_by( year, prime_category, gender) %>% 
  summarise( n = n())

emmy_CatWins_Wide <-  emmy_CatWins_long %>%
  pivot_wider(names_from = c(gender, prime_category), values_from = n) %>% 
  replace(is.na(.), 0) %>%
  mutate(Producing_total = sum(female_Producing, male_Producing, na.rm = TRUE),
         Directing_total = sum(female_Directing, male_Directing, na.rm = TRUE),
         Writing_total = sum(female_Writing, male_Writing, na.rm = TRUE))

emmy_CatWins_total <- emmy_CatWins_Wide %>% 
  pivot_longer(cols = Producing_total:Writing_total, 
               names_to = "prime_category", values_to = "count_cat") %>% 
  select(year, prime_category, count_cat) %>% 
  mutate(prime_category = str_remove(prime_category, pattern = "\\_.*"))

emmy_CatWins_full <- emmy_CatWins_long %>% 
  full_join(emmy_CatWins_total, by = c("year", "prime_category")) %>% 
  complete(year, prime_category, gender, fill = list(n = 0)) %>% 
  mutate(prop_cat = round(n/count_cat, 2),
         prime_category = factor(prime_category, levels = c("Producing", "Directing", "Writing"))) %>% 
  replace(is.na(.), 0)
  

# PLOTTING

# ADDIND FONTS

font_add(family = "futura regular", regular = "Futura.ttc")
showtext_auto()

font_add(family = "futura bold", regular = "Futura Bold font.ttf")
showtext_auto()

# CREATING COLORS VALUES AND COLORS LABELS VECTORS

cols <- c("female" = "#ba7a8d", "male" = "#3940bb")
cols_labs <- c("female" = "Female", "male" = "Male")

# CREATING FACET LABELS VECTOR

labs_emmy_cat <- c("Producing" = "Outstanding Producing", "Directing" = "Outstanding Directing", "Writing" = "Outstanding Writing")

# LET'S PLOTTING!

# NOMINEES PLOT
emmys_catnoms_plot <- emmy_CatNoms_full %>%
  ggplot(aes(year, prop_cat, fill = fct_reorder(gender, prop_cat))) +
  geom_area(show.legend = TRUE) +
  scale_x_continuous(breaks = seq(2000, 2021, 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, 0.25)) +
  scale_fill_manual(values = cols, labels = cols_labs, name = "Gender:") +
  coord_cartesian(expand = FALSE) + 
  theme(axis.title.x = element_text(face = "bold", margin = margin(0.3, 0, 0, 0, "cm")),
        axis.text.x = element_text(face = "bold", size = 10, margin = margin(0.3, 0, 0, 0, "cm")),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(face = "bold", margin = margin(0, 0.1, 0, 0, "cm")),
        axis.text.y = element_text(face = "bold", size = 10),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "beige", color = "beige"),
        panel.spacing = unit(0.5, "cm"),
        strip.text = element_text(family = "futura bold", face = "bold", size = 14),
        strip.background = element_blank(),
        plot.title = element_text(family = "futura bold", face = "bold", size = 16)) +
  facet_wrap(~ prime_category, ncol = 1, labeller = labeller(prime_category = labs_emmy_cat)) +
  labs(title = "Nominees",
       y = "Gender Distribution (%)")

# WINNERS PLOT

emmys_catwins_plot <- emmy_CatWins_full %>%
  ggplot(aes(year, prop_cat, fill = fct_reorder(gender, prop_cat))) +
  geom_area(show.legend = FALSE) +
  scale_x_continuous(breaks = seq(2000, 2021, 5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, 0.25)) +
  scale_fill_manual(values = cols)+
  coord_cartesian(expand = FALSE) + 
  theme(axis.title.x = element_text(face = "bold", margin = margin(0.3, 0, 0, 0, "cm")),
        axis.text.x = element_text(face = "bold", size = 10, margin = margin(0.3, 0, 0, 0, "cm")),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "beige", color = "beige"),
        panel.spacing = unit(0.5, "cm"),
        strip.text = element_text(family = "futura bold", face = "bold", size = 14),
        strip.background = element_blank(),
        plot.title = element_text(family = "futura bold", face = "bold", size = 16)) +
  facet_wrap(vars(prime_category), ncol = 1, labeller = labeller(prime_category = labs_emmy_cat)) +
  labs(title = "Winners",
       y = "Gender (%)")


# ANALYSIS TEXT 

analysis <- glue("Over the past 21 years, the total number of Emmy Award nominees in the top 3 categories: Directing, Producing, and Writing corresponds to 74% (2,410) men and 26% (827) women. During the 2018-2021 period, the range of the percentage of women in the 3 main categories is 23% to 37% of all nominees; showing a marginal increase compared to previous years. Otherwise, of the total winners in these 3 categories, in the last 21 years, corresponds to 80% (451) men and 20% (116) women. As with the nominees, in the 2018-2021 period, the range for the percentage of women Emmy Award winners has increased from previous years where the range for this period is 23% to 34%.<br><br>

Overall, Outstanding Directing is the category with the lowest representation of women, with only 14% of all nominees being women, compared to 31% of Outstanding Producing nominees and 21% of Outstanding Writing. An improvement is observed in the last three years for Outstanding Directing, with women nominated between 21% and 34% of the annual nominees. Similarly, Outstanding Directing is the category with the least women representation of all Emmy Award winners, with just 11%, in contrast to 25% of winners for Outstanding Producing and 17% of Outstanding Writing. Like the nominees, there is an improvement in the last three years for outstanding Directing, with women winning between 17% to 40% of the annual winners.")

# COMBINE PLOTS 

plot <- (emmys_catnoms_plot + emmys_catwins_plot) + 
  plot_layout(guides = "collect", ncol = 2) +
  plot_annotation(
    title = "Emmys Awards: Gender Distribution for Top 3 Categories in the last two decades",
    subtitle = str_wrap(analysis, 100),
    caption = "First name of nominees was used to predict gender with the R package ‘predictrace’, which matches names with names in the SSA data (Social Security Administration data). \n 
    Data Source: 'Emmys.com' and 'Kaggle', Dataviz by Marco Arellano",
    theme = theme(plot.title = element_text(family = "futura bold", size = 25, 
                                            margin = margin(1, 1, 0.3, 0.5, "cm"),
                                            hjust = 0.11),
                  plot.subtitle = element_textbox(family = "futura regular", size = 13, 
                                                  hjust = 1, halign = 0, 
                                                  width = unit(1, "npc"), margin = margin(0.3, 0, 0, 1.5, "cm")),
                  plot.caption = element_text(size = 10, margin = margin(0.5, 0, 0.5, 0, "cm")))) &
  theme(text = element_text(family = "futura regular"),
        plot.background = element_rect(fill = "beige", colour = "beige"),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 11),
        plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

png(file="emmy.png", res=600, width=9600, height=6600, pointsize=10,
    type="quartz", antialias="none")

plot

dev.off()
