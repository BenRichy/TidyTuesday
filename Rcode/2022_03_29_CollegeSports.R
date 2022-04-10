# Tidy tuesday for 2022_04_05

library(tidytuesdayR)
library(tidyverse)
library(ggtext)


source("Rcode/0_DataVisFunctions.R")

# non-scientific mode
options(scipen=999)

# load data
tuesdata <- tidytuesdayR::tt_load('2022-03-29')
sports_data <- tuesdata$sports

# define constants
year_filter <- 2019
male_colour <- "#14C9EC"
female_colour <- "yellow"
bg_colour <- "#696464"
text_colour <- "white"

# shape data
sports_data_ToUse <- sports_data %>% 
  select(sports,
         year,
         exp_men,
         exp_women,
         sum_partic_men,
         sum_partic_women) %>% 
  group_by(sports,
           year) %>% 
  summarise(total_partic_men = sum(sum_partic_men),
            total_partic_women = sum(sum_partic_women),
            total_exp_men = sum(exp_men, na.rm = TRUE),
            total_exp_women = sum(exp_women, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(exp_per_partic_men = total_exp_men/total_partic_men,
         exp_per_partic_women = total_exp_women/total_partic_women) %>% 
  filter(year == year_filter) %>% 
  select(sports,
         exp_per_partic_men,
         exp_per_partic_women) %>% 
  mutate(exp_per_partic_men = case_when(is.nan(exp_per_partic_men)~0,
                                        TRUE~exp_per_partic_men),
         exp_per_partic_women = case_when(is.nan(exp_per_partic_women)~0,
                                        TRUE~exp_per_partic_women),
         male_exp_per_partic = exp_per_partic_men,
         female_exp_per_partic = exp_per_partic_women,
         exp_per_partic_women = exp_per_partic_women * -1,
         sports = as.factor(sports)) %>% 
  pivot_longer(exp_per_partic_men:exp_per_partic_women,
               names_to = "gender",
               values_to = "exp_per_partic") %>% 
  mutate(gender = case_when(gender == "exp_per_partic_men" ~ "men",
                            gender == "exp_per_partic_women" ~ "women")) 


# code to c place names next to the graph (geom_text code) adapted from https://github.com/jvieroe/TidyTuesday/blob/main/2022/week_13/code_sports.R
ggplot(data = sports_data_ToUse, aes(x = exp_per_partic, y = fct_reorder(sports, female_exp_per_partic))) +
  geom_segment(data = sports_data_ToUse,
               aes(x = exp_per_partic,
                   xend = 0,
                   y = reorder(sports, female_exp_per_partic),
                   yend = reorder(sports, female_exp_per_partic),
                   color = gender),
               alpha = 0.7) +
  geom_point(aes(colour=gender), size=4, alpha = 0.7) +
  scale_x_continuous(labels = c("$60,000","$40,000","$20,000","$0","$20,000","$40,000","$60,000"),
                     n.breaks = 7,
                     limits = c(-60000,60000)) +
  labs(x = "Expenditure per Participant",
       y = "Sport",
       title = paste0("College Sport Spend per Participant in ", year_filter),
       subtitle = paste0("US College Sport Expenditure by the education institution per <span style='color:",male_colour,"'>Male</span> participant versus <span style='color:",female_colour,"'>Female</span> participants.")) +
  geom_text(data = sports_data_ToUse %>% 
              filter(gender == "women"),
            aes(y = reorder(sports,
                            female_exp_per_partic),
                x = -female_exp_per_partic,
                label = sports),
            color = text_colour,
            hjust = 1,
            size = 3,
            nudge_y = 0,
            nudge_x = -3000) +
  annotate("text",
           x = 46000,
           y = 15,
           label = "Male",
           colour = male_colour,
           size = 16,
           alpha = 0.7) +
  annotate("text",
           x = -46000,
           y = 15,
           label = "Female",
           colour = female_colour,
           size = 16,
           alpha = 0.7) +
  scale_colour_manual(values = c(male_colour, female_colour))+
  theme(plot.background = element_rect(fill = bg_colour),
        panel.background = element_rect(fill = bg_colour),
        axis.title.y= element_text(colour = text_colour,
                                   size = 14),
        axis.text.y= element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x= element_text(colour = text_colour,
                                   size = 14),
        axis.text.x= element_text(colour = text_colour),
        plot.title = element_text(colour = text_colour,
                                  size = 18),
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey",
                                          linetype = 2),
        plot.subtitle = ggtext::element_markdown(color = text_colour,
                                                 size = 12,
                                                 margin = ggplot2::margin(b = 10,
                                                                          unit = "pt")))


ggsave("plots/2022_03_29_CollegeSports.png", height = 7, width =12)
  