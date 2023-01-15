# Tidy tuesday for 2022_03_22

library(tidytuesdayR)
library(tidyverse)
library(ggwordcloud)
library(maps)
library(wordcloud2)

source("Rcode/0_DataVisFunctions.R")

# non-scientific mode
options(scipen=999)

# load data
tuesdata <- tidytuesdayR::tt_load('2022-03-22')
babynames <- tuesdata$babynames
years <- unique(babynames$year)

set.seed(18)

for (i in 2005:max(years)){
nameData <- tuesdata$babynames %>% 
  filter(year == i,
         sex == "F") %>%
  slice_max(order_by = prop, n = 1000) %>% 
  mutate(angle = 22 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))



ggplot(
  nameData,
  aes(
    label = name, size = prop,
    color = factor(sample.int(10, nrow(nameData), replace = TRUE)),
    angle = angle
  )
) +
  geom_text_wordcloud_area(mask = png::readPNG("img/2022_03_22_BabyNames/USA_map.png"),
                           rm_outside = TRUE) +
  scale_size_area(max_size = 20) 

ggsave(paste0("img/2022_03_22_BabyNames/plots/BabyNames_",i,".png"),
       width = 6,
       height = 4)

}

year_to_use <- 2017
fp_copy <- paste0("img/2022_03_22_BabyNames/plots/BabyNames_",year_to_use,".png")
fp_paste <- "plots/2022_03_22_BabyNames.png"

file.copy(fp_copy, fp_paste, overwrite = TRUE)
