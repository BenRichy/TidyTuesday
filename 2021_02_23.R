library(tidytuesdayR)



# load in data
tuesdata <- tt_load('2021-02-23')
earn <- tuesdata$earn
employed <- tuesdata$employed

earn$sex %>%  unique()
earn$race %>%  unique()
earn$ethnic_origin %>%  unique()
earn$age %>% unique()
earn$year %>%  unique()
earn$quarter %>% unique()

employed$industry %>%  unique()
employed$major_occupation %>%  unique()
employed$minor_occupation %>%  unique()
employed$race_gender %>% unique()
employed$year %>%  unique()

