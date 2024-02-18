#0_TidyTuesdayTemplate

library(tidytuesdayR)
library(tidyverse)
library(ggtext)


source("Rcode/0_DataVisFunctions.R")

# non-scientific mode
options(scipen=999)

#TidyTuesday Date
tt_date <- '2022-03-29'

# load data
tuesdata <- tidytuesdayR::tt_load(tt_date)
