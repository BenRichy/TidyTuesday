# Tidy tuesday for 2022_03_22

library(tidytuesdayR)
library(tidyverse)


source("Rcode/0_DataVisFunctions.R")

# non-scientific mode
options(scipen=999)

# load data
tuesdata <- tidytuesdayR::tt_load('2022-03-22')