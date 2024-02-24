#TidyTuseday for 2024_02_13

library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(fmsb)
library(janitor)


source("Rcode/0_DataVisFunctions.R")

# non-scientific mode
options(scipen=999)

#TidyTuesday Date
tt_date <- '2024-02-13'

# colours
colors_in=c("#428fc9", "#e83fe8")
colors_border=c("#29618c", "#a127a1")

# load data
tuesdata <- tidytuesdayR::tt_load(tt_date)

historicalSpend <- tuesdata$historical_spending
giftsAge <- tuesdata$gifts_age

#need to convert gifts_gender to data.frame 
#tibble won't keep the rownames once columns are deselected
giftsGender <- data.frame(tuesdata$gifts_gender)

rownames(giftsGender) <- giftsGender$Gender

giftsGender <- giftsGender |> 
  select(-c("Gender",
            "SpendingCelebrating"))

#create rows for min and max ranges
giftsGenderPlot <- rbind(rep(100,ncol(giftsGender)), 
                         rep(0,ncol(giftsGender)),
                         giftsGender)

#Add spaces where necessary for the colnames
giftsGenderCols <- snakecase::to_lower_camel_case(colnames(giftsGender))
giftsGenderCols <- gsub("([A-Z]){1}", " \\1", giftsGenderCols)
giftsGenderCols <- snakecase::to_title_case(giftsGenderCols)
#replace the current col names
colnames(giftsGenderPlot) <- giftsGenderCols
  


png("plots/2024_02_13_ValentinesConsumer.png", width=10, height=10, units="in", res=1200)
# radar chart for men vs women and gifts
par(bg = "#f7d4ef")

radarchart(giftsGenderPlot,
           axistype = 1,
           #custom polygon
           pcol=colors_border , pfcol=scales::alpha(colors_in,0.5) , plwd=2 , plty=1, 
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="darkgrey", caxislabels=seq(0,100,25), cglwd=0.8,
           #custom labels
           vlcex=1 )

# Add a legend
legend(x=0.8, y=1.2, legend = rownames(giftsGenderPlot[-c(1,2),]), bty = "n", pch=16 , col=colors_in , text.col = "grey2", cex=1, pt.cex=1)
#Add title
title(main = "Gifts Bought on Valentine's Day by Gender",
      sub = "Percentage of giftbuyers displayed.")

#save plot
dev.off()
