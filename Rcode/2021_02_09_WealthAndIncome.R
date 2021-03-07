library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(packcircles)
library(scales)
library(ggtext)
library(grid)
library(gridtext)

source("Rcode/DataVisFunctions.R")

# load in data
tuesdata <- tt_load('2021-02-09')
home_owner <- tuesdata$home_owner
race_wealth <- tuesdata$race_wealth
retirement <- tuesdata$retirement
student_debt <- tuesdata$student_debt

background_colour <- "#B5DCD3"


# clean the data wanted to plot

## circle packing
lifetime_earn_Male <- tuesdata$lifetime_earn %>% 
  filter(gender=="Men") %>% 
  arrange(race) %>% 
  mutate(race = gsub("Hispanic.*","Hispanic",race))

packing_male <- circleProgressiveLayout(lifetime_earn_Male$lifetime_earn, sizetype = 'area')
lifetime_earn_Male <- cbind(lifetime_earn_Male, packing_male)
lifetime_earn_Male_gg <- circleLayoutVertices(packing_male, npoints = 50)

lifetime_earn_Female <- tuesdata$lifetime_earn %>% 
  filter(gender=="Women")  %>% 
  arrange(race) %>% 
  mutate(race = gsub("Hispanic.*","Hispanic",race))

packing_female <- circleProgressiveLayout(lifetime_earn_Female$lifetime_earn, sizetype = 'area')
lifetime_earn_Female <- cbind(lifetime_earn_Female, packing_female)
lifetime_earn_Female_gg <- circleLayoutVertices(packing_female, npoints = 50)


## data for bar charts
retirement <- tuesdata$retirement %>% 
  filter(year >= 2000) %>% 
  mutate(year = factor(year, levels = unique(sort(year))))

student_debt <- tuesdata$student_debt %>% 
  filter(year >= 2000) %>% 
  mutate(year = factor(year, levels = unique(sort(year))))

home_owner <- tuesdata$home_owner %>% 
  filter(year %in% student_debt$year) %>% 
  mutate(year = factor(year, levels = unique(sort(year))))



# start plotting

## set a general theme for the bar plots
generalThemeBar <- theme(panel.grid.minor = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.grid.major.y = element_blank(),
                      axis.title.x= element_blank(),
                      axis.text.x= element_blank(),
                      axis.ticks.x= element_blank(),
                      axis.title.y= element_text(size = 13),
                      axis.text.y= element_text(size = 11),
                      legend.position = "none",
                      plot.background = element_rect(fill = background_colour, color = background_colour))

generalThemeCircle <- theme(panel.grid.minor = element_blank(),
                            panel.grid.major.x = element_blank(),
                            panel.grid.major.y = element_blank(),
                            axis.title.x= element_blank(),
                            axis.text.x= element_blank(),
                            axis.ticks.x= element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            legend.position = "none",
                            plot.background = element_rect(fill = background_colour, color = background_colour))

## make circle packing plots

lifetimeMenPlot <- ggplot() +
  geom_polygon(data = lifetime_earn_Male_gg, aes(x, y, group = id, fill = as.factor(id)), color = "black") +
  geom_text(data = lifetime_earn_Male, aes(x, y, label = paste(race,"\n$",formatC(lifetime_earn/1000, format="f", big.mark=",", digits=0),"k"))) +
  scale_size_continuous(range = c(1, 4)) +
  coord_equal()+
  generalThemeCircle
  
lifetimeWomenPlot <- ggplot()+
    geom_polygon(data = lifetime_earn_Female_gg, aes(x, y, group = id, fill = as.factor(id)), color = "black") +
    geom_text(data = lifetime_earn_Female, aes(x, y, label = paste(race,"\n$",formatC(lifetime_earn/1000, format="f", big.mark=",", digits=0),"k"))) +
    scale_size_continuous(range = c(1,4)) +
    coord_equal() +
    generalThemeCircle

## make bar charts

studentLoanPlot <- ggplot(data=student_debt,aes(x=year,y=loan_debt_pct, fill = race)) +
  geom_col(position = "dodge") +
  ylab("% Families with Student Loan Debt") +
  xlab("Year") +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1)
  ) +
  generalThemeBar

homeOwnerPlot <- ggplot(data=home_owner,aes(x=year,y=home_owner_pct, fill = race)) +
  geom_col(position = "dodge") +
  ylab("% Who Own Their Own Home") +
  xlab("Year") + 
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1)
  ) +
  generalThemeBar 

retirementPlot <- ggplot(data=retirement,aes(x=year,y=retirement, fill = race)) +
  geom_col(position = "dodge") +
  ylab("Retirement Funds") +
  xlab("Year") +
  scale_y_continuous(
    labels = scales::dollar
  ) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y= element_text(size = 13),
        axis.text.y= element_text(size = 11),
        axis.title.x= element_text(size = 13),
        axis.text.x= element_text(size = 12),
        legend.position = "none",
        plot.background = element_rect(fill = background_colour, colour = background_colour))




colplotsRect <- list(studentLoanPlot, homeOwnerPlot, retirementPlot)

colplotsRound <- roundPlots(colplotsRect)

circlePlotsRound <- roundPlots(list(lifetimeMenPlot, lifetimeWomenPlot))

## Title for the viz
title <- ggdraw() + 
  draw_label(
    "Wealth and income in the USA by race at different stages of life between 2000 and 2016",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 24
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(5, 7, 0, 7),
    plot.title = element_markdown(),
    plot.background = element_rect(fill = "#89BCB0", color = "#89BCB0")
  )


# set out the layout for the plots

circPlots <- plot_grid(circlePlotsRound[[1]], circlePlotsRound[[2]], align = "h", ncol=3, rel_widths = c(1,1,1.5))
circPlots <- plot_grid(NULL,circPlots, align = "v",ncol = 1, rel_heights = c(0.1,1))

colPlots <- plot_grid(NULL, colplotsRound[[1]],NULL, colplotsRound[[2]],NULL, colplotsRound[[3]], NULL,
          align = "v",
          ncol = 1,
          rel_heights = c(0.05,1,0.05,1,0.05,1,0.05))

# create plot

fullViz <- plot_grid(NULL,
                     title,
                     NULL,
                     circPlots,
                     colPlots,
                     align = "v",
                     ncol = 1,
                     rel_heights = c(0.05,0.05,0.05,0.8,3)) +
  theme(plot.background = element_rect(fill="#89BCB0", color = "#89BCB0")) 


# create text
graphInfoText <- gridtext::textbox_grob("Since the turn of the millenium, <b style = 'color:red;'> Black </b> people and <b style = 'color:green;'> Hispanic </b> people
                                have seen the gap in their financial situation compared to <b style = 'color:blue;'> White </b> people continue to widen.
                                \n On a generational scale, older generations thrive more than younger ones, as seen by an increase in retirement funds whilst
                                stuent loans rise and the percentage of those are own their own homes fall.
                                \n Although the increase in the proportion of families with student loan debt, could also be associated with an increase in the number of
                                people enrolling in further education, rather than simply having more limited funds to pay the debt.
                                \n On a gender basis, <b style = 'color:red;'> Black </b> men will earn 50% more over their lifetime than <b style = 'color:red;'> Black </b> women,
                                whilst <b style = 'color:green;'> Hispanic </b> and <b style = 'color:blue;'> White </b> men will earn c.a. 80% more than women of the same race.",
                                x=0.75,
                                y=0.85,
                                width = 0.45)

maleCircleText <- gridtext::textbox_grob("<- Lifetime earnings of men by race",
                                         x = 0.28,
                                         y = 0.88,
                                         width = 0.1)

femaleCircleText <- gridtext::textbox_grob("Lifetime earnings of women by race ->",
                                         x = 0.31,
                                         y = 0.8,
                                         width = 0.1)

# view plot
fullViz +
  draw_grob(graphInfoText) +
  draw_grob(maleCircleText) +
  draw_grob(femaleCircleText)


#save plot
ggsave("plots/2021_02_09_WealthAndIncome.png", width = 16, height = 14)



