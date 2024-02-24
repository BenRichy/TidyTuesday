#TidyTuesday for 2024_02_20

library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(scales)
library(gghighlight)


source("Rcode/0_DataVisFunctions.R")

# non-scientific mode
options(scipen=999)

#TidyTuesday Date
tt_date <- '2024-02-20'

# load data
tuesdata <- tidytuesdayR::tt_load(tt_date)

# set globals
colour_bg <- "#182c7d"
colour_text <- "white"
colour_high <- "yellow"
colour_low <- "darkgrey"
threshold <- 30000
  

#Pull data out
GrantRawData <- tuesdata$isc_grants |> 
  filter(funded > 0) |> 
  mutate(total_average = mean(funded)) |> 
  #give each row a different row id
  mutate(row = row_number(),
         above_total_ave = case_when(funded > total_average ~ "above",
                                    TRUE ~ "below"),
         above_threshold = case_when(funded > threshold ~ "above",
                                     TRUE ~ "below")) 

# plot
ggplot(data = GrantRawData) +
  geom_col(aes(x=row, y=funded, fill = above_threshold, alpha = above_threshold)) +
  facet_grid(~ year, 
             scales = "free_x",
             space = "free_x",
             switch = "x") + # move the facet labels to the bottom
  scale_fill_manual(values = c("above" = colour_high,
                               "below" = colour_low)) + #set the colours based on if funding is above a threshold
  scale_alpha_manual(values = c("above" = 1,
                                "below" = 0.5)) + #set the alpha based on if funding is above a threshold
  scale_y_continuous(labels = dollar, #use dollar formatting
                     expand=c(0,1)) + #move x axis labels up
  labs(x = "Year",
       y = "Grant Funding",
       title = "R Consortium Grant funding",
       subtitle = sprintf("Recipients of the R Consortium Infrastructure Steering Committee (ISC) Grant Program.
       Those above <span style='color: %s'>$%s</span> highlighted.", colour_high, format(threshold, big.mark=","))) +
  theme(panel.spacing = unit(0, units = "cm"), # removes space between panels
        panel.grid = element_blank(),
        strip.placement = "inside",
        strip.background = element_rect(fill = colour_bg,
                                        colour = colour_text),
        
        strip.text = element_text(colour = colour_text),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = colour_text),
        axis.title.y = element_text(color = colour_text),
        axis.title.x = element_text(color = colour_text),
        plot.title = element_text(colour = colour_text,
                                  hjust = 0.5),
        plot.subtitle = ggtext::element_markdown(color = colour_text, #need this to force colours
                                                 margin = ggplot2::margin(b = 10,
                                                                          unit = "pt")),
        legend.position = "none",
        plot.background = element_rect(fill = colour_bg),
        panel.background = element_rect(fill = colour_bg))

#save plot
ggsave("plots/2024_02_20_ISCGrant.png", height = 7, width =12)


        