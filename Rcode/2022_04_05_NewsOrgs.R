# Tidy tuesday for 2022_04_05

library(tidytuesdayR)
library(tidyverse)
library(jpeg)
library(ggpubr)
library(magick)
library(grid)
library(ggplot2)
library(ggforce)
library(svglite)
library(rsvg)

source("Rcode/0_DataVisFunctions.R")

# non-scientific mode
options(scipen=999)

# load data
tuesdata <- tidytuesdayR::tt_load('2022-04-05')
new_orgs <- tuesdata$news_orgs


new_orgs[sapply(new_orgs, is.character)] <- lapply(new_orgs[sapply(new_orgs, is.character)],
                                                   as.factor)

new_orgs <- new_orgs %>% 
  select(year_founded,
         revenue_stream_largest) %>% 
  na.omit()





my_chart <- ggplot(data = new_orgs) +
  geom_bar(aes(x = year_founded,
               fill = revenue_stream_largest)) +
  labs(x="Year Founded",
       y = "# of News Corporations",
       title = "The Changing Ways that North American News Corporations have Made Money (1980-2020)",
       fill = "Largest Revenue Stream") + 
  xlim(1975, 2035) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        axis.title.y= element_text(colour = "white",
                                   size = 12),
        axis.text.y= element_text(colour = "white"),
        axis.title.x= element_text(colour = "white",
                                   size = 12),
        axis.text.x= element_text(colour = "white"),
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white"),
        plot.title = element_text(colour = "white",
                                  size = 18)) +
  guides(fill = guide_legend(ncol=2)) +
  annotate("text", 
           x = 1987,
           y = 10,
           label = wrapper("Noticeably more news corporations founded post 2000, likely due to the DotCom Bubble",
                           width = 20),
           colour = "blue") + 
  geom_segment(aes(x = 1991, y = 9, xend = 2000, yend = 5),
                                           arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", 
           x = 1998,
           y = 20,
           label = wrapper("Spike in new News corporations founded around the 2009 economic crash",
                           width = 20),
           colour = "blue") + 
  geom_segment(aes(x = 2002, y = 20, xend = 2008, yend = 20),
               arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", 
           x = 2031,
           y = 20,
           label = wrapper("Greater diversification of revenue streams in the newest corportations",
                           width = 20),
           colour = "blue") + 
  geom_segment(aes(x = 2027, y = 15, xend = 2021, yend = 5),
               arrow = arrow(length = unit(0.5, "cm")))



ggsave(my_chart, filename = "img/2022_04_05_NewsCorp/TransparentPlot.png",
       bg = "transparent",
       dpi = 300,units="px", width=4500, height=2080)
reload_graph <- image_read("img/2022_04_05_NewsCorp/TransparentPlot.png")

img <- image_read("img/2022_04_05_NewsCorp/BreakingNewsBG.jpg")
img <- image_resize(img, "7000x5000")




final_plot <- image_composite(img, reload_graph, offset = "-1100-1000", operator = "atop")

image_write(final_plot, path = "plots/2022_04_05_NewsCorps.png")
