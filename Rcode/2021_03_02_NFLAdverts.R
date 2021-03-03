# load packages
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(cowplot)

#load wd
setwd("C:/Users/BR14/OneDrive - Ricardo Plc/Data-Science/R/TidyTuesday")
general_fp <- "C:/Users/BR14/OneDrive - Ricardo Plc/Data-Science/R/TidyTuesday"


# load in data
tuesdata <- tt_load('2021-03-02')
tuesdata <- tuesdata$youtube

# create df of unique brands in tt data
uniqueBrands <- data.frame(brand = brandYears$brand %>% 
                             unique())

# create path to image for each brand
uniqueBrands <- uniqueBrands %>% 
  arrange(`brand`) %>%
  mutate(`count_number`=1:nrow(uniqueBrands),
         image = paste0(general_fp,"/img/2021_03_02_NFLAdverts/",brand,".png"))
  

#create df of data, using brands and years, calculate ratio between likes and dislikes
brandYears <- tuesdata %>% 
  select(`year`,`brand`,`like_count`,`dislike_count`) %>% 
  arrange(`year`,`brand`) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(`year`,`brand`) %>% 
  summarise(like_count=sum(like_count),
            dislike_count=sum(dislike_count)) %>% 
  mutate(like_count = like_count +1,
         dislike_count = dislike_count+1,
         like_ratio = like_count/dislike_count)


# tell each row what position on the x axis to be on
brandYears <- brandYears %>% 
  left_join(uniqueBrands, by = 'brand')


# make plot
p <- brandYears %>%  ggplot(aes(y=`year`,x=`count_number`)) +
  geom_text(aes(label = "\U0001f3c8", col = "brown", size= brandYears$like_ratio*10)) +
  scale_y_reverse()+
  theme(panel.background = element_rect(fill = "#0A2034"),
        plot.background = element_rect(fill = "#0A2034"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(colour = "white", size = 13),
        plot.title = element_text(color = "white",size = 20),
        plot.subtitle = element_text(color = "white", size=13),
        legend.position = "none") +
  scale_x_discrete(name = NULL, labels = brandYears$image) +
  geom_segment(aes(x=0,xend=0, y=1999, yend = 2020.5), size=2, colour = '#E59213',
               arrow = arrow(length = unit(0.6,"cm"))) +
  labs(title="Super Bowl Adverts",
       subtitle="A measure of popularity by company over time 
       (youtube likes/dislikes)")

# add images to x axis
pimage <- axis_canvas(p, axis = 'x') + 
  draw_image(uniqueBrands[1,3], x = 0.5, scale = 0.5) +
  draw_image(uniqueBrands[2,3], x = 1.5, scale = 0.5) +
  draw_image(uniqueBrands[3,3], x = 2.5, scale = 0.5) +
  draw_image(uniqueBrands[4,3], x = 3.5, scale = 0.5) +
  draw_image(uniqueBrands[5,3], x = 4.5, scale = 0.5) +
  draw_image(uniqueBrands[6,3], x = 5.5, scale = 0.5) +
  draw_image(uniqueBrands[7,3], x = 6.5, scale = 0.5) +
  draw_image(uniqueBrands[8,3], x = 7.5, scale = 0.5) +
  draw_image(uniqueBrands[9,3], x = 8.5, scale = 0.5) +
  draw_image(uniqueBrands[10,3], x = 9.5, scale = 0.5)

ggdraw(insert_xaxis_grob(p, pimage, position = "left"))

ggsave("../plots/2021_03_02_NFLAdverts.png",height = 6,width=5)

  

