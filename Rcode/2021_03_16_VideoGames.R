library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(jpeg)
library(ggpubr)

source("Rcode/0_DataVisFunctions.R")

# get rid of scientific notation

options(scipen=999)
tuesdata <- tidytuesdayR::tt_load('2021-03-16')$games

# formt data into a usable date format
panData <- tuesdata %>% 
  #filter(year %in% c(2019:2020)) %>% 
  mutate(monthNo = match(month, month.name),
         Month = ifelse(monthNo < 10,
                        format(ymd(paste0(year,"/",0,monthNo,"/01")),'%m-%Y'),
                        format(ymd(paste0(year,"/",monthNo,"/01")),'%m-%Y')))

# find games that appear in the top 100 by average player count over the desired date range
topGames <- panData %>% 
  arrange(desc(avg)) %>% 
  head(100) %>% 
  select(gamename) %>% 
  unique()


# group non-top games as 'other'
panData2 <- panData %>% 
  mutate(gamename2 = ifelse(gamename %in% topGames$gamename,
                            gamename,
                            "Other")) 

# sum together the average player count of all other games
dataToPlot <- panData2 %>% 
  select(gamename2, Month, avg) %>% 
  group_by(gamename2, Month) %>% 
  summarise(sumAvgPlayer = sum(avg)) %>% 
  arrange(Month) %>% 
  mutate(Month = my(Month))


# order games by order in which they were released
minMonths <- dataToPlot %>% 
  select(gamename2, Month) %>% 
  ungroup() %>% 
  group_by(gamename2) %>% 
  slice(1L) %>% 
  arrange(desc(Month)) %>% 
  mutate(gamename2 = factor(gamename2))

# apply the factor levels to the data to be plotted
dataToPlot$gamename2 <- factor(dataToPlot$gamename2, levels = minMonths$gamename2)

# -- plotting

backgroundImage <- jpeg::readJPEG("img/2021_03_16_SteamGames/white-smoke-steam-black-background.jpg")

ggplot(data = dataToPlot, aes(x = Month, y = sumAvgPlayer/1000, fill = gamename2)) +
  ggthemes::theme_economist() +
  background_image(backgroundImage) +
  geom_area(alpha = 0.8) +
  scale_fill_excel_new()+
  geom_vline(xintercept = as.numeric(as.Date('2020-3-19')),
             linetype = 2,
             size = 2,
             colour = "white") +
  geom_vline(xintercept = as.numeric(as.Date('2017-09-26')),
             linetype = 3,
             size = 2,
             colour = "white") +
  scale_y_continuous(
    labels = scales::comma) +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  labs(y = "Average monthly player count (thousands)",
       x = "Date",
       fill = NULL,
       title = "Average monthly playercount of games on Steam, 2012-2020",
       caption = "Data is based on data scraped by Michal Bogacz and posted on Kaggle at \n https://www.kaggle.com/michau96/popularity-of-games-on-steam. \n
       The data represents 1260 games of the 50,360 games on Steam (as of 04-04-2020)") +
  annotate("text", 
           x = as.Date('2019-02-01'),
           y = 4000,
           label = wrapper("The first state in the USA was locked down as a result of the Coronavirus pandemic on the 19th March 2020.", width = 30),
           colour = "yellow") +
  annotate("text", 
           x = as.Date('2016-06-01'),
           y = 3800,
           label = wrapper("PUBG is the first 'Battle Royale' style game released. Average player numbers peak in late 2017 before dwindling when players switch to Fortnite. Fortnite's Battle Royal mode is released on 26th September 2017.", width = 35),
           colour = "yellow") +
  theme(plot.background = element_rect(fill = "#313131"),
        axis.title.y= element_text(colour = "white",
                                   margin = margin(r=10)),
        axis.text.y= element_text(colour = "white"),
        axis.title.x= element_text(colour = "white",
                                   margin = margin(t=10)),
        axis.text.x= element_text(colour = "white"),
        legend.text = element_text(colour = "white"),
        plot.title = element_text(colour = "white",
                                  margin = margin(b=20)),
        plot.caption = element_text(colour = "#EDEBEB",
                                    hjust = 1,
                                    margin = margin(t=10)))


ggsave("plots/2021_03_16_SteamGames.png",height = 6,width=12)
  


