#TidyTuesday for 2024_02_06

library(tidytuesdayR)
library(tidyverse)
library(ggtext)
library(slideview)
library(rnaturalearth)
library(CoordinateCleaner)
library(htmltools)
library(svglite)


source("Rcode/0_DataVisFunctions.R")

# non-scientific mode
options(scipen=999)

#TidyTuesday Date
tt_date <- '2024-02-06'

# load data
tuesdata <- tidytuesdayR::tt_load(tt_date)
heritage <- tuesdata$heritage

#colours
colour_denmark <- "#6ecbf2"
colour_norway <- "#f26363"
colour_sweden <- "#fdf877"
colour_bg <- "#397fc0"

# get maps
europe <- ne_countries(scale = 50, returnclass = "sf", continent = "Europe")
norway <- ne_countries(scale = 50, returnclass = "sf", country = "Norway")
denmark <- ne_countries(scale = 50, returnclass = "sf", country = "Denmark")
sweden <- ne_countries(scale = 50, returnclass = "sf", country = "Sweden")

#get capital city coordinates
CapitalCoords <- countryref |> 
  filter(name %in% heritage$country,
         source == "geolocate") |> 
  select(name,
         capital.lon,
         capital.lat)

# join heritage data onto coordinates
HeritageDataCoord <- heritage |> 
  left_join(CapitalCoords,
            by = c("country" = "name"))


#create base map for the slideview
plot_base_geog <- ggplot() +
  geom_sf(data = europe) +
  geom_sf(data = norway, fill = colour_norway) +
  geom_sf(data = denmark, fill = colour_denmark) +
  geom_sf(data = sweden, fill = colour_sweden) +
  coord_sf(xlim = c(5, 30), ylim = c(53, 71)) +
  theme_void() +
  theme(plot.background = element_rect(fill = colour_bg),
        legend.position = "none")+
  labs(title = paste0("Number of UNESCO World Heritage \nSites in Nordic Countries"))


#create map of sites for each year
plot_geog_2004 <-  plot_base_geog +
  geom_point(
    data = HeritageDataCoord,
    aes(x = capital.lon,
        y = capital.lat,
        size = `2004`),
    fill = "black",
    shape = 21
  ) +
  scale_size(range = c(7, 15)) +
  labs(subtitle = paste0("2004")) 
  

plot_geog_2004


plot_geog_2022 <-  plot_base_geog +
  geom_point(
    data = HeritageDataCoord,
    aes(x = capital.lon,
        y = capital.lat,
        size = `2022`),
    fill = "grey50",
    shape = 21
  ) +
  scale_size(range = c(7, 15)) +
  labs(subtitle = paste0("2022")) +
  theme(plot.subtitle = element_text(hjust = 1))

plot_geog_2022


#Save plots as images
ggsave("img/2024_02_06_WorldHeritage/Heritage_2004.png", plot = plot_geog_2004, height = 7, width =12)
ggsave("img/2024_02_06_WorldHeritage/Heritage_2022.png", plot = plot_geog_2022, height = 7, width =12)

img_2004 <- "img/2024_02_06_WorldHeritage/Heritage_2004.png"
img_2022 <- "img/2024_02_06_WorldHeritage/Heritage_2022.png"



combinedImage <- slideView(img_2004, img_2022, label1 = "2004", label2 = "2022")


save_html(combinedImage, "plots/2024_02_06_WorldHeritage.html")

