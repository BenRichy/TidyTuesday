library(tidytuesdayR)
library(tidyverse)
library(maps)
library(RColorBrewer)
library(ggpolypath)
library(scales)
library(cowplot)
library(magick)
library(gifski)


# non-scientific mode
options(scipen=999)

# load data
tuesdata <- tidytuesdayR::tt_load('2021-04-06')
brazilLossData <- tuesdata$brazil_loss



# extract coordinates for Brazil
brazilMap <- map('world','Brazil', exact = TRUE, bg="white")
brazilMap_Coord <- data.frame(brazilMap$x, brazilMap$y) %>% 
  na.omit()


# get min and max x and y bounds to scale the deforestation data and to plot later
brazilMap_xMin <- min(brazilMap$x, na.rm = TRUE)
brazilMap_xMax <- max(brazilMap$x, na.rm = TRUE)
brazilMap_yMin <- min(brazilMap$y, na.rm = TRUE)
brazilMap_yMax <- max(brazilMap$y, na.rm = TRUE)


# calculate deforestation total of each row for defining the y limit for each year
brazilLossDataNorm <- brazilLossData %>% 
  mutate(TotalLoss = rowSums(.[4:14]),
         yMaxData = max(TotalLoss),
         yMaxGraph = abs(brazilMap_yMax) + abs(brazilMap_yMin))


# scale deforestation data so that it is within the y scale defined by the min and max latitudes of brazil
# add the y limit of one column to the value of the previous one to create a pseudo-stacked bar chart
for (i in 1:nrow(brazilLossDataNorm)) {
  brazilLossDataNorm[i,4] <- (brazilLossDataNorm[i,4]/brazilLossDataNorm[i,16])*brazilLossDataNorm[i,17]
  for (j in 5:14) {
    brazilLossDataNorm[i,j] <- ((brazilLossDataNorm[i,j]/brazilLossDataNorm[i,16])*brazilLossDataNorm[i,17]) +brazilLossDataNorm[i,j-1] 
  }
}

# create dataframe to assign colours to each column title
# also set the dataframe up so it can produce a legend that can be pasted over the top of the final graph
columnColours <- data.frame(column = colnames(brazilLossDataNorm[4:14]),
                            fillCol = brewer.pal(n = 11, name = "Paired"),
                            x = seq(1,11),
                            y = seq(1,11),
                            plotName = c(
                              "Commercial Crops",
                              "Flooding due to Dams",
                              "Natural Disturbances",
                              "Pasture",
                              "Selective Logging",
                              "Fire",
                              "Mining",
                              "Other Infrastructure",
                              "Roads",
                              "Tree Plantation including Palm",
                              "Small-Scale Clearing"
                            ))


# create list for rectangle objects to be pasted into
rectOutputs <- rep(list(rep(list(list()),11)),nrow(brazilLossDataNorm))


# create rectangle objects from the data such that they can be stacked like a bar chart
# adjust y limits so that the boxes are within the brazil country cutout
for (i in 1:nrow(brazilLossDataNorm)){
  yAxisReset <- abs(brazilMap_yMin)
  for (j in 4:14) {
    if(j==4){
      
      xmin = as.numeric(brazilMap_xMin)
      xmax = as.numeric(brazilMap_xMax)
      ymin = as.numeric(0-yAxisReset)
      ymax = as.numeric(brazilLossDataNorm[i,j]-yAxisReset)
      
      rectOutputs[[i]][[j-3]] <- geom_rect(aes_string(xmin = xmin,
                                               xmax = xmax,
                                               ymin = ymin,
                                               ymax = ymax),
                                           fill = columnColours[j-3,2])

      
    }else {
      
      xmin = as.numeric(brazilMap_xMin)
      xmax = as.numeric(brazilMap_xMax)
      ymin = as.numeric(brazilLossDataNorm[i,j-1]-yAxisReset)
      ymax = as.numeric(brazilLossDataNorm[i,j]-yAxisReset)
      
    rectOutputs[[i]][[j-3]] <- geom_rect(aes_string(xmin = xmin,
                                             xmax = xmax,
                                             ymin = ymin,
                                             ymax = ymax),
                                     fill = columnColours[j-3,2])
    }
  }
  
}



# make the country outline into a hole
# https://stackoverflow.com/questions/51715907/how-to-apply-a-clipping-mask-to-geom-in-a-ggplot
# create box around the limits of brazil
brazilMap_bounds <- data.frame(brazilMap.x=c(brazilMap_xMin, brazilMap_xMax,brazilMap_xMax, brazilMap_xMin),
                               brazilMap.y=c(brazilMap_yMin, brazilMap_yMin, brazilMap_yMax, brazilMap_yMax),
                               hole = rep(FALSE,4),
                               group = rep("1",4))


# join graph bounds to country outline coordinates
brazilMap_all_coords <- brazilMap_Coord %>% 
  mutate(hole = TRUE,
         group = "2") %>% 
  rbind(brazilMap_bounds)


# create tick marks and tick labels for y axis, need to adjust based on coordinates of brazil
brazilMapY_ticks <- c(seq(0, 4000000, 500000))
brazilMapY_values <- (brazilMapY_ticks/max(brazilLossDataNorm$TotalLoss))*(abs(brazilMap_yMax) + abs(brazilMap_yMin)) - abs(brazilMap_yMin)


# plot graph using dummy data to create a legend that can be pasted on the final graph
plotLegend <- ggplot(data = columnColours, aes(x = x, y = y, fill = plotName)) +
  geom_col() +
  scale_fill_manual(values = columnColours$fillCol,
                    labels = columnColours$plotName,
                    guide = guide_legend(reverse = TRUE)) +
  labs(fill = "Cause of Deforestation") +
  theme(legend.background = element_rect(colour = "black"),
        legend.margin = margin(4,8,4,4))

# pull out the legend from the dummy plot
plotLegend <- as_grob(get_legend(plotLegend))





# create series of graphs and save them
# layer boxes on top of one another, then put the cut out of Brazil over the top
# formatting
# add the legend to the plot
# save the plot


for (i in 1:13) {

BrazilPlot1 <- ggplot() +
  rectOutputs[[i]][[1]] +
  rectOutputs[[i]][[2]] +
  rectOutputs[[i]][[3]] +
  rectOutputs[[i]][[4]] +
  rectOutputs[[i]][[5]] +
  rectOutputs[[i]][[6]] +
  rectOutputs[[i]][[7]] +
  rectOutputs[[i]][[8]] +
  rectOutputs[[i]][[9]] +
  rectOutputs[[i]][[10]] +
  rectOutputs[[i]][[11]] +
  geom_polypath(data = brazilMap_all_coords,
                aes(x=brazilMap.x, y = brazilMap.y, group = group),
                colour = "#EFE2BA", fill = "#EFE2BA") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0),
                     breaks = brazilMapY_values,
                     labels = comma(brazilMapY_ticks)) +
  labs(title = "Largest causes of deforestation in the Brazilian Amazon 2001 - 2013",
       y = "Area of Brazilian Amazon lost (hectares)",
       caption = "Data obtained from https://ourworldindata.org/drivers-of-deforestation and originates from 
       Tyukavina, A. et al., (2017). Types and rates of forest disturbance in Brazilian Legal Amazon, 2000–2013. Science Advances, 3, e1601047") +
  theme(axis.title.x= element_blank(),
        axis.text.x= element_blank(),
        axis.ticks.x.bottom = element_blank(),
        axis.title.y= element_text(size = 13,
                                   margin = margin(r=10)),
        axis.text.y= element_text(size = 12),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 18),
        plot.background = element_rect(fill = "#EADDB4")) 


ggdraw(BrazilPlot1) + 
  draw_grob(plotLegend,
            x = -0.25,
            y=-0.2) +
  draw_text(brazilLossDataNorm$year[i],
            x = 0.85,
            y = 0.87,
            size = 32)

ggsave(paste0("img/2021_04_06_GlobalDeforestation/BrazilPlot_",brazilLossDataNorm$year[i],".png"),
       width = 10,
       height = 10)

}


# make a gif out of the graphs

## list file names and read in
imgs <- list.files("img/2021_04_06_GlobalDeforestation", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

# save gif
image_write_gif(img_joined, path = "plots/2021_04_06_GlobalDeforestation.gif", delay = 3/2)
