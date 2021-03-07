
# Functions for tidying plots


## make charts have rounded background (https://stackoverflow.com/questions/48199791/rounded-corners-in-ggplot2)

roundPlots <- function(plotListInput){
  plotListOutput <- list()
  for (i in 1:length(plotListInput)) {
    g <- ggplotGrob(plotListInput[[i]])
    bg <- g$grobs[[1]]
    round_bg <- roundrectGrob(x=bg$x, y=bg$y, width=bg$width, height=bg$height,
                              r=unit(0.1, "snpc"),
                              just=bg$just, name=bg$name, gp=bg$gp, vp=bg$vp)
    g$grobs[[1]] <- round_bg
    plotListOutput[[i]]<-g
  }
  return(plotListOutput)
}