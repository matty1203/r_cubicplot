library(ggplot2)
library(threed)
library(dplyr) 
library(tidyverse)
library(lubridate)
data <- read.csv("Example.csv")
head(data)

set.seed(0)

fit <- lm(y~ï..T*C*K,data = data)


matCube <- function(model, var1,var2,var3) {
  newData<-data.frame(   ï..T =c(-1,1,-1,1,-1,1,-1,1),
                      C=c(-1,-1,1,1,-1,-1,1,1),
                      K=c(-1,-1,-1,-1,1,1,1,1))
  newData$y=predict(model,newData)
  newData[newData==-1]=0
  newData$dim_margin=(newData$K)/2;
  newData$plot_T=newData$ï..T+newData$dim_margin;
  newData$plot_C=newData$C+newData$dim_margin;
  print(newData)
  plots<-data.frame(x1 = as.numeric(0), y1 = as.numeric(0), x2 = as.numeric(0), y2 = as.numeric(1),
                    x3 = as.numeric(1), y3 = as.numeric(1), x4 = as.numeric(1), y4 = as.numeric(0),
                    x5 = as.numeric(0.5), y5 = as.numeric(0.5), x6 = as.numeric(0.5), y6 = as.numeric(1.5),
                    x7 = as.numeric(1.5), y7 = as.numeric(1.5), x8 = as.numeric(1.5), y8 = as.numeric(0.5))
  cube<-ggplot(newData, aes(plot_T, plot_C))+ ggtitle("User Defined Cube Plot") +
    #geom_polygon(fill = NA, colour = 'black', size = 0.2) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = plots)+
    geom_segment(aes(x = x2, y = y2, xend = x3, yend = y3), data = plots)+
    geom_segment(aes(x = x3, y = y3, xend = x4, yend = y4), data = plots)+
    geom_segment(aes(x = x4, y = y4, xend = x1, yend = y1), data = plots)+
    geom_segment(aes(x = x5, y = y5, xend = x6, yend = y6), data = plots)+
    geom_segment(aes(x = x6, y = y6, xend = x7, yend = y7), data = plots)+
    geom_segment(aes(x = x7, y = y7, xend = x8, yend = y8), data = plots)+
    geom_segment(aes(x = x8, y = y8, xend = x5, yend = y5), data = plots)+
    geom_segment(aes(x = x2, y = y2, xend = x6, yend = y6), data = plots)+
    geom_segment(aes(x = x1, y = y1, xend = x5, yend = y5), data = plots)+
    geom_segment(aes(x = x4, y = y4, xend = x8, yend = y8), data = plots)+
    geom_segment(aes(x = x8, y = y8, xend = x5, yend = y5), data = plots)+
    geom_segment(aes(x = x7, y = y7, xend = x3, yend = y3), data = plots)+
    
    geom_point(color="blue",size=15,pch=1,fill="white") +
    theme_void() + 
    geom_text(data=newData, aes(plot_T, plot_C, label=y))+
    geom_text(data=newData, aes(plot_T, plot_C, label=y)) +
    annotate("text", x=0, y=-0.1, label= "-1")+
    annotate("text", x=-0.1, y=0, label= "-1")+
    annotate("text", x=-0.1, y=1, label= "1")  +
    annotate("text", x=1.1, y=0, label= "-1") +
    annotate("text", x=1, y=-0.1, label= "1")+
    annotate("text", x=1.6, y=0.5, label= "1")+
    annotate("text", x=0.5, y=-0.1, label= var1)+
    annotate("text", x=0.5, y=0, label= '|')+
    annotate("text", x=-0.1, y=0.5, label= var2)+
    annotate("text", x=0, y=0.5, label= '-')+
    annotate("text", x=1.3, y=0.25, label= var3)+
    annotate("text", x=1.28, y=0.3, label= '-')
  return(cube)
}


matCube(fit,"ï..T","C","K")
