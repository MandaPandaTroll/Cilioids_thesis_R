#positions

library(tidyverse)
library(ggplot2)
library(gganimate)
rm(list=ls())
gc()
autoPos <- read.csv("./PositionData/AutotrophPositions0.csv")

singlePos <- autoPos%>%filter(individualNumber == 1)

ggplot(autoPos,aes(x,y))+geom_point()+transition_time(time_seconds)+ease_aes('linear')+xlim(0,1)+ylim(0,1)
