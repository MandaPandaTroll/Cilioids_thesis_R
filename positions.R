#positions

library(tidyverse)
library(ggplot2)
library(gganimate)
rm(list=ls())
gc()
autoPos <- read.csv("./PositionData/AutotrophPositions0.csv")

singlePos <- autoPos%>%filter(individualNumber == 1)

ggplot(autoPos,aes(x,y))+geom_point()+transition_time(time_seconds)+ease_aes('linear')+xlim(0,1)+ylim(0,1)


pos.small <- autoPos%>%select(time_steps,x,y)
pos.small$time_steps <- as.numeric(pos.small$time_steps)
pos.small$x <- as.numeric(pos.small$x)
pos.small$y <- as.numeric(pos.small$y)
summary(pos.small)
postab <- table(pos.small)

posmat <- as.matrix(postab)

plot(pos.small$time_steps, type = "l")
diff(unique(pos.small$time_steps))


matrix(postab, ncol = ncol(M), dimnames = dimnames(M))


postabList <- list()

for( i in 1:length(postab[,1,1])){
  postabList[[i]] = postab[i,,]
  postabList[[i]] = as.matrix(postabList[[i]])
}
tet <- (matrix(as.numeric(postabList[[1]]), ncol = 16))

out <- split( pos.small , f = pos.small$time_steps)

ggplot(pos.small,aes(x,y))+geom_density_2d_filled()
plot(pos.small$x, pos.small$y)

