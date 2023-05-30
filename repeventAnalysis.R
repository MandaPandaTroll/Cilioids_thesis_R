#repeventanalysis

library(tidyverse)
library(ggplot2)

rm(list=ls())
dev.off()
gc()





{
  library(tidyverse)
  rm(list=ls())
  gc()
  POI <- data.frame(y = rep(8,8), x = c(8,9,10,11,12,13,14,15))
  simname <- "mat02_mig_8e4"
  
  thisdir = r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat02_mig_8e4)"
  setwd(thisdir)
  
  #setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/PopData/ManyRuns)")
  
  repEventPathList <- list()
  
  
  {
    shortest <- 1e6
    for(i in 1:10){
      repEventPathList[[i]] = Sys.glob(paste0(i,"/*/*/repEvents*.csv"))
      
      if(length(repEventPathList[[i]]) < shortest){
        shortest = length(repEventPathList[[i]])
      }
    }
    
    
    for(i in 1:10){
      repEventPathList[[i]] <- repEventPathList[[i]][1:shortest]
    }
    
    repEventMetaList <- list()
    for (i in 1:10){
      repEventMetaList[[i]] <- Map(read.csv,header = TRUE,repEventPathList[[i]])
      
    }
    
  }
  
  for(i in 1:10){
    repEventMetaList[[i]] = bind_rows(repEventMetaList[[i]])
  }
  
  for(i in 1:10){
    repEventMetaList[[i]]$run = i
  }
  
  
  repEventList.onlyPOI <- list()
  
  for(i in 1:10){
    repEventList.onlyPOI[[i]] = repEventMetaList[[i]]%>%filter(pos_x > 7 & pos_y == 8)
  }
  
  rm(repEventMetaList)
  gc()
  
  filename = "repEvents_1dim"
  if(!dir.exists("./repEvents_1dim")){
    dir.create("./repEvents_1dim")
  }
  setwd("./repEvents_1dim")
  
  for(i in 1:10){
    write.csv(repEventList.onlyPOI[[i]], paste0(filename,"_",simname,"_run",i,".csv"))
    gc()
  }
  setwd(thisdir)
  
  
  
}





POIrepEventPaths = Sys.glob("repEvents_1dim*.csv")
repEventList.onlyPOI <- list()
for(i in 1:10){
  repEventList.onlyPOI[[i]] <- read.csv(POIrepEventPaths[[i]])
}






big.rep <- bind_rows(repEventList.onlyPOI)
big.rep$ofMaxLifeSpan <- big.rep$age/big.rep$maxlifespan

ggplot(big.rep,aes(time_steps,ofMaxLifeSpan))+geom_bin_2d()+facet_wrap("repEventNumber")

hist(big.rep$age[which(big.rep$repEventNumber == 1)])
boxplot(age~repEventNumber,big.rep)
boxplot(ofMaxLifeSpan~repEventNumber,big.rep)
par(mfrow=c(3,3))

for(i in 1:9){
  hist(big.rep$ofMaxLifeSpan[which(big.rep$repEventNumber == i)], breaks = 32)
}
ggplot(big.rep,aes(ofMaxLifeSpan))+geom_histogram()+facet_grid(rows =vars(repEventNumber), cols = vars(run), scales = "free")


?aggregate()
big8 <- big.rep%>%filter(pos_x == 8)

ggplot(big8%>%filter(),aes(time_steps,ofMaxLifeSpan,colour = repEventNumber))+geom_point()



meanOverRuns.repevent_x8 <- aggregate(big8,by = list(big8$time_steps), mean)


meanOverRuns.repevent_x8 <- meanOverRuns.repevent_x8%>%select(-Group.1,-run)

hist(meanOverRuns.repevent_x8$ofMaxLifeSpan)

boxplot(meanOverRuns.repevent_x8$ofMaxLifeSpan~meanOverRuns.repevent_x8$repEventNumber)

ggplot(meanOverRuns.repevent_x8,aes(time_steps,ofMaxLifeSpan))+facet_wrap("repEventNumber")+geom_density_2d_filled()


setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R)")

parameters <- read.csv("params.txt")

parameters <- spread(parameters, key = Parameter, value = Value)


setwd("./RepData/")
repList <- lapply(list.files(pattern = "repEvents*"),read.csv,header = TRUE)


repEvents.df <- bind_rows(repList)
repEvents.df$hypotenuse <- sqrt(((repEvents.df$pos_x-7)^2)+((repEvents.df$pos_y-7)^2))

plot(repEvents.df$time_steps,repEvents.df$hypotenuse)
ggplot(repEvents.df,aes(pos_x,pos_y, colour = time_steps))+geom_count()+scale_colour_continuous()

lilrep <- repEvents.df%>%select(time_steps,pos_x,pos_y)

liltab <- table(lilrep)


repeventCountMat <- as.matrix(table(repevents$pos_x,repevents$pos_y))

my_matrix <- matrix(repeventCountMat, ncol=ncol(repeventCountMat), dimnames=dimnames(repeventCountMat))


image(my_matrix)
grid::grid.raster(my_matrix/max(my_matrix))
repevents<-bind_rows(repList)
rm(repList)

ggplot(repevents,aes(pos_x,pos_y, colour = time_steps))+geom_count()

gc()
setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R)")
meanReproductiveAgeStart <- mean(repevents$age[which(repevents$repEventNumber == 1)])





summary(repevents)
minrepAge <- min(repevents$age)
hist(repevents$age, breaks = 256)
boxplot(age~repEventNumber,repevents)
ggplot(repevents,aes(age,repEventNumber))+geom_density2d_filled()


gc()
#repevents <- read.csv("./RepData/RepEvents0.csv")


ggplot(repevents,aes(age,repEventNumber))+geom_count()
ggplot(repevents,aes(age/max(age),repEventNumber))+geom_hex()


plot(age~repEventNumber,repevents)


firstReps <- repevents%>%filter(repEventNumber == 1)
notFirstReps <- repevents%>%filter(repEventNumber > 1)


hist(firstReps$age, breaks = 256)
hist(notFirstReps$age, breaks = 256)

