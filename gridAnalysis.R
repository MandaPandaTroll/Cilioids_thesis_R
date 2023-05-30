#Nutrient grid analysis
{
library(dplyr)
library(magick)
library(png)
  library(tidyverse)
  library(grid)
  library(viridis)
if(!require(scales)){
  install.packages("scales", dependencies=TRUE)
  library(scales)
}
library(ggplot2)
#library(stringr) 
  #library(spatstat)
}

rm(list=ls())
dev.off()
gc()



  
  {
    rm(list=ls())
    gc()
    POI <- data.frame(y = rep(8,8), x = c(8,9,10,11,12,13,14,15))
    simname <- "mat02_mig_8e4"
    #
    thisdir = r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat02_mig_8e4)"
    setwd(thisdir)
    
    parameters <- read.csv("./1/Cilioids_thesis_Data/params.txt")
    parameters <- spread(parameters, key = Parameter, value = Value)
    parameters$Grid.height <- as.numeric(parameters$Grid.height)
    parameters$Grid.width <- as.numeric(parameters$Grid.width)
    parameters$Nutrients.total <- as.numeric(parameters$Nutrients.total)
    parameters$numCells = parameters$Grid.height*parameters$Grid.width
    parameters$meanExpectedNutrients <- parameters$Nutrients.total/parameters$numCells
    parameters$Sample.period.grid <- as.numeric(parameters$Sample.period.grid)
    
    
    #setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/PopData/ManyRuns)")
    
    
    
    
    
    
    #freenute
    {
      
      freeNutePathList <- list()
      
      
      {
        shortest <- 1e6
        for(i in 1:10){
          freeNutePathList[[i]] = Sys.glob(paste0(i,"/*/NutrientGridData/gridData*.csv"))
          
          if(length(freeNutePathList[[i]]) < shortest){
            shortest = length(freeNutePathList[[i]])
          }
        }
        
        
        for(i in 1:10){
          freeNutePathList[[i]] <- freeNutePathList[[i]][1:shortest]
        }
        
        freeNuteMetaList <- list()
        for (i in 1:10){
          freeNuteMetaList[[i]] <- Map(read.csv,header = FALSE,freeNutePathList[[i]])
          
        }
        
      }
      
      # tempfremeta <- freeNuteMetaList
      
      
      
      for(i in 1:10){
        freeNuteMetaList[[i]] <- lapply(freeNuteMetaList[[i]], as.matrix)
      }
      onedimMetaList.freeNute <- list()
      
      
      C1list <- list()
      C2list <- list()
      C3list <- list()
      C4list <- list()
      C5list <- list()
      C6list <- list()
      C7list <- list()
      C8list <- list()
      
      for(n in 1:10){
        for(i in 1:length(freeNuteMetaList[[n]])){
          
          C1list[[i]] <- freeNuteMetaList[[n]][[i]][8,9]
          C2list[[i]] <- freeNuteMetaList[[n]][[i]][8,10]
          C3list[[i]] <- freeNuteMetaList[[n]][[i]][8,11]
          C4list[[i]] <- freeNuteMetaList[[n]][[i]][8,12]
          C5list[[i]] <- freeNuteMetaList[[n]][[i]][8,13]
          C6list[[i]] <- freeNuteMetaList[[n]][[i]][8,14]
          C7list[[i]] <- freeNuteMetaList[[n]][[i]][8,15]
          C8list[[i]] <- freeNuteMetaList[[n]][[i]][8,16]
          
        }
        onedimMetaList.freeNute[[n]] <- data.frame(
          run = rep(n, length(freeNuteMetaList[[n]])),
          sample = seq(1:length(freeNuteMetaList[[n]])),
          C1 = unlist(C1list),
          C2 = unlist(C2list),
          C3 = unlist(C3list),
          C4 = unlist(C4list),
          C5 = unlist(C5list),
          C6 = unlist(C6list),
          C7 = unlist(C7list),
          C8 = unlist(C8list)
        )
        
      }
      
      
      
      time_steps <- seq(1:length(freeNuteMetaList[[1]]))*as.numeric(parameters$Sample.period.grid)-(parameters$Sample.period.grid-4)
      
      for(i in 1:10){
        onedimMetaList.freeNute[[i]]$time_steps <- time_steps
      }
      
      #Run Sample A B C D E F G H
      
      filename = paste0(simname,"freenute_1dim")
      if(!dir.exists("./freeNute_1dim")){
        dir.create("./freeNute_1dim")
      }
      setwd("./freeNute_1dim")
      for(i in 1:10){
        write.csv(onedimMetaList.freeNute[[i]], paste0(filename,"_",simname,"_run",i,".csv"))
        gc()
      }
      setwd(thisdir)
      
    }
    
    
    
    
    
    
    #lockednute
    {
      
      lockedNutePathList <- list()
      
      
      {
        shortest <- 1e6
        for(i in 1:10){
          lockedNutePathList[[i]] = Sys.glob(paste0(i,"/*/LockedNutrientGridData/gridData_locked*.csv"))
          
          if(length(lockedNutePathList[[i]]) < shortest){
            shortest = length(lockedNutePathList[[i]])
          }
        }
        
        
        for(i in 1:10){
          lockedNutePathList[[i]] <- lockedNutePathList[[i]][1:shortest]
        }
        
        lockedNuteMetaList <- list()
        for (i in 1:10){
          lockedNuteMetaList[[i]] <- Map(read.csv,header = FALSE,lockedNutePathList[[i]])
          
        }
        
      }
      
      # tempfremeta <- lockedNuteMetaList
      
      
      
      for(i in 1:10){
        lockedNuteMetaList[[i]] <- lapply(lockedNuteMetaList[[i]], as.matrix)
      }
      onedimMetaList.lockedNute <- list()
      
      
      C1list <- list()
      C2list <- list()
      C3list <- list()
      C4list <- list()
      C5list <- list()
      C6list <- list()
      C7list <- list()
      C8list <- list()
      
      for(n in 1:10){
        for(i in 1:length(lockedNuteMetaList[[n]])){
          
          C1list[[i]] <- lockedNuteMetaList[[n]][[i]][8,9]
          C2list[[i]] <- lockedNuteMetaList[[n]][[i]][8,10]
          C3list[[i]] <- lockedNuteMetaList[[n]][[i]][8,11]
          C4list[[i]] <- lockedNuteMetaList[[n]][[i]][8,12]
          C5list[[i]] <- lockedNuteMetaList[[n]][[i]][8,13]
          C6list[[i]] <- lockedNuteMetaList[[n]][[i]][8,14]
          C7list[[i]] <- lockedNuteMetaList[[n]][[i]][8,15]
          C8list[[i]] <- lockedNuteMetaList[[n]][[i]][8,16]
          
        }
        onedimMetaList.lockedNute[[n]] <- data.frame(
          run = rep(n, length(lockedNuteMetaList[[n]])),
          sample = seq(1:length(lockedNuteMetaList[[n]])),
          C1 = unlist(C1list),
          C2 = unlist(C2list),
          C3 = unlist(C3list),
          C4 = unlist(C4list),
          C5 = unlist(C5list),
          C6 = unlist(C6list),
          C7 = unlist(C7list),
          C8 = unlist(C8list)
        )
        
      }
      
      
      
      time_steps <- seq(1:length(lockedNuteMetaList[[1]]))*as.numeric(parameters$Sample.period.grid)-(parameters$Sample.period.grid-4)
      
      for(i in 1:10){
        onedimMetaList.lockedNute[[i]]$time_steps <- time_steps
      }
      
      #Run Sample A B C D E F G H
      
      filename = "lockednute_1dim"
      if(!dir.exists("./lockedNute_1dim")){
        dir.create("./lockedNute_1dim")
      }

      setwd("./lockedNute_1dim")
      for(i in 1:10){
        write.csv(onedimMetaList.lockedNute[[i]], paste0(filename,"_",simname,"_run",i,".csv"))
        gc()
      }
      setwd(thisdir)
      
    }
    
    
    
    
    
    
    #pop
    {
      
      
      
      popPathList <- list()
      
      
      {
        shortest <- 1e6
        for(i in 1:10){
          popPathList[[i]] = Sys.glob(paste0(i,"/*/gridPopData/gridPopData*.csv"))
          
          if(length(popPathList[[i]]) < shortest){
            shortest = length(popPathList[[i]])
          }
        }
        
        
        for(i in 1:10){
          popPathList[[i]] <- popPathList[[i]][1:shortest]
        }
        
        popMetaList <- list()
        for (i in 1:10){
          popMetaList[[i]] <- Map(read.csv,header = FALSE,popPathList[[i]])
          
        }
        
      }
      
      
      
      
      
      for(i in 1:10){
        popMetaList[[i]] <- lapply(popMetaList[[i]], as.matrix)
      }
      onedimMetaList.pop <- list()
      
      
      C1list <- list()
      C2list <- list()
      C3list <- list()
      C4list <- list()
      C5list <- list()
      C6list <- list()
      C7list <- list()
      C8list <- list()
      
      for(n in 1:10){
        for(i in 1:length(popMetaList[[n]])){
          
          C1list[[i]] <- popMetaList[[n]][[i]][8,9]
          C2list[[i]] <- popMetaList[[n]][[i]][8,10]
          C3list[[i]] <- popMetaList[[n]][[i]][8,11]
          C4list[[i]] <- popMetaList[[n]][[i]][8,12]
          C5list[[i]] <- popMetaList[[n]][[i]][8,13]
          C6list[[i]] <- popMetaList[[n]][[i]][8,14]
          C7list[[i]] <- popMetaList[[n]][[i]][8,15]
          C8list[[i]] <- popMetaList[[n]][[i]][8,16]
          
        }
        onedimMetaList.pop[[n]] <- data.frame(
          run = rep(n, length(popMetaList[[n]])),
          sample = seq(1:length(popMetaList[[n]])),
          C1 = unlist(C1list),
          C2 = unlist(C2list),
          C3 = unlist(C3list),
          C4 = unlist(C4list),
          C5 = unlist(C5list),
          C6 = unlist(C6list),
          C7 = unlist(C7list),
          C8 = unlist(C8list)
        )
        
      }
      
      
      
      time_steps <- seq(1:length(popMetaList[[1]]))*as.numeric(parameters$Sample.period.grid)-(parameters$Sample.period.grid-4)
      
      for(i in 1:10){
        onedimMetaList.pop[[i]]$time_steps <- time_steps
      }
      
      #Run Sample A B C D E F G H
      
      filename = "pop_1dim"
      if(!dir.exists("./pop_1dim")){
        dir.create("./pop_1dim")
      }
      setwd("./pop_1dim")
      for(i in 1:10){
        write.csv(onedimMetaList.pop[[i]], paste0(filename,"_",simname,"_run",i,".csv"))
        gc()
      }
      setwd(thisdir)
      
    }  
    
    
    
    
    #self
    {
      
      
      
      selfPathList <- list()
      
      
      {
        shortest <- 1e6
        for(i in 1:10){
          selfPathList[[i]] = Sys.glob(paste0(i,"/*/selfingGridData/selfingGridData*.csv"))
          
          if(length(selfPathList[[i]]) < shortest){
            shortest = length(selfPathList[[i]])
          }
        }
        
        
        for(i in 1:10){
          selfPathList[[i]] <- selfPathList[[i]][1:shortest]
        }
        
        selfMetaList <- list()
        for (i in 1:10){
          selfMetaList[[i]] <- Map(read.csv,header = FALSE,selfPathList[[i]])
          
        }
        
      }
      
      
      
      
      
      for(i in 1:10){
        selfMetaList[[i]] <- lapply(selfMetaList[[i]], as.matrix)
      }
      onedimMetaList.self <- list()
      
      
      C1list <- list()
      C2list <- list()
      C3list <- list()
      C4list <- list()
      C5list <- list()
      C6list <- list()
      C7list <- list()
      C8list <- list()
      
      for(n in 1:10){
        for(i in 1:length(selfMetaList[[n]])){
          
          C1list[[i]] <- selfMetaList[[n]][[i]][8,9]
          C2list[[i]] <- selfMetaList[[n]][[i]][8,10]
          C3list[[i]] <- selfMetaList[[n]][[i]][8,11]
          C4list[[i]] <- selfMetaList[[n]][[i]][8,12]
          C5list[[i]] <- selfMetaList[[n]][[i]][8,13]
          C6list[[i]] <- selfMetaList[[n]][[i]][8,14]
          C7list[[i]] <- selfMetaList[[n]][[i]][8,15]
          C8list[[i]] <- selfMetaList[[n]][[i]][8,16]
          
        }
        onedimMetaList.self[[n]] <- data.frame(
          run = rep(n, length(selfMetaList[[n]])),
          sample = seq(1:length(selfMetaList[[n]])),
          C1 = unlist(C1list),
          C2 = unlist(C2list),
          C3 = unlist(C3list),
          C4 = unlist(C4list),
          C5 = unlist(C5list),
          C6 = unlist(C6list),
          C7 = unlist(C7list),
          C8 = unlist(C8list)
        )
        
      }
      
      
      
      time_steps <- seq(1:length(selfMetaList[[1]]))*as.numeric(parameters$Sample.period.grid)-(parameters$Sample.period.grid-4)
      
      for(i in 1:10){
        onedimMetaList.self[[i]]$time_steps <- time_steps
      }
      
      #Run Sample A B C D E F G H
      
      filename = "self_1dim"
      if(!dir.exists("./self_1dim")){
        dir.create("./self_1dim")
      }
      
      setwd("./self_1dim")
      for(i in 1:10){
        write.csv(onedimMetaList.self[[i]], paste0(filename,"_",simname,"_run",i,".csv"))
        gc()
      }
      setwd(thisdir)
      
    }
  #OOOOOO
  
  
  
  
  
  
  big.freenute <- bind_rows(onedimMetaList.freeNute)
  write.csv(big.freenute, paste0("freenute_allruns_", simname,".csv"))
  big.lockednute <- bind_rows(onedimMetaList.lockedNute)
  write.csv(big.lockednute, paste0("lockednute_allruns_", simname,".csv"))
  big.pop <- bind_rows(onedimMetaList.pop)
  write.csv(big.pop, paste0("pop_allruns_", simname,".csv"))
  big.self <- bind_rows(onedimMetaList.self)
  write.csv(big.self, paste0("self_allruns_", simname,".csv"))
  big.selfRatio <- big.pop
  
  
  big.selfRatio$C1 <- big.self$C1/big.pop$C1
  big.selfRatio$C2 <- big.self$C2/big.pop$C2
  big.selfRatio$C3 <- big.self$C3/big.pop$C3
  big.selfRatio$C4 <- big.self$C4/big.pop$C4
  big.selfRatio$C5 <- big.self$C5/big.pop$C5
  big.selfRatio$C6 <- big.self$C6/big.pop$C6
  big.selfRatio$C7 <- big.self$C7/big.pop$C7
  big.selfRatio$C8 <- big.self$C8/big.pop$C8
  big.selfRatio <- replace(big.selfRatio, is.na(big.selfRatio),0)
  
  write.csv(big.selfRatio, paste0("selfratio_allruns_", simname,".csv"))
  
  big.totalnute <- big.freenute
  
  big.totalnute$C1 <- big.freenute$C1+big.lockednute$C1
  big.totalnute$C2 <- big.freenute$C2+big.lockednute$C2
  big.totalnute$C3 <- big.freenute$C3+big.lockednute$C3
  big.totalnute$C4 <- big.freenute$C4+big.lockednute$C4
  big.totalnute$C5 <- big.freenute$C5+big.lockednute$C5
  big.totalnute$C6 <- big.freenute$C6+big.lockednute$C6
  big.totalnute$C7 <- big.freenute$C7+big.lockednute$C7
  big.totalnute$C8 <- big.freenute$C8+big.lockednute$C8
  
  write.csv(big.totalnute, paste0("totalnute_allruns_", simname,".csv"))
    
    
    
  }
  
  
  
  #new_names <- paste0("gridData",(1000 + as.numeric(str_extract(file_names, '(?<=gridData)[0-9]*'))),'.csv')
  # [1] "1100.asc" "1002.asc" "1202.asc" "1301.asc" "1003.asc"  
  
  #file.rename(file_names, new_names)
  
  
  {
    
   

plot(big.totalnute$time_steps,big.totalnute$C8, type = "h")


meanOverRuns.totalnute <- aggregate(big.totalnute,by = list(big.totalnute$time_steps), mean)
meanOverRuns.totalnute <- meanOverRuns.totalnute%>%select(-Group.1,-run,-sample)


meanOverRuns.pop <- aggregate(big.pop,by = list(big.pop$time_steps), mean)
meanOverRuns.pop <- meanOverRuns.pop%>%select(-Group.1,-run,-sample)

meanOverRuns.self <- aggregate(big.self,by = list(big.self$time_steps), mean)
meanOverRuns.self <- meanOverRuns.self%>%select(-Group.1,-run,-sample)

meanOverRuns.selfRatio <- aggregate(big.selfRatio,by = list(big.selfRatio$time_steps), mean)
meanOverRuns.selfRatio <- meanOverRuns.selfRatio%>%select(-Group.1,-run,-sample)


meanOverRuns.freeNute <- aggregate(big.freenute,by = list(big.freenute$time_steps), mean)
meanOverRuns.freeNute <- meanOverRuns.freeNute%>%select(-Group.1,-run,-sample)


library(zoo)

pdf(paste0("meansFromGrids_",simname,".pdf"))


ggplot(meanOverRuns.selfRatio%>%gather(key = "grid.cell", value = "selfing.ratio", -time_steps),aes(time_steps,selfing.ratio))+facet_wrap("grid.cell",scales = "free")+geom_point(size = 0.5)+ggtitle(paste0("Selfing ratio over time\nfrom center to edge\nmean over 10 runs\nmaturity at 0.1, p.migration = ",parameters$P.migration ))+geom_line(aes(y = rollmedian(selfing.ratio, 33, na.pad = T)), colour = "red")

ggplot(meanOverRuns.self%>%gather(key = "grid.cell", value = "inds.from.selfing", -time_steps),aes(time_steps,inds.from.selfing))+facet_wrap("grid.cell")+geom_point(size = 0.5)+ggtitle(paste0("Inds from selfing over time\nfrom center to edge\nmean over 10 runs\nmaturity at 0.1, p.migration = ",parameters$P.migration ))+geom_line(aes(y = rollmedian(inds.from.selfing, 33, na.pad = T)), colour = "red")


ggplot(meanOverRuns.pop%>%gather(key = "grid.cell", value = "population.size", -time_steps),aes(time_steps,log10(population.size+1)))+facet_wrap("grid.cell")+geom_point(size = 0.5)+ggtitle(paste0("Population over time\nfrom center to edge\nmean over 10 runs\nmaturity at 0.1, p.migration = ",parameters$P.migration ))+geom_line(aes(y = rollmedian(log10(population.size+1), 33, na.pad = T)), colour = "red")




ggplot(meanOverRuns.freeNute%>%gather(key = "grid.cell", value = "free.nutrient.content", -time_steps),aes(time_steps,log10(free.nutrient.content+1)))+facet_wrap("grid.cell")+geom_point(size = 0.5)+ggtitle(paste0("Free nutrients over time\nfrom center to edge\nmean over 10 runs\nmaturity at 0.1, p.migration = ",parameters$P.migration ))+geom_line(aes(y = rollmedian(log10(free.nutrient.content+1), 33, na.pad = T)), colour = "red")


ggplot(meanOverRuns.totalnute%>%gather(key = "grid.cell", value = "total.nutrient.content", -time_steps),aes(time_steps,log10(total.nutrient.content+1)))+facet_wrap("grid.cell")+geom_point(size = 0.5)+ggtitle(paste0("Total nutrients over time\nfrom center to edge\nmean over 10 runs\nmaturity at 0.1, p.migration = ",parameters$P.migration ))+geom_line(aes(y = rollmedian(log10(total.nutrient.content+1), 33, na.pad = T)), colour = "red")



dev.off()

gogo.g <- gogo%>%filter(time_steps  > 132)%>%select(-sample)%>%gather(key = "grid.cell", value = "free.nutrient.content", -time_steps, - run)

ggplot(gogo.g, aes(time_steps, free.nutrient.content, colour = grid.cell))+geom_line()+facet_grid(rows = vars(run), cols = vars(grid.cell))

run1.g <- onedimMetaList.freeNute[[1]]%>%filter(time_steps > 132)%>%select(-sample, -run)%>%gather(key = "grid.cell", value = "free.nutrient.content", -time_steps)
  

ggplot(run1.g,aes(time_steps,free.nutrient.content))+geom_area()+facet_wrap("grid.cell")+ggtitle("free nutrients from middle to outer")

ggplot(run1.g,aes(time_steps,free.nutrient.content))+geom_point(size = 0.5)+facet_grid(rows = vars(grid.cell))+ggtitle("free nutrients from middle to outer")+geom_smooth(aes(colour = grid.cell))


ggplot(run1.g,aes(time_steps,free.nutrient.content, fill = grid.cell))+geom_area()+ggtitle("free nutrients from middle to outer")







  setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/)")
  
  
  
  setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R)")
  setwd("./LockedNutrientGridData")
  lockedGridList <- lapply(lapply(list.files(pattern = "gridData_locked*"),read.csv,header = FALSE),as.matrix)
  
  
  
  setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/NutrientGridData)")
  gridList<- lapply(lapply(list.files(pattern = "gridData*"),read.csv,header = FALSE),as.matrix)
  gc()
  
  
  setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/gridPopData)")
  gridPopList<- lapply(lapply(list.files(pattern = "gridPopData*"),read.csv,header = FALSE),as.matrix)
  gc()
  
  setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/selfingGridData)")
  selfingGridList<- lapply(lapply(list.files(pattern = "selfingGridData*"),read.csv,header = FALSE),as.matrix)
  gc()
  
  
  setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R)")
  
  parameters <- read.csv("params.txt")
  parameters <- spread(parameters, key = Parameter, value = Value)
  parameters$Grid.height <- as.numeric(parameters$Grid.height)
  parameters$Grid.width <- as.numeric(parameters$Grid.width)
  parameters$Nutrients.total <- as.numeric(parameters$Nutrients.total)
  parameters$numCells = parameters$Grid.height*parameters$Grid.width
  parameters$meanExpectedNutrients <- parameters$Nutrients.total/parameters$numCells
  gridDims <- list(as.character(seq(1:parameters$Grid.width)),as.character(seq(1:parameters$Grid.height)))
  #gridList <- (lapply(list.files(pattern = "gridData*"), read.csv, header = FALSE))
  gridHW <- data.frame(width = as.numeric(parameters$Grid.width), height = as.numeric(parameters$Grid.height))
  
  
  #gridList <- lapply(gridList,as.matrix)
  for(i in length(gridList)){
    dimnames(gridList[[i]]) <- gridDims
  }
  
  for(i in length(lockedGridList)){
    dimnames(lockedGridList[[i]]) <- gridDims
  }
  
  for(i in length(gridPopList)){
    dimnames(gridPopList[[i]]) <- gridDims
  }
  
  for(i in length(selfingGridList)){
    dimnames(selfingGridList[[i]]) <- gridDims
  }
  
  gridlist2 <- gridList[1:length(gridPopList)]
  
  totalNuteList <- gridlist2
  
  
}












{
  rotate <- function(x) t(apply(x, 2, rev))
  grayPal <- colorRampPalette(grey.colors(1024))
  coloPal <-  colorRampPalette(plasma(1024))
  colos <- coloPal(1024)
  grays <- grayPal(1024)
  colos[1] <- "#000000"
  grays[1] <- "#000000"
  
  f <- colorRamp( colos)
  f.gray <- colorRamp(grays)
  
  for(i in 1:length(gridlist2)){
    totalNuteList[[i]] = totalNuteList[[i]]+lockedGridList[[i]]
    
  }
  

  
  selfingRatioList <- selfingGridList

  for(i in 1:length(gridPopList)){
    selfingRatioList[[i]] = (selfingGridList[[i]]/ (gridPopList[[i]]))
    selfingRatioList[[i]] = replace(selfingRatioList[[i]], is.na(selfingRatioList[[i]]),0)

  } 
  
  HighestPop <- max(unlist(lapply(gridPopList,max)))
  HighestFreeNute <- max(unlist(lapply(gridlist2,max)))
  HighestLockedNute <- max(unlist(lapply(lockedGridList,max)))
  HighestToteNute <- max(unlist(lapply(totalNuteList,max)))
  HighestSelf <- max(unlist(lapply(selfingGridList,max)))
  HighestSelfRatio <- max(unlist(lapply(selfingRatioList,max)))
  

  centersList.freeNutrient <- list()
  centersList.relativeFreeNutrient <- list()
  centersList.pop<- list()
  centersList.selfing <- list()
  centersList.selfingRatio <- list()
  wC <- parameters$Grid.width/2
  hC <- parameters$Grid.height/2
  tempcenters = matrix(nrow = 2, ncol = 2)

  
  newCell.df <- data.frame(center.freeNutrient = rep(0,length(gridlist2)),center.lockedNutrient = rep(0,length(gridlist2)), center.relativeFreeNutrient = rep(0,length(gridlist2)), center.pop = rep(0,length(gridlist2)), center.selfing = rep(0,length(gridlist2)), center.selfingRatio =rep(0,length(gridlist2)), adjacent.freeNutrient = rep(0,length(gridlist2)),adjacent.lockedNutrient = rep(0,length(gridlist2)), adjacent.relativeFreeNutrient = rep(0,length(gridlist2)), adjacent.pop = rep(0,length(gridlist2)), adjacent.selfing = rep(0,length(gridlist2)), adjacent.selfingRatio =rep(0,length(gridlist2)), distant.freeNutrient = rep(0,length(gridlist2)),distant.lockedNutrient = rep(0,length(gridlist2)), distant.relativeFreeNutrient = rep(0,length(gridlist2)), distant.pop = rep(0,length(gridlist2)), distant.selfing = rep(0,length(gridlist2)), distant.selfingRatio = rep(0,length(gridlist2)))
  
  
  for (i in 1: length(gridlist2)){
    tempcenters[1,1] = gridlist2[[i]][hC,wC]
    tempcenters[1,2] = gridlist2[[i]][hC,wC+1]
    tempcenters[2,1] = gridlist2[[i]][hC+1,wC]
    tempcenters[2,2] = gridlist2[[i]][hC+1,wC+1]
    centersList.freeNutrient[[i]] = tempcenters
    newCell.df$center.freeNutrient[i] = gridlist2[[i]][hC+1,wC+1]
    newCell.df$adjacent.freeNutrient[i] = gridlist2[[i]][hC+1,wC+2]
    newCell.df$distant.freeNutrient[i] = gridlist2[[i]][hC+1,wC+5]
    
    newCell.df$center.lockedNutrient[i] = lockedGridList[[i]][hC+1,wC+1]
    newCell.df$adjacent.lockedNutrient[i] = lockedGridList[[i]][hC+1,wC+2]
    newCell.df$distant.lockedNutrient[i] = lockedGridList[[i]][hC+1,wC+5]
    
    
    tempcenters[1,1] = gridlist2[[i]][hC,wC]/parameters$meanExpectedNutrients
    tempcenters[1,2] = gridlist2[[i]][hC,wC+1]/parameters$meanExpectedNutrients
    tempcenters[2,1] = gridlist2[[i]][hC+1,wC]/parameters$meanExpectedNutrients
    tempcenters[2,2] = gridlist2[[i]][hC+1,wC+1]/parameters$meanExpectedNutrients
    centersList.relativeFreeNutrient[[i]] = tempcenters
    
    newCell.df$center.relativeFreeNutrient[i] = gridlist2[[i]][hC+1,wC+1]/parameters$meanExpectedNutrients
    newCell.df$adjacent.relativeFreeNutrient[i] = gridlist2[[i]][hC+1,wC+2]/parameters$meanExpectedNutrients
    newCell.df$distant.relativeFreeNutrient[i] = gridlist2[[i]][hC+1,wC+5]/parameters$meanExpectedNutrients
    
    
    tempcenters[1,1] = gridPopList[[i]][hC,wC]
    tempcenters[1,2] = gridPopList[[i]][hC,wC+1]
    tempcenters[2,1] = gridPopList[[i]][hC+1,wC]
    tempcenters[2,2] = gridPopList[[i]][hC+1,wC+1]
    centersList.pop[[i]] = tempcenters

    
    newCell.df$center.pop[i] = gridPopList[[i]][hC+1,wC+1]
    newCell.df$adjacent.pop[i] = gridPopList[[i]][hC+1,wC+2]
    newCell.df$distant.pop[i] = gridPopList[[i]][hC+1,wC+5]
    
    tempcenters[1,1] = selfingGridList[[i]][hC,wC]
    tempcenters[1,2] = selfingGridList[[i]][hC,wC+1]
    tempcenters[2,1] = selfingGridList[[i]][hC+1,wC]
    tempcenters[2,2] = selfingGridList[[i]][hC+1,wC+1]
    centersList.selfing[[i]] = tempcenters
    newCell.df$center.selfing[i] = selfingGridList[[i]][hC+1,wC+1]
    newCell.df$adjacent.selfing[i] = selfingGridList[[i]][hC+1,wC+2]
    newCell.df$distant.selfing[i] = selfingGridList[[i]][hC+1,wC+5]
    
    tempcenters[1,1] = selfingRatioList[[i]][hC,wC]
    tempcenters[1,2] = selfingRatioList[[i]][hC,wC+1]
    tempcenters[2,1] = selfingRatioList[[i]][hC+1,wC]
    tempcenters[2,2] = selfingRatioList[[i]][hC+1,wC+1]
    centersList.selfingRatio[[i]] = tempcenters
    newCell.df$center.selfingRatio[i] = selfingRatioList[[i]][hC+1,wC+1]
    newCell.df$adjacent.selfingRatio[i] = selfingRatioList[[i]][hC+1,wC+2]
    newCell.df$distant.selfingRatio[i] = selfingRatioList[[i]][hC+1,wC+5]
      
  }
  
   
  time_steps <- seq(1:length(centersList.freeNutrient))*as.numeric(parameters$Sample.period.grid)  
    
  time_steps <- time_steps-(as.numeric(parameters$Sample.period.grid) -4)
  newCell.df$time_steps <- time_steps
}




ggplot(newCell.df%>%select(time_steps,center.freeNutrient,adjacent.freeNutrient,distant.freeNutrient)%>%gather(key = "cell",value = "value", - time_steps),aes(time_steps,value))+geom_point()+geom_smooth(aes(colour = cell))+facet_grid("cell", scales = "free")

ggplot(newCell.df%>%select(time_steps,center.pop,adjacent.pop,distant.pop)%>%gather(key = "cell",value = "value", - time_steps),aes(time_steps,value))+geom_point()+geom_smooth(aes(colour = cell))+facet_grid("cell", scales = "free")

ggplot(newCell.df%>%select(time_steps,center.selfing,adjacent.selfing,distant.selfing)%>%gather(key = "cell",value = "value", - time_steps),aes(time_steps,value))+geom_point()+geom_smooth(aes(colour = cell))+facet_grid("cell", scales = "free")

ggplot(newCell.df%>%select(time_steps,center.lockedNutrient,adjacent.lockedNutrient,distant.lockedNutrient)%>%gather(key = "cell",value = "value", - time_steps),aes(time_steps,value))+geom_point()+geom_smooth(aes(colour = cell))+facet_grid("cell", scales = "free")

ggplot(newCell.df%>%select(time_steps,center.selfingRatio,adjacent.selfingRatio,distant.selfingRatio,center.pop,adjacent.pop,distant.pop)%>%gather(key = "cell",value = "value", - time_steps),aes(time_steps,value))+geom_point()+geom_smooth(aes(colour = cell))+facet_grid("cell", scales = "free")

library(GGally)
ggpairs(newCell.df%>%select(time_steps,center.selfingRatio,adjacent.selfingRatio,distant.selfingRatio,center.pop,adjacent.pop,distant.pop))

ggplot(newCell.df%>%select(time_steps,center.relativeFreeNutrient,adjacent.relativeFreeNutrient,distant.relativeFreeNutrient)%>%gather(key = "cell",value = "value", - time_steps),aes(time_steps,value))+geom_point()+geom_smooth(aes(colour = cell))+facet_grid("cell", scales = "free")




centersVector.freeNutrient <- unlist(lapply(centersList.freeNutrient,mean))
centersVector.relativeFreeNutrient <- unlist(lapply(centersList.relativeFreeNutrient,mean))

centersVector.pop <- unlist(lapply(centersList.pop,mean))
centersVector.selfing <- unlist(lapply(centersList.selfing,mean))
centersVector.selfingRatio <- unlist(lapply(centersList.selfingRatio,mean))

centersVector.freeNutrient.scaled <- rescale(unlist(lapply(centersList.freeNutrient,mean)), c(0,1))
centersVector.pop.scaled <- rescale(unlist(lapply(centersList.pop,mean)), c(0,1))
centersVector.selfing.scaled <- rescale(unlist(lapply(centersList.selfing,mean)), c(0,1))
centersVector.selfingRatio.scaled <- rescale(unlist(lapply(centersList.selfingRatio,mean)), c(0,1))





centersVectors.df <- data.frame(time_steps = time_steps, meanFreeNutrients = centersVector.freeNutrient, relativeFreeNutrient = centersVector.relativeFreeNutrient,  meanPopulation = centersVector.pop, meanIndsFromSelfing = centersVector.selfing, meanSelfingRatio = centersVector.selfingRatio)

centersVectors.df.scaled <- data.frame(time_steps = time_steps, meanFreeNutrients.scaled = centersVector.freeNutrient.scaled, meanPopulation.scaled = centersVector.pop.scaled, meanIndsFromSelfing.scaled = centersVector.selfing.scaled, meanSelfingRatio.scaled = centersVector.selfingRatio)

ratios.df <- centersVectors.df%>%select(time_steps, relativeFreeNutrient, meanSelfingRatio)

notRatios.df <- centersVectors.df%>%select(time_steps, meanFreeNutrients, meanPopulation, meanIndsFromSelfing)


ggplot(gather(centersVectors.df, key= "variable", value = "value", - time_steps), aes(time_steps, value, color = variable))+geom_point(size = 0.5)+facet_wrap("variable", scales ="free")+ggtitle("Mean of  4 center cells", subtitle = paste0("init nutrient distribution: ", parameters$Nutrient.initial.distribution,"\n", "init avg nutrient concentration = ", parameters$Mean.global.nutrient.concentration))

plot(centersVectors.df$meanPopulation,centersVectors.df$meanSelfingRatio)

ggplot(gather(notRatios.df, key= "variable", value = "value", - time_steps), aes(time_steps, value, color = variable))+geom_point(size = 0.5)+geom_smooth()+facet_grid("variable", scales = "free")+ggtitle("Mean of  4 center cells", subtitle = paste0("init nutrient distribution: ", parameters$Nutrient.initial.distribution,"\n", "init avg nutrient concentration = ", parameters$Mean.global.nutrient.concentration))






getSDs <- function(inlist){
  f <- as.numeric(parameters$Sample.period.grid)
  vlist <- lapply(inlist,as.vector)
  vsteps <- seq(from = f, to = length(vlist)*f,by = f)-(f-4)
  sds <- unlist(lapply(vlist,sd))
  sd_df <- data.frame(steps = vsteps, sdev = sds)
  return(sd_df)
  
}


sd_df <- getSDs(gridList)
sd_df$variable ="free.nutrients"
lockedsd_df <- getSDs(lockedGridList)
lockedsd_df$variable ="locked.nutrients"
popsd_df <- getSDs(gridPopList)
popsd_df$variable ="population.size"

big.sd.df <- rbind(sd_df,lockedsd_df,popsd_df)

ggplot(big.sd.df,aes(steps, sdev))+facet_grid(vars(variable), scales = "free")+geom_line()

{
  f <- as.numeric(parameters$Sample.period.grid)
  
  
  
  
  vlist <- lapply(gridList,as.vector)
  lockedvlist <- lapply(lockedGridList,as.vector)
  popvlist <- lapply(gridPopList,as.vector)
  
  vsteps <- seq(from = f, to = length(vlist)*f,by = f)-(f-4)
  lockedvsteps <- seq(from = f, to = length(lockedvlist)*f,by = f)-(f-4)
  popvsteps <- seq(from = f, to = length(popvlist)*f,by = f)-(f-4)
  
  
 sds <- unlist(lapply(vlist,sd))
 lockedsds <- unlist(lapply(lockedvlist,sd))
 popsds <- unlist(lapply(popvlist,sd))
 
 
 rm(vlist)
 rm(lockedvlist)
 rm(popvlist)
 gc()
 
 sd_df <- data.frame(steps = vsteps, sdev = sds)
 lockedsd_df <- data.frame(steps = lockedvsteps, sdev = lockedsds)
 popsd_df <- data.frame(steps = popvsteps, sdev = popsds)
 
 par(mfrow=c(3,1))
  pdf("sdevs_grid.pdf")
 plot(sd_df,type = "l", main = "free nutrients sd")
 plot(lockedsd_df,type = "l", main = "locked nutrients sd")
 plot(popsd_df,type = "l", main = "population size sd")
 dev.off()
 #write.csv(sd_df,"sdevs_steps_0.csv" )
 
}
#length(gridList)


{
  
  
  
  sumPopNute <- data.frame(x = seq(1:gridHW$width),sumNute = rep(0,gridHW$width), sumPop = rep(0,gridHW$width), t = rep(0,gridHW$width))
  
  sumPopNuteList <- list()
  for(i in 1:length(gridlist2)){
    #gridPopList[[i]][1,1] = 0
    for(j in 1:gridHW$width){
      sumPopNute$sumNute[j] <- sum(gridlist2[[i]][,j])
      sumPopNute$sumlockedNute[j] <- as.numeric(sum(lockedGridList[[i]][,j]))
      sumPopNute$sumPop[j] <- sum(gridPopList[[i]][,j])
      sumPopNute$sumSelfing[j] <- sum(selfingGridList[[i]][,j])
      sumPopNute$t[j] <- i
    }
    sumPopNuteList[[i]] <- sumPopNute
  }
  
  
  
  
  
  
  neues <- sumPopNuteList[[1]]
  for(i in 2:(length(gridlist2))){
    neues <- rbind(neues,sumPopNuteList[[i]])
  }
  
  
  {
    # sumPop.mat <- matrix(data = log(neues$sumPop+1), ncol = nrow(neues)/gridHW$width, nrow = gridHW$width)
    # sumPop.mat <- rescale(sumPop.mat,c(0,1))
    # sumPop.mat <- rotate(rotate(rotate(sumPop.mat)))
    # sumPop.col <- rgb(f(sumPop.mat)/255)
    # dim(sumPop.col) <- dim(sumPop.mat)
    # 
    # sumNute.mat <- matrix(data = log(neues$sumNute+1), ncol = nrow(neues)/gridHW$width, nrow = gridHW$width)
    # sumNute.mat <- rescale(sumNute.mat,c(0,1))
    # sumNute.mat <- rotate(rotate(rotate(sumNute.mat)))
    # sumNute.col <- rgb(f(sumNute.mat)/255)
    # dim(sumNute.col) <- dim(sumNute.mat)
    # 
    # sumLock.mat <- matrix(data = neues$sumlockedNute, ncol = nrow(neues)/gridHW$width, nrow = gridHW$width)
    # sumLock.mat <- rescale(sumLock.mat,c(0,1))
    # sumLock.mat <- rotate(rotate(rotate(sumLock.mat)))
    # sumLock.col <- rgb(f(sumLock.mat)/255)
    # dim(sumLock.col) <- dim(sumLock.mat)
    # 
    # sumSelf.mat <- matrix(data = neues$selfing, ncol = nrow(neues)/gridHW$width, nrow = gridHW$width)
    # sumSelf.mat <- rescale(sumSelf.mat,c(0,1))
    # sumSelf.mat <- rotate(rotate(rotate(sumSelf.mat)))
    # sumSelf.col <- rgb(f(sumSelf.mat)/255)
    # dim(sumSelf.col) <- dim(sumSelf.mat)
  }

  

  #ggplot(neues,aes(x,log(sumPop+1), color = t))+geom_point(size = 1, alpha = 1)
  

    

   
# 
# par(mfrow=c(1,1))
# grid.raster(sumPop.col, interpolate =  F)
# grid.raster(sumNute.col, interpolate =  F)
# grid.raster(sumLock.col, interpolate =  F)
# grid.raster(sumSelf.col, interpolate =  F)
#   


  
 
  
  neues$t <- neues$t * as.numeric(parameters$Sample.period.grid)
  
  neues$autoAlloRatio <- ((neues$sumSelfing)/(neues$sumPop))
  neues <- replace(neues, is.na(neues),0)
  neues$sumTotalNute <- as.numeric(neues$sumNute) + as.numeric(neues$sumlockedNute)
  
  
  
  meanPopNute <- data.frame(x = seq(1:parameters$Grid.width),meanNute = rep(0,gridHW$width), meanPop = rep(0,gridHW$width), t = rep(0,gridHW$width))
  
  meanPopNuteList <- list()
  for(i in 1:length(gridlist2)){
    #gridPopList[[i]][1,1] = 0
    for(j in 1:gridHW$width){
      meanPopNute$meanNute[j] <- mean(gridlist2[[i]][,j])
      meanPopNute$meanlockedNute[j] <- mean(lockedGridList[[i]][,j])
      meanPopNute$meanPop[j] <- mean(gridPopList[[i]][,j])
      meanPopNute$meanSelfing[j] <- mean(selfingGridList[[i]][,j])
      meanPopNute$meanTotalNute[j] <- mean(gridlist2[[i]][,j]+lockedGridList[[i]][,j])
      meanPopNute$autoAlloRatio[j] <-  mean(selfingGridList[[i]][,j])/mean(gridPopList[[i]][,j])
      meanPopNute$t[j] <- i
    }
    meanPopNuteList[[i]] <- meanPopNute
  }
  
  meanies <- meanPopNuteList[[1]]
  for(i in 2:(length(gridlist2))){
    meanies <- rbind(meanies,meanPopNuteList[[i]])
  }
  
  # meanies$autoAlloRatio <- rep(0,nrow(meanies))
  # for(i in 1:nrow(meanies)){
  #   meanies$autoAlloRatio[i] <-  mean(neues$sumSelfing[i]/neues$sumPop[i])
  # }
  
  meanies <- replace(meanies, is.na(meanies),0)
  
  meanies$t <- (meanies$t * as.numeric(parameters$Sample.period.grid))-(as.numeric(parameters$Sample.period.grid)-4)
  
  #meanies$autoAlloRatio <- ((meanies$selfing)/(meanies$meanPop))


  
  
  {
    # meanPop.mat <- matrix(data = meanies$meanPop, ncol = nrow(meanies)/gridHW$width, nrow = gridHW$width)
    # meanPop.mat <- rescale(meanPop.mat,c(0,1))
    # meanPop.mat <- rotate(rotate(rotate(meanPop.mat)))
    # meanPop.col <- rgb(f(meanPop.mat)/255)
    # dim(meanPop.col) <- dim(meanPop.mat)
    # 
    # meanNute.mat <- matrix(data = meanies$meanNute, ncol = nrow(meanies)/gridHW$width, nrow = gridHW$width)
    # meanNute.mat <- rescale(meanNute.mat,c(0,1))
    # meanNute.mat <- rotate(rotate(rotate(meanNute.mat)))
    # meanNute.col <- rgb(f(meanNute.mat)/255)
    # dim(meanNute.col) <- dim(meanNute.mat)
    # 
    # meanLock.mat <- matrix(data = meanies$meanlockedNute, ncol = nrow(meanies)/gridHW$width, nrow = gridHW$width)
    # meanLock.mat <- rescale(meanLock.mat,c(0,1))
    # meanLock.mat <- rotate(rotate(rotate(meanLock.mat)))
    # meanLock.col <- rgb(f(meanLock.mat)/255)
    # dim(meanLock.col) <- dim(meanLock.mat)
    # 
    # meanSelf.mat <- matrix(data = meanies$selfing, ncol = nrow(meanies)/gridHW$width, nrow = gridHW$width)
    # meanSelf.mat <- rescale(meanSelf.mat,c(0,1))
    # meanSelf.mat <- rotate(rotate(rotate(meanSelf.mat)))
    # meanSelf.col <- rgb(f(meanSelf.mat)/255)
    # dim(meanSelf.col) <- dim(meanSelf.mat)
  }
  
  
  # grid.raster(meanPop.col, interpolate =  F)
  # grid.raster(meanNute.col, interpolate =  F)
  # grid.raster(meanLock.col, interpolate =  F)
  # grid.raster(meanSelf.col, interpolate =  F)
  
  
  
  
  
  
  
  
  # rm(sumPopNute)
  # rm(sumPopNuteList)
  # rm(neues)
  # 
  # rm(meanPopNute)
  # rm(meanPopNuteList)
  # rm(meanies)
  
  
  
  
}

pdf("ValuesByX_sum.pdf")
{
  
  ggplot(neues, aes(x = x, y = t,fill = sumPop))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle("sum Population for each [xi,ymin:ymax]")+xlab("X coordinate")+ylab("time steps")
  ggplot(neues, aes(x = x, y = t, fill = log(sumNute+1)))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle(" sum Free nutrient for each [xi,ymin:ymax], log(n +1)")+xlab("X coordinate")+ylab("time steps")
  ggplot(neues, aes(x = x, y = t, fill = log(sumlockedNute+1)))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle("sum Locked nutrient for each [xi,ymin:ymax], log(n +1)")+xlab("X coordinate")+ylab("time steps")
  ggplot(neues, aes(x = x, y = t, fill = sumSelfing))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle("sum ([,x]) inds produced from selfing\nfor each [xi,ymin:ymax], ")+xlab("X coordinate")+ylab("time steps")
  ggplot(neues, aes(x = x, y = t, fill = autoAlloRatio))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle("Auto- to allogamy ratio for each [xi,ymin:ymax]\n(sum.auto/sum.allo")+xlab("X coordinate")+ylab("time steps")
  dev.off()
}

{
  pdf("ValuesByX_mean.pdf")
  
  ggplot(meanies, aes(x = x, y = t,fill = meanPop))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle("mean Population for each [xi,ymin:ymax]")+xlab("X coordinate")+ylab("time steps")
  ggplot(meanies, aes(x = x, y = t, fill = log(meanNute+1)))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle(" mean Free nutrient for each [xi,ymin:ymax], log(n +1)")+xlab("X coordinate")+ylab("time steps")
  ggplot(meanies, aes(x = x, y = t, fill = log(meanlockedNute+1)))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle("mean Locked nutrient for each [xi,ymin:ymax], log(n +1)")+xlab("X coordinate")+ylab("time steps")
  ggplot(meanies, aes(x = x, y = t, fill = meanSelfing))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle("mean inds produced from selfing\nfor each [xi,ymin:ymax], ")+xlab("X coordinate")+ylab("time steps")
  ggplot(meanies, aes(x = x, y = t, fill = autoAlloRatio))+geom_raster()+scale_fill_gradientn(colors = colos)+ggtitle("Auto- to allogamy ratio for each [xi,ymin:ymax]\n mean(auto)/mean(allo) ")+xlab("X coordinate")+ylab("time steps")
  dev.off()  
}
gc()

{
  
  
  #rm(gridList)
   sDecay <- function(t, a, b, c) (a*exp(-t/b) +c)
  Decay2 = function(t,a,r)(a*(1-r)^t ) 
  
  plot(sd_df,type = "l") 
  
 
   model <- nls(sdev ~ sDecay(steps,myA,myB, myC), data=sd_df, start=list(myA=256,myB=2.146e+03, myC = 1.382))
   
   
   
   smolSD <- sd_df[2:50,]
   plot(smolSD,type = "l")
   model2 <- nls(sdev ~ Decay2(steps,a,r), data=smolSD, start=list(a = 256,r =0.001))
   lines(smolSD$steps,predict(model2), col = "red")
   #summary(model)
   

 
 pdf("diffusionsdevs.pdf")
 plot(sd_df, type = "l", main = "sd of grid cell nutrient conc over time \n with fitted curve and curve function", ylab = "sd (cell content)", xlab = "time steps")
 lines(sd_df$steps,predict(model), col = "red")
 
#lines(sd_df$steps,predict(lm(sdev~poly(steps,25), data = sd_df)), col = "orange")

 #lines(lm(sd_df$sdev~poly(sd_df$steps,2)), col = "blue")
 text(max(sd_df$steps/2), max(sd_df$sdev)*0.66, expression(f(t)==a^(-t/b) +c))
   plot(vsteps,log(sds), type = "l", main = "ln(sd) of grid cell nutrient conc over time \n with fitted curve and curve function", ylab = paste0(expression(ln(sd), "(cell content)")), xlab = "time steps")
   lines(sd_df$steps,log(predict(model)), col = "red")
   text(max(sd_df$steps/2), log(max(sd_df$sdev))*0.66, expression(f(t)==log( a^(-t/b) +c)))
   plot(vsteps,log(sds), type = "l", main = "ln(sd) of grid cell nutrient conc. over time ", ylab = paste0(expression(ln(sd), "(cell content)")), xlab = "time steps")
   plot(seq(from = f, to = f*165, by = f),diff(log(sds[1:166])), type = "l", main = "diff(ln(sd)) of grid cell nutrient conc over time ", ylab = paste0(expression(diff(log10(sd)), "(cell content)")), xlab = "time steps")
   stats <- read.csv("./PopData/Stats0.csv")
   plot(stats$time_steps,stats$nutrients_total, type = "l", main = "total nutrients over time")
   
   
   # {
   #   par(mfrow=c(2,1))
   #   plot(stats$time_steps,stats$autotrophs, type = "l", xlab = "time steps", ylab = "individuals", main = "Population size over time \n red line: peak")
   #   abline(v = stats$time_steps[which.max(stats$autotrophs)], col = "red")
   #   plot(vsteps,log(sds), type = "l", xlab = "time steps", ylab ="ln(sd) nutrient content", main = "standard deviation \n nutrient content in each cell")
   #   abline(v = stats$time_steps[which.max(stats$autotrophs)], col = "red")
   #   par(mfrow=c(1,1))
   # }
   rm(stats)
 dev.off()
 
 

#plot(vsteps[(length(vsteps)*0.3):length(vsteps)],sds[(length(sds)*0.3):length(sds)], type = "l")
 

rm(model)
rm(sd_df)
rm(vlist)
rm(sds)
rm(vsteps)
rm(sDecay)
rm(f)
rm(i)
 
 #sd_df_2 <- approx(sd_df,n = max(sd_df$steps))
 
 gc()
}






  {
   scaledGridList <- gridList
    selfingRatioList <- selfingGridList
    #notSelfingList <- gridPopList
  for(i in 1:length(selfingGridList)){
    selfingRatioList[[i]] = selfingGridList[[i]]/ (gridPopList[[i]])
    selfingRatioList[[i]] <- replace(selfingRatioList[[i]], is.na(selfingRatioList[[i]]),0)
    #selfingGridList[[i]] =  selfingGridList[[i]]/maxSelfingVal
    #selfingGridList[[i]] = rescale(selfingGridList[[i]], c(0,1))
    #notSelfingList[[i]] = gridPopList[[i]] - selfingGridList[[i]]
  } 
  
  
  
  
   
   for(i in 1:length(gridList)){
     gridList[[i]] <- apply(gridList[[i]], 2, as.numeric)
   }
   
   
   for(i in 1:length(scaledGridList)){
     scaledGridList[[i]] <- rescale(scaledGridList[[i]], to = c(0,1))
   }
  
  
  
  for(i in 1:length(totalNuteList)){
    totalNuteList[[i]] <- apply(totalNuteList[[i]], 2, as.numeric)
  }
  
  
  for(i in 1:length(totalNuteList)){
    totalNuteList[[i]] <- rescale(totalNuteList[[i]], to = c(0,1))
  } 
  
  
  
  
  
  
{
  logDivider = function(x){
    return((1/8)*log10(x))
  }
  logDivider2 = function(x){
    return(log10(x))
  }
  
    logGridList <- gridList
    loglockedGridList <- lockedGridList
    
    #rm(gridList)
    #rm(lockedGridList)
    
    gc()
    
    for(i in 1:length(logGridList)){
      logGridList[[i]] <- logGridList[[i]] +1
      #logGridList[[i]] <- apply(logGridList[[i]],1,log10)
      logGridList[[i]] <- apply(logGridList[[i]],1,logDivider)
    }
    
    
    
    for(i in 1:length(logGridList)){
      logGridList[[i]] <- rescale(logGridList[[i]], to = c(0,1))
    }
    
    
    
    for(i in 1:length(loglockedGridList)){
      loglockedGridList[[i]] <- loglockedGridList[[i]] +1
      #logGridList[[i]] <- apply(logGridList[[i]],1,log10)
      loglockedGridList[[i]] <- apply(loglockedGridList[[i]],1,logDivider)
    }
    
    
    
    for(i in 1:length(loglockedGridList)){
      loglockedGridList[[i]] <- rescale(loglockedGridList[[i]], to = c(0,1))
    }
    
    
    
    # for(i in 1:length(loggridPopList)){
    #   loggridPopList[[i]] <- loggridPopList[[i]] +1
    #   #logGridList[[i]] <- apply(logGridList[[i]],1,log10)
    #   loggridPopList[[i]] <- apply(loggridPopList[[i]],1,logDivider2)
    # }
    
    
    scaledPopList <- gridPopList
    for(i in 1:length(scaledPopList)){
      scaledPopList[[i]] <- scaledPopList[[i]]/HighestPop#rescale(gridPopList[[i]], to = c(0,1))
    }
    
    # Delete Directory
    dir <- "./gridPics"
    if (file.exists(dir)) {
      unlink(dir,recursive = TRUE)
      cat(paste0(dir,"  has been deleted"))
    }
    
    
  }
  
  
  
  
  dir.create(dir)
  rm(dir)
  dir_out_log10 <- file.path( "gridPics/log10")
  dir.create(dir_out_log10, recursive = TRUE)
  
  dir_out_loglock <- file.path( "gridPics/loglock")
  dir.create(dir_out_loglock, recursive = TRUE)
  
  dir_out_pop <- file.path( "gridPics/pop")
  dir.create(dir_out_pop, recursive = TRUE)
  
  
  dir_out_scaled <- file.path( "gridPics/scaled")
  dir.create(file.path("gridPics/gifs"))
  
  dir.create(dir_out_scaled, recursive = TRUE)
  
  dir_out_unscaled <- file.path( "gridPics/unscaled")
  dir.create(dir_out_unscaled, recursive = TRUE)
  
  dir_out_rgb <- file.path( "gridPics/rgb")
  dir.create(dir_out_rgb, recursive = TRUE)
  
  dir_out_totalNute <- file.path( "gridPics/totalNute")
  dir.create(dir_out_totalNute, recursive = TRUE)
  
  dir_out_selfing <- file.path( "gridPics/selfing")
  dir.create(dir_out_selfing, recursive = TRUE)
  
  
  
  gc()
}
#library(jpeg)
library(imager)
library(png)
library(magick)


#selfing
{
  
  
  collist <- list()
  for(i in 1:length(selfingRatioList)){
    
    
    col <- rgb(f(selfingRatioList[[i]])/255)
    dim(col) <- dim(selfingRatioList[[i]])

    collist[[i]] = col}
  gc()
  
  for(i in 1:length(collist)){
    name = paste0(dir_out_selfing,"/image-",1000+i,".png")
    png(name)
    grid.raster(collist[[i]], interpolate=TRUE)
    dev.off()
    
  }
  gc()
  
  
}


#log10_free
{
  
  collist <- list()
  for(i in 1:length(logGridList)){
    
    # r = logGridList[[i]]
    # g = logGridList[[i]]
    # b = logGridList[[i]]
    
    
    
    col <- rgb(f.gray(logGridList[[i]])/255)
    dim(col) <- dim(logGridList[[i]])
    collist[[i]] = col
  }
  gc()
  
  for(i in 1:length(collist)){
    name = paste0(dir_out_log10,"/image-",1000+i,".png")
    png(name)
    grid.raster(collist[[i]], interpolate=TRUE)
    
    dev.off()
  }
  
  #rm(logGridList)
  gc()
  
  ## list file names and read in
  # imgs <- list.files(dir_out_log10, full.names = TRUE)
  # 
  # img_list <- lapply(imgs, image_read)
  # gc()
  # ## join the images together
  # img_joined <- image_join(img_list)
  # gc()
  # 
  # ## animate 
  # img_animated <- image_animate(img_joined, fps = 20)
  # gc()
  # img_animated <- image_rotate(img_animated, 270) 
  # gc()
  # 
  # 
  # 
  # # make.video(
  # #   dname = "./gridPics/log10",
  # #   fname = "owoee.mp4",
  # #   
  # #   verbose = TRUE,
  # #   
  # #   fps = 2
  # #   
  # # )
  # #ffmpeg -framerate 30 -pattern_type glob -i ‘*.jpeg’ \
  # #-c:v libx264 -pix_fmt yuv420p out.mp4
  # #owo <- make.video(img_list,"owo.mp4")
  # 
  # ## save to disk
  # image_write(image = img_animated,
  #             path = "gridPics/gifs/log10.gif")
  gc()
}


#pop
{
  
  collist <- list()
  for(i in 1:length(scaledPopList)){
    
    
    
    col <- rgb(f(scaledPopList[[i]])/255)
    dim(col) <- dim(scaledPopList[[i]])
    collist[[i]] = col}
  gc()
  
  for(i in 1:length(collist)){
    name = paste0(dir_out_pop,"/image-",1000+i,".png")
    png(name)
    grid.raster(collist[[i]], interpolate=TRUE)
    dev.off()
    
  }
  
  # for(i in 1:length(gridPopList)){
  #   fp <- file.path(dir_out_pop, paste0("image-",1000+i,".jpeg"))
  #   jpeg::writeJPEG(logGridList[[i]],fp,quality = 0.5)
  #   #writePNG(gridPopList[[i]], fp, dpi = 32)
  #   
  # }
  #rm(logGridList)
  gc()
  
  ## list file names and read in
  # imgs <- list.files(dir_out_pop, full.names = TRUE)
  # 
  # img_list <- lapply(imgs, image_read)
  # gc()
  # ## join the images together
  # img_joined <- image_join(img_list)
  # gc()
  # 
  # ## animate 
  # img_animated <- image_animate(img_joined, fps = 20)
  # gc()
  # img_animated <- image_rotate(img_animated, 270) 
  # gc()
  # 
  # 
  # 
  # ## save to disk
  # image_write(image = img_animated,
  #             path = "gridPics/gifs/pop.gif")
  # gc()
}


#totalNute
{
  
  
  collist <- list()
  for(i in 1:length(gridlist2)){
    tempMat <- rescale(totalNuteList[[i]],c(0,1))
    
    col <- rgb(f.gray(tempMat)/255)
    dim(col) <- dim(tempMat)
    collist[[i]] = col}
  gc()
  
  for(i in 1:length(collist)){
    name = paste0(dir_out_totalNute,"/image-",1000+i,".png")
    png(name)
    grid.raster(collist[[i]], interpolate=TRUE)
    dev.off()
    
  }
  gc()
  

}








# img_animated
# rm(img_list)
# gc()
# rm(img_joined)

#rgb
{
  collist <- list()
  for(i in 1:length(gridlist2)){
    r = rescale(gridlist2[[i]], to = c(0,1))
    g = rescale(gridlist2[[i]], to = c(0,1))
    b = rescale(gridlist2[[i]], to = c(0,1))
    
  
    g = rescale(gridPopList[[i]], to = c(0,1))
    
    
    col <- rgb(r, g, b)
    dim(col) <- dim(r)
    collist[[i]] = col}
    
  for(i in 1:length(collist)){
    name = paste0(dir_out_rgb,"/image-",1000+i,".png")
    png(name)
    grid.raster(collist[[i]], interpolate=FALSE)
    dev.off()
    
    
  }

  gc()
  
  
}


#img_animated

#loglocked
{
  
  collist <- list()
  for(i in 1:length(loglockedGridList)){

    
    
    
    col <- rgb(f.gray(loglockedGridList[[i]])/255)
    dim(col) <- dim(loglockedGridList[[i]])
    collist[[i]] = col}
  gc()
  
  for(i in 1:length(collist)){
    name = paste0(dir_out_loglock,"/image-",1000+i,".png")
    png(name)
    grid.raster(collist[[i]], interpolate=TRUE)
    dev.off()
    
  }
  
  # for(i in 1:length(loglockedGridList)){
  #   fp <- file.path(dir_out_loglock, paste0("image-",1000+i,"jpeg"))
  #   jpeg::writeJPEG(logGridList[[i]],fp,quality = 0.5)
  #   #writePNG(loglockedGridList[[i]], fp, dpi = 32)
  #   
  # }
  #rm(logGridList)
  gc()
  
  ## list file names and read in
  # imgs <- list.files(dir_out_loglock, full.names = TRUE)
  # 
  # img_list <- lapply(imgs, image_read)
  # gc()
  # ## join the images together
  # img_joined <- image_join(img_list)
  # gc()
  # 
  # ## animate 
  # img_animated <- image_animate(img_joined, fps = 20)
  # gc()
  # img_animated <- image_rotate(img_animated, 270) 
  # gc()
  # 
  # 
  # 
  # ## save to disk
  # image_write(image = img_animated,
  #             path = "gridPics/gifs/loglock.gif")
  # gc()
}











#scaled
{
  
  collist <- list()
  for(i in 1:length(scaledGridList)){
    # r = scaledGridList[[i]]
    # g = scaledGridList[[i]]
    # b = scaledGridList[[i]]
    
    
    
    col <- rgb(f.gray(scaledGridList[[i]])/255)
    dim(col) <- dim(scaledGridList[[i]])
    collist[[i]] = col}
  gc()
  
  for(i in 1:length(collist)){
    name = paste0(dir_out_scaled,"/image-",1000+i,".png")
    png(name)
    grid.raster(collist[[i]], interpolate=TRUE)
    dev.off()
    
  }
  
  
  # for(i in 1:length(scaledGridList)){
  #   fp <- file.path(dir_out_scaled, paste0("nutrientGrid",1000+i,".png"))
  #   
  #   writePNG(scaledGridList[[i]], fp, dpi = 32)
  #   
  # }
  
  
  # ## list file names and read in
  # imgs <- list.files(dir_out_scaled, full.names = TRUE)
  # img_list <- lapply(imgs, image_read)
  # 
  # ## join the images together
  # img_joined <- image_join(img_list)
  # gc()
  # ## animate at 2 frames per second
  # img_animated <- image_animate(img_joined, fps = 20)
  # gc()
  # img_animated <- image_rotate(img_animated, 270) 
  # 
  # 
  # 
  # ## save to disk
  # image_write(image = img_animated,
  #             path = "gridPics/gifs/scaled.gif")
  gc()
}
## view animated image
#img_animated






