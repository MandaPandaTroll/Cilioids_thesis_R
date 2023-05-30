#AutoGrid

{
  library(tidyverse)
  rm(list=ls())
  gc()
  POI <- data.frame(y = rep(8,8), x = c(8,9,10,11,12,13,14,15))
  simname <- "mat08_mig_8e4"
  #
  thisdir = r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat08_mig_8e4)"
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