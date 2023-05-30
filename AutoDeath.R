#AutoDeath

{
  library(tidyverse)
  rm(list = ls())
  gc()
  
  
  POI <- data.frame(y = rep(8,8), x = c(8,9,10,11,12,13,14,15))
  
  simname <- "mat08_mig_8e4"
  #
  thisdir = r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat08_mig_8e4)"
  setwd(thisdir)
  
  #setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/PopData/ManyRuns)")
  
  deathEventPathList <- list()
  
  
  {
    shortest <- 1e6
    for(i in 1:10){
      deathEventPathList[[i]] = Sys.glob(paste0(i,"/*/*/deathEvents*.csv"))
      
      if(length(deathEventPathList[[i]]) < shortest){
        shortest = length(deathEventPathList[[i]])
      }
    }
    
    
    for(i in 1:10){
      deathEventPathList[[i]] <- deathEventPathList[[i]][1:shortest]
    }
    
    deathEventMetaList <- list()
    for (i in 1:10){
      deathEventMetaList[[i]] <- Map(read.csv,header = TRUE,deathEventPathList[[i]])
      
    }
    
  }
  
  
  for(i in 1:10){
    deathEventMetaList[[i]] = bind_rows(deathEventMetaList[[i]])
  }
  
  for(i in 1:10){
    deathEventMetaList[[i]]$run = i
  }
  
  
  deathEventList.onlyPOI <- list()
  
  for(i in 1:10){
    deathEventList.onlyPOI[[i]] = deathEventMetaList[[i]]%>%filter(pos_x > 7 & pos_y == 8)
  }
  
  rm(deathEventMetaList)
  gc()
  
  
  
  filename = "deathEvents_1dim"
  if(!dir.exists("./deathEvents_1dim")){
    dir.create("./deathEvents_1dim")
  }
  setwd("./deathEvents_1dim")
  
  for(i in 1:10){
    write.csv(deathEventList.onlyPOI[[i]], paste0(filename,"_",simname,"_run",i,".csv"))
    gc()
  }
  
  setwd(thisdir)
}