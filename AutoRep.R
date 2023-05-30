#AUTOREP


{
  library(tidyverse)
  rm(list=ls())
  gc()
  POI <- data.frame(y = rep(8,8), x = c(8,9,10,11,12,13,14,15))
  simname <- "mat08_mig_8e4"
  
  thisdir = r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat08_mig_8e4)"
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