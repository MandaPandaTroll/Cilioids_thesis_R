#onlynutes



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
  library(imager)
  library(png)
  library(magick)
  #library(stringr) 
  #library(spatstat)
}


rm(list=ls())

gc()


{
  
  
  simname <- "_8_8"
  freedir <- r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat08_mig_8e4\1\Cilioids_thesis_Data\NutrientGridData)" 
  lockeddir <- r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat08_mig_8e4\1\Cilioids_thesis_Data\LockedNutrientGridData)" 
  
  thisdir = r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat08_mig_8e4\1\Cilioids_thesis_Data)"
  setwd(thisdir)
  
  writeGridPics <- function(destination, inlist, interp = FALSE, specificLength = FALSE, samples = 0, hmap = TRUE){
    
    
    
    outlength = 0
    
    if(specificLength == TRUE){
      outlength = samples
    }else{
      outlength = length(inlist)
    }
    
    collist <- list()
    for(i in 1:outlength){
      
      if(hmap == TRUE){
        col <- rgb(f(inlist[[i]])/255)
      }else{
        col <- rgb(f.gray(inlist[[i]])/255)
      }
      
      dim(col) <- dim(inlist[[i]])
      
      collist[[i]] = col
    }
    gc()
    
    for(i in 1:length(collist)){
      name = paste0(destination,"/image-",1000+i,".png")
      png(name)
      
      
      grid.raster(collist[[i]], interpolate=interp)
      
      dev.off()
      
    }
    gc()
    
    
  }
  
  
  annotatePics <- function(destination, samples){
    namelist <- list.files(destination)
    if(samples > length(namelist)){
      samples = length(namelist)
    }
    
    for(i in 1:samples){
      name = paste0(destination,"/image-",1000+i,".png")
      outpic <- image_read(name)
      outpic <- image_annotate(outpic, paste0(i*32),color = "white", size = 16)
      outpic <- image_annotate(outpic, paste0("Mcoef: ", parameters$Maturity.coefficient, " | Pmig: ", parameters$P.migration), gravity = "southeast", color = "white", boxcolor =  "black", size = 16)
      
      
      image_write(image = outpic,path = name, depth = 16, quality = 100)
    }
    gc()
    
  }
  
  
  
  
  
  
  setwd(freedir)
  freelist<- lapply(lapply(list.files(pattern = "gridData*"),read.csv,header = FALSE),as.matrix)
  gc()
  
  setwd(lockeddir)
  lockedlist<- lapply(lapply(list.files(pattern = "gridData_locked*"),read.csv,header = FALSE),as.matrix)
  gc()
  
  
  
  
  setwd(thisdir)
  
  
  
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
  
  
  
  
  for(i in length(freelist)){
    dimnames(freelist[[i]]) <- gridDims
  }
  
  for(i in length(lockedlist)){
    dimnames(lockedlist[[i]]) <- gridDims
  }
  
  

  
  

  
  
  rotate <- function(x) t(apply(x, 2, rev))
  grayPal <- colorRampPalette(grey.colors(1024))
  coloPal <-  colorRampPalette(plasma(1024))
  colos <- coloPal(1024)
  grays <- grayPal(1024)
  colos[1] <- "#000000"
  grays[1] <- "#000000"
  
  f <- colorRamp( colos)
  f.gray <- colorRamp(grays)
  
  
  HighestFree <- max(unlist(lapply(freelist,max)))
  HighestLocked <- max(unlist(lapply(lockedlist,max)))
  HighestAll <- max(HighestFree, HighestLocked)
  
  
  
  for(i in 1:length(freelist)){
    freelist[[i]] <- freelist[[i]]/HighestAll
    lockedlist[[i]] <- lockedlist[[i]]/HighestAll
  }
  
  
  
  dir <- "./gridPics"
  if (file.exists(dir)) {
    unlink(dir,recursive = TRUE)
    cat(paste0(dir,"  has been deleted"))
  }
  
  
  dir.create(dir)
  rm(dir)
  
  dir_out_free <- file.path( paste0("gridPics/free",simname))
  dir.create(dir_out_free, recursive = TRUE)
  
  
  dir_out_locked <- file.path( paste0("gridPics/locked",simname))
  dir.create(dir_out_locked, recursive = TRUE)
  
  
  
  
  writeGridPics(dir_out_free, freelist, interp = T, specificLength = T, samples = 960, hmap = T)
  
  annotatePics(dir_out_free, 960)
  
  writeGridPics(dir_out_locked, lockedlist, interp = T, specificLength = T, samples = 960, hmap = T)
  
  annotatePics(dir_out_locked, 960)
}