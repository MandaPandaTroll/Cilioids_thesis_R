#singleGridpicCreator



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
dev.off()
gc()

#library(jpeg)
setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R)")
parameters <- read.csv("params.txt")
parameters <- spread(parameters, key = Parameter, value = Value)
parameters$Grid.height <- as.numeric(parameters$Grid.height)
parameters$Grid.width <- as.numeric(parameters$Grid.width)
parameters$Nutrients.total <- as.numeric(parameters$Nutrients.total)
parameters$numCells = parameters$Grid.height*parameters$Grid.width
parameters$meanExpectedNutrients <- parameters$Nutrients.total/parameters$numCells
parameters$Sample.period.grid <- as.numeric(parameters$Sample.period.grid)





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
    outpic <- image_annotate(outpic, paste0(i*as.numeric(parameters$Sample.period.grid)),color = "white", size = 16)
    outpic <- image_annotate(outpic, paste0("Mcoef: ", parameters$Maturity.coefficient, " | Pmig: ", parameters$P.migration), gravity = "southeast", color = "white", boxcolor =  "black", size = 16)
    
    
    image_write(image = outpic,path = name, depth = 16, quality = 100)
  }
  gc()
  
}









setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/gridPopData)")
gridPopList<- lapply(lapply(list.files(pattern = "gridPopData*"),read.csv,header = FALSE),as.matrix)
gc()

setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/selfingGridData)")
selfingGridList<- lapply(lapply(list.files(pattern = "selfingGridData*"),read.csv,header = FALSE),as.matrix)
gc()




setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R)")
setwd("./LockedNutrientGridData")
lockedGridList <- lapply(lapply(list.files(pattern = "gridData_locked*"),read.csv,header = FALSE),as.matrix)



setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/NutrientGridData)")
gridList<- lapply(lapply(list.files(pattern = "gridData*"),read.csv,header = FALSE),as.matrix)
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



for(i in length(gridPopList)){
  dimnames(gridPopList[[i]]) <- gridDims
}

for(i in length(selfingGridList)){
  dimnames(selfingGridList[[i]]) <- gridDims
}

#gridList <- lapply(gridList,as.matrix)
for(i in length(gridList)){
  dimnames(gridList[[i]]) <- gridDims
}

for(i in length(lockedGridList)){
  dimnames(lockedGridList[[i]]) <- gridDims
}



gridlist2 <- gridList[1:length(gridPopList)]

totalNuteList <- gridlist2




selfingRatioList <- selfingGridList


for(i in 1:length(gridPopList)){
  selfingRatioList[[i]] = (selfingGridList[[i]]/ (gridPopList[[i]]))
  selfingRatioList[[i]] = replace(selfingRatioList[[i]], is.na(selfingRatioList[[i]]),0)
  
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

for(i in 1:length(gridlist2)){
  totalNuteList[[i]] = totalNuteList[[i]]+lockedGridList[[i]]
  
}




HighestSelf <- max(unlist(lapply(selfingGridList,max)))
HighestSelfRatio <- max(unlist(lapply(selfingRatioList,max)))
HighestPop <- max(unlist(lapply(gridPopList,max)))


HighestFreeNute <- max(unlist(lapply(gridlist2,max)))
HighestLockedNute <- max(unlist(lapply(lockedGridList,max)))
HighestToteNute <- max(unlist(lapply(totalNuteList,max)))




  scaledGridList <- gridList
  
  #notSelfingList <- gridPopList
  
  
  
  
  
  
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
  
  
  
  scaledPopList <- gridPopList
  for(i in 1:length(scaledPopList)){
    scaledPopList[[i]] <- scaledPopList[[i]]/HighestPop#rescale(gridPopList[[i]], to = c(0,1))
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
    
    
   
    
    # Delete Directory
    dir <- "./gridPics"
    if (file.exists(dir)) {
      unlink(dir,recursive = TRUE)
      cat(paste0(dir,"  has been deleted"))
    }
    
    
  
  
  
  
  
  dir.create(dir)
  rm(dir)
  
  dir_out_selfing <- file.path( "./gridPics/selfing")
  dir.create(dir_out_selfing, recursive = TRUE)
  
  
  dir_out_pop <- file.path( "./gridPics/pop")
  dir.create(dir_out_pop, recursive = TRUE)
  
  dir_out_log10 <- file.path( "./gridPics/log10")
  dir.create(dir_out_log10, recursive = TRUE)
  
  dir_out_loglock <- file.path( "./gridPics/loglock")
  dir.create(dir_out_loglock, recursive = TRUE)
  

  
  
  dir_out_scaled <- file.path( "./gridPics/scaled")
  dir.create(file.path("./gridPics/gifs"))
  
  dir.create(dir_out_scaled, recursive = TRUE)
  
  dir_out_unscaled <- file.path( "./gridPics/unscaled")
  dir.create(dir_out_unscaled, recursive = TRUE)
  
  dir_out_rgb <- file.path( "gridPics/rgb")
  dir.create(dir_out_rgb, recursive = TRUE)
  
  dir_out_totalNute <- file.path( "gridPics/totalNute")
  dir.create(dir_out_totalNute, recursive = TRUE)
  

  
  
  
  gc()
}

writeGridPics(dir_out_selfing, selfingRatioList, interp = T, specificLength = T, samples = 960)

annotatePics(dir_out_selfing, 960)

writeGridPics(dir_out_pop, scaledPopList, interp = T, specificLength = T, samples = 429, hmap = F)

annotatePics(dir_out_pop, 429)










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
