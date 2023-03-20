#Nutrient grid analysis

library(dplyr)
library(magick)
library(png)
if(!require(scales)){
  install.packages("scales", dependencies=TRUE)
  library(scales)
}
#library(stringr) 


rm(list=ls())
dev.off()
gc()

 



# example file names in an incorrect order
#file_names <- list.files(pattern = "gridData*")
#file_names[order(as.numeric(str_extract(file_names, '(?<=gridData)[0-9]*')))]
#--------



#new_names <- paste0("gridData",(1000 + as.numeric(str_extract(file_names, '(?<=gridData)[0-9]*'))),'.csv')
# [1] "1100.asc" "1002.asc" "1202.asc" "1301.asc" "1003.asc"  

#file.rename(file_names, new_names)


{
  
  setwd("./NutrientGridData")
  
  gridList<- lapply(list.files(),read.csv,header = FALSE)
  setwd("/Users/Amanda/RstudioStuff/Cilioids_thesis_R/")
  parameters <- read.csv("params.txt")
  gridDims <- list(as.character(seq(1:parameters$Value[1])),as.character(seq(1:parameters$Value[2])))
  #gridList <- (lapply(list.files(pattern = "gridData*"), read.csv, header = FALSE))
  
  
  
  gridList <- lapply(gridList,as.matrix)
  for(i in length(gridList)){
    dimnames(gridList[[i]]) <- gridDims
  }
  
  f <- as.numeric(parameters$Value[9])
  vlist <- lapply(gridList,as.vector)
  vsteps <- seq(from = f, to = length(vlist)*f,by = f)
 sds <- unlist(lapply(vlist,sd))
 sd_df <- data.frame(steps = vsteps, sdev = unlist(sds))
 plot(sd_df,type = "l")
 write.csv(sd_df,"sdevs_steps_0.csv" )
}
{
   sDecay <- function(t, a, b, c) (a*exp(-t/b) +c)
   
   
   model <- nls(sdev ~ sDecay(steps,myA,myB, myC), data=sd_df, start=list(myA=256,myB=2.146e+03, myC = 1.382))
   
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
  
  for(i in 1:length(gridList)){
    gridList[[i]] <- apply(gridList[[i]], 2, as.numeric)
  }
  
  
  for(i in 1:length(scaledGridList)){
    scaledGridList[[i]] <- rescale(scaledGridList[[i]], to = c(0,1))
  }
  
  
  
  
  
  
  
  
  logDivider = function(x){
    return((1/8)*log10(x))
  }
  logGridList <- gridList
  for(i in 1:length(logGridList)){
    logGridList[[i]] <- logGridList[[i]] +1
    #logGridList[[i]] <- apply(logGridList[[i]],1,log10)
    logGridList[[i]] <- apply(logGridList[[i]],1,logDivider)
  }
  
  
  
  for(i in 1:length(logGridList)){
    logGridList[[i]] <- rescale(logGridList[[i]], to = c(0,1))
  }
  
  # Delete Directory
  dir <- "./gridPics"
  if (file.exists(dir)) {
    unlink(dir,recursive = TRUE)
    cat(paste0(dir,"  has been deleted"))
  }
  
  dir.create(dir)
  rm(dir)
  dir_out_log10 <- file.path( "gridPics/log10")
  dir.create(dir_out_log10, recursive = TRUE)
  
  
  dir_out_scaled <- file.path( "gridPics/scaled")
  dir.create(file.path("gridPics/gifs"))
  
  dir.create(dir_out_scaled, recursive = TRUE)
  
  dir_out_unscaled <- file.path( "gridPics/unscaled")
  dir.create(dir_out_unscaled, recursive = TRUE)
  
  gc()
}

#log10
{
  for(i in 1:length(logGridList)){
    fp <- file.path(dir_out_log10, paste0("nutrientGrid",1000+i,".png"))
    
    writePNG(logGridList[[i]], fp, dpi = 32)
   
  }
  
  
  ## list file names and read in
  imgs <- list.files(dir_out_log10, full.names = TRUE)
  img_list <- lapply(imgs, image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate 
  img_animated <- image_animate(img_joined, fps = 10)
  img_animated <- image_rotate(img_animated, 270) 
  ## view animated image
  
  
  ## save to disk
  image_write(image = img_animated,
              path = "gridPics/gifs/log10.gif")
  gc()
}
img_animated
rm(img_list)
rm(img_joined)



#scaled
{
  for(i in 1:length(scaledGridList)){
    fp <- file.path(dir_out_scaled, paste0("nutrientGrid",1000+i,".png"))
    
    writePNG(scaledGridList[[i]], fp, dpi = 32)
    
  }
  
  
  ## list file names and read in
  imgs <- list.files(dir_out_scaled, full.names = TRUE)
  img_list <- lapply(imgs, image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- image_animate(img_joined, fps = 20)
  img_animated <- image_rotate(img_animated, 270) 
  
  
  
  ## save to disk
  image_write(image = img_animated,
              path = "gridPics/gifs/scaled.gif")
}
## view animated image
img_animated



{
  for(i in 1:length(gridList)){
    fp <- file.path(dir_out_unscaled, paste0("nutrientGrid",1000+i,".png"))
    
    writePNG(gridList[[i]], fp)
    
  }
  
  
  ## list file names and read in
  imgs <- list.files(dir_out_unscaled, full.names = TRUE)
  img_list <- lapply(imgs, image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- image_animate(img_joined, fps = 5)
  img_animated <- image_rotate(img_animated, 270) 
  ## view animated image
  img_animated
  
  ## save to disk
  image_write(image = img_animated,
              path = "gridPics/gifs/unscaled.gif")
}



