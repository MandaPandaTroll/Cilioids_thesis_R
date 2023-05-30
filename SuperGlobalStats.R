#SUPERGLOBALSTATS


library(tidyverse)
#


rm(list = ls())
thisdir = r"(C:\Users\gushanamc\RStudioStuff\Cilioids_thesis_R\AllSieved)"
setwd(thisdir)


list.dirs(thisdir)


globalPopList <- list()


popPaths <-  Sys.glob("./*/globalPopStats*.csv")

experimentNames <- list.files("./","mat*")
matCoefs <- c(rep(0.2,3),rep(0.4,3),rep(0.8,3))
pmigs <- c(rep(c(2e-4,4e-4,8e-4),3))
for (i in 1:9){
  globalPopList[[i]] <- read.csv(popPaths[i] )
  globalPopList[[i]]$experiment.ID <- experimentNames[i]
  globalPopList[[i]]$maturity.coefficient = matCoefs[i]
  globalPopList[[i]]$p.migration = pmigs[i]
}  

superPop.df <- bind_rows(globalPopList)
superPop.df<- superPop.df[,-1]
meanPops.df <- superPop.df%>%select(experiment.ID,time_steps,mean.pop,SD)

superpop.gat <- superPop.df%>%select(-mean.pop,-SD,-SE, -experiment.ID)
superpop.gat <- superpop.gat%>%gather(key = "run", value = "N", - maturity.coefficient, -p.migration, -time_steps)

ggplot(superpop.gat,aes(time_steps ,N, colour = factor(p.migration)))+geom_point(size = 0.25)+facet_grid("maturity.coefficient")

ggplot(superpop.gat,aes(time_steps ,N, colour = factor(maturity.coefficient)))+geom_point(size = 0.25)+facet_grid("p.migration")

ewe <- group_split(superpop.gat%>%group_by(experiment.ID))
superpop.gat <- superpop.gat%>%gather(key = "experiment", value = "value", -experiment.ID)



globalSelfList <- list()


selfPaths <-  Sys.glob("./*/globalSelfStats*.csv")

for (i in 1:9){
  globalSelfList[[i]] <- read.csv(selfPaths[i])
  globalSelfList[[i]]$experiment.ID <- experimentNames[i]
  globalSelfList[[i]]$maturity.coefficient = matCoefs[i]
  globalSelfList[[i]]$p.migration = pmigs[i]
}  

superSelf.df <- bind_rows(globalSelfList)
superSelf.df<- superSelf.df[,-1]
meanSelf.df <- superSelf.df%>%select(experiment.ID,time_steps,mean.self,SD)




globalfreeNuteList <- list()


freeNutePaths <-  Sys.glob("./*/globalFreeNuteStats*.csv")

for (i in 1:9){
  globalfreeNuteList[[i]] <- read.csv(freeNutePaths[i] )
  globalfreeNuteList[[i]]$experiment.ID <- experimentNames[i]
  globalfreeNuteList[[i]]$maturity.coefficient = matCoefs[i]
  globalfreeNuteList[[i]]$p.migration = pmigs[i]
}  

superfreeNute.df <- bind_rows(globalfreeNuteList)
superfreeNute.df<- superfreeNute.df[,-1]
meanfreeNute.df <- superfreeNute.df%>%select(experiment.ID,time_steps,mean.freenute,SD)



pdf("GlobalStats_AllExperiments.pdf")


ggplot(meanPops.df,aes(time_steps,mean.pop, colour = experiment.ID))+geom_ribbon(aes(x = time_steps, ymin = mean.pop-SD, ymax = mean.pop+SD,fill = experiment.ID), alpha = 0.1, linetype = 0)+geom_line(aes(linetype = experiment.ID), linewidth = 0.5)+labs(y = "mean.global.population.size",title ="Mean global population size & SD(ribbon)\nAll experiments")+theme_bw()+scale_linetype_manual(values = c(1,1,1,1,1,1,1,1,1))+theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))


ggplot(meanSelf.df,aes(time_steps,mean.self, colour = experiment.ID))+geom_ribbon(aes(x = time_steps, ymin = mean.self-SD, ymax = mean.self+SD,fill = experiment.ID), alpha = 0.2, linetype = 0)+geom_line(aes(linetype = experiment.ID), linewidth = 0.5)+labs(y = "mean.global.selfing.ratio",title ="Mean global selfing ratios & SD(ribbon)\nAll experiments")+theme_bw()+scale_linetype_manual(values = c(1,1,1,1,1,1,1,1,1))+theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))




ggplot(meanfreeNute.df,aes(time_steps,mean.freenute, colour = experiment.ID))+geom_ribbon(aes(x = time_steps, ymin = mean.freenute-SD, ymax = mean.freenute+SD,fill = experiment.ID), alpha = 0.2, linetype = 0)+geom_line(aes(linetype = experiment.ID), linewidth = 0.5)+labs(y = "mean.global.free.nutrients",title ="Mean global free nutrients & SD(ribbon)\nAll experiments")+theme_bw()+scale_linetype_manual(values = c(1,1,1,1,1,1,1,1,1))+theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))



dev.off()




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




library(growthcurver)

plot(globalPopList[[1]]$time_steps,globalPopList[[1]]$mean.pop, type = "l")
test.gc.fit <- growthcurver::SummarizeGrowth(globalPopList[[1]]$time_steps,globalPopList[[1]]$mean.pop)

plot(test.gc.fit)
summary(test.gc.fit)

logro <- function(K, N0, r ,t){
  N <- K / (1 + ((K - N0) / N0) * exp(-r * t))
  return(N)
}



groFitList <- list()
groFitList.big <- list()

for(i in 1:9){
  groFitList[[i]] = SummarizeGrowth(globalPopList[[i]]$time_steps,globalPopList[[i]]$mean.pop)
  
}

algro <- list()
for(i in 1:9){
  tempgrof <- list()
  for(j in 1:10){
    tempgrof[[j]] <- globalPopList[[i]][,j+2]
    
  }
  groFitList.big[[i]] <- tempgrof
}

for(i in 1:9){
  for(j in 1:10){
    groFitList.big[[i]][[j]] <- SummarizeGrowth(globalPopList[[1]]$time_steps, groFitList.big[[i]][[j]])
    
  }
}

K.list.outer <- list()

for(i in 1:9){
  K.list.inner <- list()
  for(j in 1:10){
    K.list.inner[[j]] <- groFitList.big[[i]][[j]]$vals$k
    
  }
  K.list.outer[[i]] <- K.list.inner
}

r.list <- list()
l = 1
for(i in 1:9){
  for(j in 1:10){
    r.list [[l]] <- groFitList.big[[i]][[j]]$vals$r
    l = l+1
  }

}

kvpar <- data.frame(
  ID = seq(1:90), maturity.coefficient = c(rep(0.2,30),rep(0.4,30),rep(0.8,30)),p.migration = rep(c(rep(0.2,10),rep(0.4,10),rep(0.8,10) ),3 ), carrying.capacity = unlist(K.list.outer), intrinsic.growth.rate = unlist(r.list))








par(mfrow = c(1,2))
plot(kvpar$maturity.coefficient, kvpar$carrying.capacity)
plot(kvpar$p.migration, kvpar$carrying.capacity)

ggplot(kvpar,aes(maturity.coefficient,carrying.capacity, colour = factor(p.migration)))+geom_point()+geom_line()

ggplot(kvpar,aes(maturity.coefficient,intrinsic.growth.rate, colour = factor(p.migration)))+geom_point()+geom_line()

boxplot(intrinsic.growth.rate~p.migration+maturity.coefficient,kvpar)

GGally::ggpairs(kvpar%>%select(-ID))


plot(intrinsic.growth.rate~p.migration+maturity.coefficient,kvpar)
summary(lm(intrinsic.growth.rate~p.migration+maturity.coefficient, kvpar))

par(mfrow=c(3,3))
hist(kvpar$intrinsic.growth.rate[which(kvpar$p.migration == 0.2 & kvpar$maturity.coefficient == 0.2)], breaks = 10)
hist(kvpar$intrinsic.growth.rate[which(kvpar$p.migration == 0.4 & kvpar$maturity.coefficient == 0.2)], breaks = 10)
hist(kvpar$intrinsic.growth.rate[which(kvpar$p.migration == 0.8 & kvpar$maturity.coefficient == 0.2)], breaks = 10)
hist(kvpar$intrinsic.growth.rate[which(kvpar$p.migration == 0.2 & kvpar$maturity.coefficient == 0.4)], breaks = 10)
hist(kvpar$intrinsic.growth.rate[which(kvpar$p.migration == 0.4 & kvpar$maturity.coefficient == 0.4)], breaks = 10)
hist(kvpar$intrinsic.growth.rate[which(kvpar$p.migration == 0.8 & kvpar$maturity.coefficient == 0.4)], breaks = 10)
hist(kvpar$intrinsic.growth.rate[which(kvpar$p.migration == 0.2 & kvpar$maturity.coefficient == 0.8)], breaks = 10)
hist(kvpar$intrinsic.growth.rate[which(kvpar$p.migration == 0.4 & kvpar$maturity.coefficient == 0.8)], breaks = 10)
hist(kvpar$intrinsic.growth.rate[which(kvpar$p.migration == 0.8 & kvpar$maturity.coefficient == 0.8)], breaks = 10)


plot(cluster::agnes(kvpar))
plot(prcomp(dist(kvpar)))

kvpar$experiment.id <- c(rep("mat2mig2",10),rep("mat2mig4",10),rep("mat2mig8",10),rep("mat4mig2",10),rep("mat4mig4",10),rep("mat4mig8",10),rep("mat8mig2",10),rep("mat8mig4",10),rep("mat8mig8",10))

library(factoextra)
forpca <- kvpar%>%select(maturity.coefficient,p.migration,intrinsic.growth.rate, carrying.capacity)

forpca.scal <- scale(forpca)

cor.test(forpca$intrinsic.growth.rate,forpca$maturity.coefficient)


le.pca <- prcomp(forpca.scal)
factoextra::fviz_pca(le.pca)
fviz_pca_var(le.pca)

corrplot::corrplot(forpca.scal)
par(mfrow = c(3,3))
for(i in 1:9){
  plot(groFitList[[i]])
}


dev.off()










for (i in 1:9){
  selfrat.poi.list[[i]] <- read.csv(selfrat.poi.paths[i])
  selfrat.poi.list[[i]]$experiment.ID <- experimentNames[i]
}  

selfrat.poi.df <- bind_rows(selfrat.poi.list)



ggplot(selfrat.poi.df,aes(time_steps,C2,colour = experiment.ID))+geom_line()

meanselfrat.poi.df <- selfrat.poi.df%>%select(experiment.ID,time_steps,mean.self,SD)


allpoppath <-  Sys.glob("./*/pop_allruns*.csv")

alglobalPopList <- list()

for(i in 1:9){
  alglobalPopList[[i]] <- read.csv(allpoppath[i])
}

C1_1 <-aggregate(alglobalPopList[[1]]$C1, list(alglobalPopList[[1]]$time_steps), mean)$x

plot(C1_1)



totalnutepath <-  Sys.glob("./*/totalnute_allruns*.csv")

totalnutelist <- list()



totalnutelist_1dim <- get1dims("totalnute")
lockednutelist_1dim <- get1dims("lockednute_allruns")
globalfreeNuteList_1dim <- get1dims("freenute_allruns")
globalPopList_1dim <- get1dims("pop_allruns")
globalSelfList_1dim <- get1dims("self_allruns")
selfratiolist_1dim <- get1dims("selfratio_allruns")


