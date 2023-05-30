#AutoStat

#mat02_mig_2e4
#mat02_mig_4e4
#mat02_mig_8e4
#mat04_mig_2e4
#mat04_mig_4e4
#mat04_mig_8e4
#mat08_mig_2e4
#mat08_mig_4e4
#mat08_mig_8e4
{
  
  library(tidyverse)
  rm(list=ls())
  gc()
  
  {
    simname <- "mat08_mig_8e4"
    #
    thisdir = r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat08_mig_8e4)"
    setwd(thisdir)
    parameters <- read.csv("./1/Cilioids_thesis_Data/params.txt")
    parameters <- spread(parameters, key = Parameter, value = Value)
    
    parameters$Maturity.coefficient = as.numeric(parameters$Maturity.coefficient)
    parameters$P.migration = as.numeric(parameters$P.migration)
    
    
    
    
    
    #setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/PopData/ManyRuns)")
    paths <- Sys.glob("*/*/*/Stats0*.csv")
    L <- Map(read.csv, paths)
    
    #manyRunStats<- lapply(list.files(pattern = "Stats*"),read.csv,header = TRUE)
    #setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/)")
  }
  
  shortestStat= 1e6
  for(i in 1:length(L)){
    L[[i]]$run <- i
    if(nrow(L[[i]]) < shortestStat){
      shortestStat = nrow(L[[i]])
    }
    
  }
  
  shortestStat = 1920
  
  for(i in 1:length(L)){
    
    if(nrow(L[[i]]) > shortestStat){
      L[[i]] <- L[[i]][1:shortestStat,]
    }
    
  }
  
  
  peakGlobalPop <- data.frame(run = seq(1:10), peakPop = rep(0,10))
  for(i in 1:10){
    peakGlobalPop[i,2] = max(L[[i]]$autotrophs)
  }
  write.csv(peakGlobalPop,paste0("peakPops",simname,".csv"))
  #plot(peakGlobalPop)
  #hist(peakGlobalPop$peakPop, breaks = 8)
  
  big.pop.stat <- data.frame(time_steps =L[[1]]$time_steps,"RUN0" = L[[1]]$autotrophs,"RUN1" = L[[2]]$autotrophs,"RUN2" = L[[3]]$autotrophs,"RUN3" = L[[4]]$autotrophs,"RUN4" = L[[5]]$autotrophs,"RUN5" = L[[6]]$autotrophs,"RUN6" = L[[7]]$autotrophs,"RUN7" = L[[8]]$autotrophs,"RUN8" = L[[9]]$autotrophs,"RUN9" = L[[10]]$autotrophs)
  
  
  big.pop.stat$mean.pop <- big.pop.stat%>%select(-time_steps)%>%rowMeans()
  big.pop.stat <- big.pop.stat%>%rowwise()%>%mutate(SD = sd(c(RUN0,RUN1,RUN2,RUN3,RUN4,RUN5,RUN6,RUN7,RUN8,RUN9)))
  big.pop.stat <- big.pop.stat%>%mutate(SE = SD/sqrt(10))
  
  
  
  big.self.stat <- data.frame(time_steps =L[[1]]$time_steps,"RUN0" = L[[1]]$globalSelfingRatio,"RUN1" = L[[2]]$globalSelfingRatio,"RUN2" = L[[3]]$globalSelfingRatio,"RUN3" = L[[4]]$globalSelfingRatio,"RUN4" = L[[5]]$globalSelfingRatio,"RUN5" = L[[6]]$globalSelfingRatio,"RUN6" = L[[7]]$globalSelfingRatio,"RUN7" = L[[8]]$globalSelfingRatio,"RUN8" = L[[9]]$globalSelfingRatio,"RUN9" = L[[10]]$globalSelfingRatio)
  
  
  big.self.stat$mean.self <- big.self.stat%>%select(-time_steps)%>%rowMeans()
  big.self.stat <- big.self.stat%>%rowwise()%>%mutate(SD = sd(c(RUN0,RUN1,RUN2,RUN3,RUN4,RUN5,RUN6,RUN7,RUN8,RUN9)))
  big.self.stat <- big.self.stat%>%mutate(SE = SD/sqrt(10))
  
  
  
  big.freenute.stat <- data.frame(time_steps =L[[1]]$time_steps,"RUN0" = L[[1]]$nutrients_free,"RUN1" = L[[2]]$nutrients_free,"RUN2" = L[[3]]$nutrients_free,"RUN3" = L[[4]]$nutrients_free,"RUN4" = L[[5]]$nutrients_free,"RUN5" = L[[6]]$nutrients_free,"RUN6" = L[[7]]$nutrients_free,"RUN7" = L[[8]]$nutrients_free,"RUN8" = L[[9]]$nutrients_free,"RUN9" = L[[10]]$nutrients_free)
  
  
  big.freenute.stat$mean.freenute <- big.freenute.stat%>%select(-time_steps)%>%rowMeans()
  big.freenute.stat <- big.freenute.stat%>%rowwise()%>%mutate(SD = sd(c(RUN0,RUN1,RUN2,RUN3,RUN4,RUN5,RUN6,RUN7,RUN8,RUN9)))
  big.freenute.stat <- big.freenute.stat%>%mutate(SE = SD/sqrt(10))
  
  
  big.lockednute.stat <- data.frame(time_steps =L[[1]]$time_steps,"RUN0" = L[[1]]$nutrients_locked,"RUN1" = L[[2]]$nutrients_locked,"RUN2" = L[[3]]$nutrients_locked,"RUN3" = L[[4]]$nutrients_locked,"RUN4" = L[[5]]$nutrients_locked,"RUN5" = L[[6]]$nutrients_locked,"RUN6" = L[[7]]$nutrients_locked,"RUN7" = L[[8]]$nutrients_locked,"RUN8" = L[[9]]$nutrients_locked,"RUN9" = L[[10]]$nutrients_locked)
  
  
  big.lockednute.stat$mean.lockednute <- big.lockednute.stat%>%select(-time_steps)%>%rowMeans()
  big.lockednute.stat <- big.lockednute.stat%>%rowwise()%>%mutate(SD = sd(c(RUN0,RUN1,RUN2,RUN3,RUN4,RUN5,RUN6,RUN7,RUN8,RUN9)), percentile.05 = quantile(c(RUN0,RUN1,RUN2,RUN3,RUN4,RUN5,RUN6,RUN7,RUN8,RUN9),0.05), percentile.95 = quantile(c(RUN0,RUN1,RUN2,RUN3,RUN4,RUN5,RUN6,RUN7,RUN8,RUN9),0.95))
  big.lockednute.stat <- big.lockednute.stat%>%mutate(SE = SD/sqrt(10))
  
  
  
  
  write.csv(big.pop.stat,paste0("globalPopStats_",simname,".csv"))
  write.csv(big.self.stat,paste0("globalSelfStats_",simname,".csv"))
  write.csv(big.freenute.stat,paste0("globalFreeNuteStats_",simname,".csv"))
  write.csv(big.lockednute.stat,paste0("globalLockedNuteStats_",simname,".csv"))
  
  
  
}



pdf(paste0("popAndSelf_pretty_",simname,".pdf"))

poppePlot <- ggplot(big.pop.stat, aes(time_steps))+
  geom_line(aes(y = RUN0), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN1), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN2), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN3), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN4), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN5), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN6), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN7), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN8), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN9), linewidth = 0.05, alpha = 0.25)+theme_bw()+labs(y = "global.population",title ="Global population over 10 runs\nwith mean line (red)\n", subtitle = paste0("Maturity coefficient = ", parameters$Maturity.coefficient, ", P(Migration) = ", parameters$P.migration))+coord_cartesian(ylim = c(0,(max(big.pop.stat$mean.pop)*1.25)))

poppePlot+ geom_line(aes(y = mean.pop), linewidth = 0.5, alpha = 0.8, colour = "red")



ggplot(big.pop.stat,aes(time_steps,mean.pop))+geom_line()+geom_linerange(aes(x = time_steps, ymin = mean.pop-SD, ymax = mean.pop+SD), alpha = 0.08)+theme_bw()+labs(y = "mean.global.population",title ="Mean global population over 10 runs\nwith SD (grey)\n", subtitle = paste0("Maturity coefficient = ", parameters$Maturity.coefficient, ", P(Migration) = ", parameters$P.migration))+coord_cartesian(ylim = c(0,(max(big.pop.stat$mean.pop)*1.25)))




ggplot(big.self.stat, aes(time_steps))+
  geom_line(aes(y = RUN0), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN1), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN2), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN3), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN4), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN5), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN6), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN7), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN8), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN9), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = mean.self), linewidth = 0.5, alpha = 0.8, colour = "red")+theme_bw()+labs(y = "global.selfing.ratio",title ="Global selfing ratio over 10 runs\nwith mean line (red)\n", subtitle = paste0("Maturity coefficient = ", parameters$Maturity.coefficient, ", P(Migration) = ", parameters$P.migration))+coord_cartesian(ylim = c(0,(max(big.self.stat$mean.self)*1.25)))


ggplot(big.self.stat,aes(time_steps,mean.self))+geom_line()+geom_linerange(aes(x = time_steps, ymin = mean.self-SD, ymax = mean.self+SD), alpha = 0.08)+theme_bw()+labs(y = "mean.global.selfing.ratio",title ="Mean global selfing ratio over 10 runs\nwith SD (grey)\n", subtitle = paste0("Maturity coefficient = ", parameters$Maturity.coefficient, ", P(Migration) = ", parameters$P.migration))+coord_cartesian(ylim = c(0,(max(big.self.stat$mean.self)*1.25)))




ggplot(big.freenute.stat, aes(time_steps))+
  geom_line(aes(y = RUN0), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN1), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN2), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN3), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN4), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN5), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN6), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN7), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN8), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = RUN9), linewidth = 0.05, alpha = 0.25)+
  geom_line(aes(y = mean.freenute), linewidth = 0.5, alpha = 0.8, colour = "red")+theme_bw()+labs(y = "global.free.nutrients",title ="Global free nutrients over 10 runs\nwith mean line (red)\n", subtitle = paste0("Maturity coefficient = ", parameters$Maturity.coefficient, ", P(Migration) = ", parameters$P.migration))+coord_cartesian(ylim = c(0,max(big.freenute.stat$mean.freenute)*1.25))


ggplot(big.freenute.stat,aes(time_steps,mean.freenute))+geom_line()+geom_linerange(aes(x = time_steps, ymin = mean.freenute-SD, ymax = mean.freenute+SD), alpha = 0.08)+theme_bw()+labs(y = "mean.global.free.nutrients",title ="Mean global free nutrients over 10 runs\nwith SD (grey)\n", subtitle = paste0("Maturity coefficient = ", parameters$Maturity.coefficient, ", P(Migration) = ", parameters$P.migration))+coord_cartesian(ylim = c(0,max(big.freenute.stat$mean.freenute)*1.25))

ggplot(big.freenute.stat,aes(time_steps,log10(mean.freenute)))+geom_line()+geom_linerange(aes(x = time_steps, ymin = log10(mean.freenute-SD), ymax = log10(mean.freenute+SD)), alpha = 0.08)+theme_bw()+labs(y = "Log10(mean.global.free.nutrients)",title ="Log 10 Mean global free nutrients, over 10 runs\nwith SD (grey)\n", subtitle = paste0("Maturity coefficient = ", parameters$Maturity.coefficient, ", P(Migration) = ", parameters$P.migration))




dev.off()
