#Statsanalysis
library(tidyverse)
library(car)
library(zoo)

rm(list=ls())
dev.off()
gc()







{
  rm(list=ls())
  gc()
  
  {
    simname <- "mat02_mig_8e4"
    #
    thisdir = r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat02_mig_8e4)"
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
  
  
  
  
  write.csv(big.pop.stat,paste0("globalPopStats_",simname,".csv"))
  write.csv(big.self.stat,paste0("globalSelfStats_",simname,".csv"))
  write.csv(big.pop.stat,paste0("globalFreeNuteStats_",simname,".csv"))
  
  
  
}
  
  
  # library(growthcurver)
  # gc.fit <- SummarizeGrowth(big.pop.stat$time_steps,big.pop.stat$mean
  
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



























pdf(paste0("popnself_allrun_",simname,".pdf"))
ggplot(big.self.stat%>%select(-mean)%>%gather(key = "run", value = "global.selfing.ratio",-time_steps), aes(time_steps,global.selfing.ratio, fill = run))+geom_line(linewidth = 0.05, alpha = 0.5)+theme_bw()+ggtitle(simname)

 ggplot(big.pop.stat%>%select(-mean)%>%gather(key = "run", value = "global.population.size",-time_steps), aes(time_steps,global.population.size, fill = run))+geom_line(linewidth = 0.05, alpha = 0.5)+theme_bw()+ggtitle(simname)

 dev.off()


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
pdf(paste0("popnSelfing_",simname,".pdf"))

ggplot(big.pop.stat%>%gather(key = "run", value = "global.population.size", -time_steps),aes(time_steps,global.population.size))+geom_line(linewidth = 0.05, alpha = 0.5)+theme_bw()+facet_wrap("run")+ggtitle(paste0("global population plot\n",simname))#+geom_line(aes(y = rollmedian(global.population.size, 33, na.pad = T)), colour = "red")

ggplot(big.self.stat%>%gather(key = "run", value = "globalSelfingRatio", -time_steps),aes(time_steps,globalSelfingRatio))+geom_line(linewidth = 0.05, alpha = 0.5)+theme_bw()+ggtitle(paste0("global selfing ratio plot\n",simname))+facet_wrap(vars(run))#+geom_line(aes(y = rollmedian(globalSelfingRatio, 33, na.pad = T)), colour = "red")

dev.off()



biggo <- bind_rows(L)

biggo <- biggo%>%group_by(run)

# Specify data frame
means.df <- aggregate(biggo,by = list(biggo$time_steps), mean)
means.df <- means.df%>%select(-Group.1,-time_seconds,-run )
means.df$experiment <- simname
names(means.df)[2] = "global.population"
write.csv(means.df,paste0(simname,"_meanStats.csv"))


pdf(paste0(simname,"_meanpopnself.pdf"))
ggplot(means.df%>%select(time_steps,global.population,globalSelfingRatio)%>%gather(key = "variable", value = "value",-time_steps),aes(time_steps,value))+geom_line(linewidth = 0.05, alpha = 0.5)+theme_bw()+facet_grid( rows ="variable", scales = "free")+ggtitle(paste0("Mean over 10 runs\nPopulation size & Global selfing ratio plot\n",simname))
dev.off()




plot(autotrophs~time_steps, means.df, type = "l")
plot(globalSelfingRatio~time_steps, means.df, type = "l")

logro <- function(P0, t, r, K){
  out = K / (1+((K-P0)/P0)*exp(-r*t) )
  return(out)
}


forPres.df <- data.frame(t = seq(1:1000), N =logro(10,1:1000,0.02,1000) )

plot(forPres.df, type = "l", main = "Logistic growth model", lwd = 3)
abline(h = 1000, col = "red", lt = 2, lwd = 3)


logro2 <- function(L, t, k){
  out = L / (1+exp(-(k*t)))
  return(out)
}



hist(biggo$autotrophs, breaks = 256)




ggplot(biggo%>%select(time_steps,autotrophs,gametes,globalSelfingRatio,run)%>%gather(key = "variable", value = "value",-time_steps),aes(time_steps,value,fill = run))+geom_line(linewidth = 0.05, alpha = 0.5)+theme_bw()+facet_wrap("variable", scales = "free")

plot(means.df$time_steps,(means.df$globalSelfingRatio), type = "l")

smallermene <- means.df%>%select(time_steps,autotrophs,gametes,meanGeneration,globalSelfingRatio)


ggplot(smallermene%>%gather(key = "variable", value = "value", -time_steps),aes(time_steps,value))+facet_wrap("variable",scales = "free")+geom_line()

meanSelf.fit <- nls(globalSelfingRatio~logro2(max(means.df$globalSelfingRatio),time_steps,myk), start = list(myk = 1e-7), data = means.df)
lines(means.df$time_steps, predict(meanSelf.fit,means.df$time_steps), col = "red")
summary(meanSelf.fit)

colos <- list()
for(i in 1:length(manyRunStats)){
  colos[[i]] <- paste0("Inds_run_",i)
  }



manyPop <- list()
  

for(i in 1:length(manyRunStats)){
  manyPop[[i]] <- manyRunStats[[i]]$autotrophs
}
manyPop.df <- as.data.frame(manyPop)

names(manyPop.df) <- colos

manyPop.df$time_steps <- manyRunStats[[i]]$time_steps

ggplot(gather(manyPop.df,key="key",value= "value", -time_steps), aes(time_steps,value, colour = key))+geom_line()






plot(logro2(1,seq(1:256),0.1,2048), type = "l")



testey = unlist(testey)
plot(testey, type = "l")
plot(diff(testey,32,1), type = "l")



rm(list=ls())
gc()
dev.off()

setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R/)")
stats <- read.csv("./PopData/Stats0.csv")
parameters <- read.csv("params.txt")

parameters <- spread(parameters, key = Parameter, value = Value)
#stats <- stats[2:nrow(stats),]
summary(stats)

par(mfrow=c(2,1))
qqnorm(stats$autotrophs)
plot(stats$time_steps,stats$autotrophs,type = "l")
plot(stats$time_steps,stats$gametes,type = "l")
plot(stats$time_steps,stats$nutrients_free,type = "l", ylim = c(0,max(stats$nutrients_free)))
plot(stats$autotrophs,stats$gametes,type = "h")
plot(stats$time_steps,stats$globalSelfingRatio,type = "l")

scaledstats <- as.data.frame(scale(stats))

plot(scaledstats$time_steps,scaledstats$globalSelfingRatio, type = "l")
lines(scaledstats$time_steps,scaledstats$nutrients_locked, col = "red")
lines(scaledstats$time_steps,scaledstats$autotrophs, col = "blue")
plot(diff(scaledstats$globalSelfingRatio), type = "l")
lines(diff(scaledstats$nutrients_locked)*10, col = "red")
plot(scaledstats$time_steps, scaledstats$nutrients_free)
points(scaledstats$time_steps, scaledstats$globalSelfingRatio, col = "blue")

plot(globalSelfingRatio~nutrients_locked,data = stats)
abline(lm(globalSelfingRatio~nutrients_locked,data = stats), col = "red")

plot(stats$time_steps,stats$nutrients_free,type = "l", ylim = c(0,max(stats$nutrients_free)))
lines(stats$time_steps,stats$nutrients_locked, col = "red")
lines(stats$time_steps,stats$globalSelfingRatio*max(stats$nutrients_free)*5, col = "blue")

plot(stats$time_steps,stats$gametes_total,type = "l")
plot(diff(stats$gametes_total),type = "l")

hist((stats$autotrophs[1:nrow(stats)]), breaks = 256)
boxplot(stats$autotrophs[1:nrow(stats)])
shapiro.test((slice_sample(stats[1000:nrow(stats),],n = 1000)$autotrophs))

hist(stats$gametes[1:nrow(stats)])
shapiro.test((slice_sample(stats[1000:nrow(stats),],n = 5000)$gametes))
plot((stats$autotrophs[1000:nrow(stats)]),stats$globalSelfingRatio[1000:nrow(stats)])



tester.df <- stats%>%select(time_steps,globalSelfingRatio,autotrophs)%>%filter(time_steps > 16000)
ggplot(tester.df,aes(autotrophs,globalSelfingRatio))+geom_count()




library(GGally)

#generate the pairs plot
ggpairs(stats[1:nrow(stats),]%>%select(time_steps,nutrients_free, nutrients_locked, autotrophs,gametes,globalSelfingRatio))

ggpairs(stats%>%select(time_steps,nutrients_free, nutrients_locked, autotrophs,gametes,globalSelfingRatio, zygotesFormed,gametes_total))



logro.fit <- nls(autotrophs~logro(P0 = autotrophs[1], t = time_steps, myr, myK), start = list(myr = 0.1, myK = max(stats$autotrophs)), data = stats)

summary(logro.fit)

plot(autotrophs~time_steps, stats, type = "l")
lines(stats$time_steps, predict(logro.fit, stats$time_steps), col = "red")





lines(stats$time_steps,max(stats$autotrophs)*stats$globalSelfingRatio, col="red")





plot(globalSelfingRatio~autotrophs, stats)

self.lm <- lm(globalSelfingRatio~autotrophs,stats)

abline(self.lm, col = "red")

self.glm <- (glm(globalSelfingRatio~autotrophs,"gaussian",data = stats))
abline(self.glm, col = "green")
summary(self.lm)
summary(self.glm)


plot(diff(stats$gametes_total,1,1), type = "l")

peakN <- which(stats$autotrophs == max(stats$autotrophs))

afterpeak <- data.frame(time_steps = stats$time_steps[peakN:nrow(stats)], autotrophs = stats$autotrophs[peakN:nrow(stats)])

plot(afterpeak, type = "l")







selfpeakindex <- which(stats$globalSelfingRatio == max(stats$globalSelfingRatio))
selfpeakindex <- selfpeakindex[length(selfpeakindex)]
afterpeak.selfing <- stats[selfpeakindex*8:nrow(stats),]
beforepeak <- stats[1:(selfpeakindex*5),]
plot(afterpeak.selfing$autotrophs, afterpeak.selfing$globalSelfingRatio)
plot(smooth(afterpeak.selfing$time_steps), smooth(afterpeak.selfing$globalSelfingRatio), type = "l")




plot(beforepeak$time_steps,beforepeak$globalSelfingRatio)
plot(diff(afterpeak$autotrophs,64,1), type = "l")
plot(smooth(diff(afterpeak$autotrophs,64,1), kind ="3R"), type = "l")

plot(smooth.spline(diff(afterpeak$autotrophs,64,1)), type = "l")

afterpeak <- approx(afterpeak, n = max(afterpeak$time_steps))

afterpeak <- as.data.frame(afterpeak)

names(afterpeak) <- c("time_steps", "autotrophs")

plot(afterpeak, type = "l")


gugu <- smooth.spline(afterpeak)

abline(v = gugu$x[amax(gugu$y)],col = "red" )


finosc <- data.frame(time_steps = stats$time_steps[which(stats$time_steps > 55000)], autotrophs = stats$autotrophs[which(stats$time_steps > 55000)])

finosc$autotrophs <- smooth.spline(rescale(finosc$autotrophs, c(-1,1)))$y

locmax <- finosc$time_steps[amax(finosc$autotrophs)]
library(spectral)

plot(finosc)
abline(v = locmax, col = "red")

locmaxdiffe <- diff(locmax)

summary(locmaxdiffe)

newFreq <- data.frame(time_steps =afterpeak$time_steps[65:nrow(afterpeak)], dN =  diff(afterpeak$autotrophs,64,1))

newFreq <- approx(newFreq, n = max(newFreq$time_steps))

newFreq$y <- rescale(newFreq$y, c(-1,1))

plot(newFreq, type = "l")

spec.fft(y = newFreq$y)

plot( amax(afterpeak$autotrophs))

plot(spec.fft(afterpeak$autotrophs))



finosc$time_steps <- finosc$time_steps-min(finosc$time_steps)

finosc$autotrophs <- finosc$autotrophs-mean(finosc$autotrophs)
finosc$autotrophs <- scales::rescale(finosc$autotrophs,c(-1,1))


plot(finosc)

expando <- function(x, rango){
  
  (pmax(pmin((x*2),rango),-rango))
  
}


for(i in 1:max(finosc$time_steps)){
  if(finosc$autotrophs[i] > 0){
    finosc$autotrophs[i] <- 1
  }
  if(finosc$autotrophs[i] < 0 ){
    finosc$autotrophs[i] <- -1
  }
}

plot(finosc$time_steps, ((finosc$autotrophs)), type = "l")

lines(sin( (seq(1:max(finosc$time_steps))+512)/210 ), col = "red")

interpOsc <- data.frame(time_Steps = seq(1:max(finosc$time_steps)), autotrophs = approx(finosc$autotrophs, n = max(finosc$time_steps)))
interpOsc <- interpOsc[,-2]

names(interpOsc) <- c("time_steps", "autotrophs")

plot(interpOsc)



triangle = function(t, p){
  xt <- 2*(abs( 2*( (t/p)-floor(t/p + 1/2) )  ) ) -1
}

plot(seq(0,4,0.5),(triangle(seq(0,4,0.5),1)), type = "p")

plot(interpOsc$time_steps, round(interpOsc$autotrophs*1.5), type = "l")

simp <- data.frame(time_steps = interpOsc$time_steps, autotrophs = round(interpOsc$autotrophs*1.5))
plot(simp$time_steps[1:3500],simp$autotrophs[1:3500], type = "l")


lines(round(triangle(simp$time_steps+700,1500)^2)/2, col = "red")

plot( simp$autotrophs^2, type ="l")


plot (simp, type = "l")

popSin <- function(x, diver, offset){
  return(sin((x+diver)/offset))
  }


popSin.fit <- nls(autotrophs~popSin(time_steps,mydiv,myoff),data = finosc, start = list(mydiv = 210, myoff = 512))
?nls()
plot(sin(seq(pi,2*pi, 0.1))
     )
diffefreqlong <- data.frame(dSteps = seq(256, max(afterpeak$time_steps),256), dAuto = diff(afterpeak$autotrophs,256,1))

peaks <- diffefreqlong%>%filter(dAuto > 750)
plot(diffefreqlong$dAuto)
fouriefreq <- spec.fft(diffefreqlong)
plot(spectral::spec.fft(diffefreqlong))
plot(fouriefreq)

plot(fouriefreq)
plot(abs(fouriefreq/length(fouriefreq)))
plot(fft(diffefreqlong), type = "l")

plot(stats$time_steps,stats$autotrophs, type = "l")
plot(stats$time_steps,stats$globalSelfingRatio, type = "l")
plot((stats$autotrophs[which(stats$globalSelfingRatio > 0)]),(stats$globalSelfingRatio)[which(stats$globalSelfingRatio > 0)])

bd.df <- data.frame(time_steps = stats$time_steps[-1], n = stats$autotrophs[-1],births = stats$zygotesFormed[-1], deaths = stats$deaths_individual[-1], birth.rate = diff(stats$zygotesFormed,1,1), death.rate = diff(stats$deaths_individual,1,1), zyg.per.gam = stats$zygotesFormed[-1]/stats$gametes_total[-1], b.per.d = stats$zygotesFormed[-1]/stats$deaths_individual[-1], abGro = stats$zygotesFormed[-1]-stats$deaths_individual[-1])

bd.df <- replace(bd.df, is.na(bd.df),0)







plot(n~time_steps, bd.df)
plot(death.rate~time_steps,bd.df, type = "l")
plot(abGro~time_steps,bd.df)
lines(n~time_steps,bd.df, col = "red")






plot(bd.df$time_steps,bd.df$b.per.d)


{
  
  bd.decay <- function(t,n0, lambda, c){
    out = n0*exp(-lambda*t) +c
  }
  
  #plot(bd.df$generation, bd.decay(t = bd.df$generation, n0 = 25.82602, lambda = 0.69877, c = 1.02786))
  
  
  model <- nls(bd ~ bd.decay(time_steps,myn0,mylambda, myc), data=bd.df, start=list(myn0=19,mylambda = 0.00439, myc = 1.049 ))
  summary(model)
  
  lines(bd.df$time_steps,predict(model,bd.df$time_steps), col = "red")
  
  
  }

#expgroFunc <- function(t,n0,r){
#  N = n0*(1+r)^t
#  return(N)
#  }

#expart <- data.frame(t = stats$time_steps[1:50],n = stats$autotrophs[1:50] )




{
  circle <- function(x,r){
    return(sqrt(x^2 - r^2))
  }
  
  colScale <- viridis::plasma(16)
  
  png("sdCircle.png")
  
  
  cdf <- pnorm(seq(0,1.95,0.05),0,0.5)
  docol <- colorRamp(colScale)
  docol <- colorRampPalette(colScale)
  par( mar = c(4,4,4,4))
  plot(0.15*cos(seq(0,2*pi,0.01)),0.15*sin(seq(0,2*pi,0.01)), xlim = c(-2,2), ylim = c(-2,2), type = "n", xlab = "", ylab = "")
  sd = 0.5
  
  scaledcdf <- scales::rescale(cdf,0,1)
  
  for(i in 1:4){
    lines((i*sd)*cos(seq(0,2*pi,0.01)),(i*sd)*sin(seq(0,2*pi,0.01)), lwd = 2) 
    
    
  }
  
  dev.off()
  
  randAngos <- runif(10000,0,pi)
  randMags <- rnorm(10000,0,0.5)
  
  randresultos <- data.frame(x = cos(randAngos)*randMags,y=sin(randAngos)*randMags)
  resultomag <- data.frame(mag =sqrt((randresultos$x^2)+(randresultos$y^2)))
  
  ggplot(randresultos, aes(x,y))+geom_density_2d()
  ggplot(randresultos, aes(x))+geom_density()
  plot(cos(randAngos)*randMags,sin(randAngos)*randMags)
  
  morex <- (randresultos[which(abs(randresultos$x) > 1),])
  morey <- (randresultos[which(abs(randresultos$y) > 1),])
  
  morexy <- length(morex$y[which(morex$y > 1)])+ length(morey$x[which(morey$x > 1)])
  
  plot(cos(runif(10000,0,pi))*rnorm(10000,0,0.2),sin(runif(10000,0,pi))*rnorm(10000,0,0.2))
  
  png("normCurve.png")
  plot(seq(-2,2,0.001),dnorm(seq(-2,2,0.001),0,0.15), type = "l", col = "red")
  lines(seq(-2,2,0.001),dnorm(seq(-2,2,0.001),0,0.25), col = "green")
  lines(seq(-2,2,0.001),dnorm(seq(-2,2,0.001),0,0.5), col = "blue")
  lines()
  
  dev.off()
}

logPhaseIndex <- which(stats$autotrophs == max(stats$autotrophs))

logGroPart <- stats[1:logPhaseIndex+1,]

lambs <- rep(0,nrow(logGroPart)-1)
for(i in 1:(length(lambs))){
  lambs[i] = logGroPart$autotrophs[i+1]/logGroPart$autotrophs[i] 
}
lambs <- replace(lambs, is.na(lambs),1)
plot(lambs, type = "l")

rmax = log(max(lambs))

max(lambs)

logGroFunc <- function(k, p0, r, t){
  p = k/(1+((k-p0)/p0)*exp(-r*t))
  return(p)
}

plot(logGroPart$autotrophs)

logro.fit <- nls(autotrophs~logGroFunc(k = max(logGroPart$autotrophs),p0 = logGroPart$autotrophs[1], myr , logGroPart$time_steps), data = logGroPart, start = list(myr = rmax))

summary(logro.fit)

par(mfrow=c(1,2))
plot(autotrophs~time_steps, data = logGroPart)
lines(logGroPart$time_steps,predict(logro.fit,logGroPart$time_steps), col = "red")

plot(logGroPart$meanGeneration,logGroPart$autotrophs)
ggplot(stats,aes(meanGeneration, autotrophs))+geom_count()
ggplot(stats,aes(time_steps, meanGeneration))+geom_point()
par(mfrow=c(1,1))






hist(stats$autotrophs,breaks = 256)
hist(stats$gametes, breaks = 256)



plot(stats$time_steps,stats$autotrophs, type = "l")
plot(stats$time_steps,stats$gametes, type = "l")
plot(stats$meanGeneration,stats$autotrophs, type = "l")
plot(stats$autotrophs,stats$gametes)

ggplot(stats,aes(autotrophs, gametes))+geom_count()

abline(lm(gametes~autotrophs,stats),col = "red")
plot(zygotesFormed~gametes_total,stats)
zygbygam.lm <- lm(zygotesFormed~gametes_total,stats)
abline(zygbygam.lm,col = "red")
summary(zygbygam.lm)

indGamFun <- function(x,x0,k, L){
  
  gam = L/(1+exp(-k*(x-x0)))
  return(gam)
}



plot(indGamFun(x = seq(1:6000),x0 = 1 ,k = 0.001, L = 150),col = "red")

indGamModel <- nls(gametes~ indGamFun(autotrophs, myx0, myk,myl), data = stats, start = list(myx0 = 1, myk = 0.001, myl = 150))

meanNums <- stats%>%group_by(meanGeneration)%>%select(meanGeneration,time_steps,autotrophs,gametes)

meanNums2 <- group_by(meanNums, autotrophs) %>% summarize(ga = mean(gametes), au = mean(autotrophs))

indGamModel_meano <- nls(ga~ indGamFun(au, myx0, myk,myl), data = meanNums2, start = list(myx0 = 1, myk = 0.001, myl = 150))


plot(ga~au, meanNums2)
lines(meanNums2$au,predict(indGamModel_meano,meanNums2$au), col = "red")

plot(stats$autotrophs,stats$gametes)
lines(stats$autotrophs,predict(indGamModel,stats$autotrophs), col = "red")

summary(indGamModel)


nls(bd ~ bd.decay(generation,myn0,mylambda, myc), data=bd.df, start=list(myn0=32,mylambda = 0.7, myc = 1 ))
summary(model)

{
  pdf("Population.pdf")
  plot(stats$time_steps,stats$autotrophs, type = "l", xlab = "time steps", ylab = "individuals", main = "x")
  dev.off()
}


#changes <- stats%>%distinct(nutrients_total, .keep_all = T)%>%select(time_steps)
#changes <- changes$time_steps[2:nrow(changes)]
#changes <- changes-(stats$time_steps[2]-stats$time_steps[1])

plot(stats$time_steps,stats$autotrophs, type = "l")
hist(stats$nutrients_total)
plot(stats$time_steps,stats$nutrients_total)

diffsize = 1
diffs <- data.frame(
  time_steps =stats$time_steps[1:(nrow(stats)-diffsize)],
  nutrients_total = diff(stats$nutrients_total, diffsize),
  nutrients_free = diff(stats$nutrients_free, diffsize),
  nutrients_locked = diff(stats$nutrients_locked, diffsize),
  nutrients_autotroph = diff(stats$nutrients_autotroph, diffsize),
  nutrients_gamete = diff(stats$nutrients_gamete, diffsize),
  autotrophs = diff(stats$autotrophs, diffsize),
  gametes = diff(stats$gametes, diffsize),
  deaths_individual = diff(stats$deaths_individual, diffsize),
  deaths_gamete = diff(stats$deaths_gamete, diffsize),
  zygotesFormed = diff(stats$zygotesFormed, diffsize)
  )

plot(stats$deaths_individual/stats$zygotesFormed)

#changes_diff <- diffs%>%distinct(nutrients_total, .keep_all = T)%>%select(time_steps)
#changes_diff <- changes_diff$time_steps[1:nrow(changes_diff)]
#changes_diff <- changes_diff-(diffs$time_steps[2]-diffs$time_steps[1])

changes <- diff(stats$nutrients_total) 
changes <- which(changes !=  0)

hist(diffs$deaths_individual)
par(mfrow=c(2,1))
pdf("deathsAndZygotes.pdf", width = (1920/16), height = (1080/64))
{
  par(cex = 1)
  plot(diffs$time_steps,diffs$deaths_gamete, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  
  plot(diffs$time_steps,diffs$deaths_individual, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  
  plot(diffs$time_steps,diffs$zygotesFormed, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  plot(diffs$time_steps,diffs$nutrients_total, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  dev.off()
}
par(mfrow=c(1,1))
lm.birth <-lm(zygotesFormed~time_steps, data = stats)
plot(stats$time_steps,stats$zygotesFormed)
zygotesFormed <- stats$zygotesFormed
time_steps <- stats$time_steps[1:800]

plot(stats$time_seconds[1:800],stats$autotrophs[1:800])
t <- time_steps
a = 0.5
r = 0.5
K = mean(stats$autotrophs)
P = 0
plot(t, (K*P*exp(r*t))/(K+P*(exp(r*t)-1)), col = "red", type = "l") 
autotrophsLogGrowth <- stats$autotrophs[1:800]


stats2 <- stats[1:2000,]



coef(lm(logit(autotrophs/400)~time_seconds,data=stats2))

uwu<-nls(autotrophs/400~phi1/(1+exp(-(phi2+phi3*time_seconds))),
            start=list(phi1=mean(stats2$autotrophs)/400,phi2=-1.061175042,phi3=0.002028524  ),data=stats2,trace=TRUE)

summary(uwu)



#set parameters
phi1<-coef(uwu)[1]
phi2<-coef(uwu)[2]
phi3<-coef(uwu)[3]
x<-c(min(stats2$time_seconds):max(stats2$time_seconds)) #construct a range of x values bounded by the data
y<-phi1/(1+exp(-(phi2+phi3*x))) 
predict<-data.frame(x,y) 
ggplot(data=stats2,aes(x=time_seconds,y=autotrophs))+
  geom_point()+theme_bw()+
  geom_line(data=predict,aes(x=x,y=y*400), colour = "red")


 par(mfrow=c(2,1))

pdf("pops_nutes.pdf", width = (1920/16), height = (1080/64))
{
  par(cex = 2)
  plot(stats$time_steps,stats$autotrophs, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  
  plot(stats$time_steps,stats$gametes, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  
  plot(stats$time_steps,stats$nutrients_free, type = "l")
  #lines(stats$time_steps,stats$autotrophs*31)
  abline(v = stats$time_steps[changes], col = "red")
  
  plot(stats$time_steps,stats$nutrients_locked, type = "l")
  #lines(stats$time_steps,stats$autotrophs*22)
  abline(v = stats$time_steps[changes], col = "red")
  
  plot(stats$time_steps,stats$nutrients_autotroph, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  plot(stats$time_steps,stats$nutrients_gamete, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  plot(stats$time_steps,stats$nutrients_total, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  dev.off()
}


pdf("regplots.pdf", width = (1920/16), height = (1080/32))
{
  plot(stats$time_steps,stats$deaths_gamete, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  
  plot(stats$time_steps,stats$deaths_individual, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  
  plot(stats$time_steps,stats$zygotesFormed, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  
  plot(stats$time_steps,stats$nutrients_total, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  dev.off()
}

par(mfrow=c(3,1))
{
  plot(stats$time_steps,stats$autotrophs, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  plot(stats$time_steps,stats$gametes, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  plot(stats$time_steps,stats$nutrients_total, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
}



{
  plot(diffs$time_steps,diffs$autotrophs, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  plot(diffs$time_steps,diffs$gametes, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  plot(stats$time_steps,stats$nutrients_total, type = "l")
  abline(v = stats$time_steps[changes], col = "red")
  }

{
  plot(diffs$time_steps,diffs$nutrients_free, type = "l")
  abline(v = changes, col = "red")
  plot(diffs$time_steps,diffs$nutrients_locked, type = "l")
  abline(v = changes, col = "red") 
  plot(diffs$time_steps,diffs$nutrients_total, type = "l")
  abline(v = changes, col = "red")
}

{
  plot(diffs$time_steps,diffs$nutrients_autotroph, type = "l")
  abline(v = changes, col = "red")
  plot(diffs$time_steps,diffs$nutrients_gamete, type = "l")
  abline(v = changes, col = "red") 
  plot(diffs$time_steps,diffs$nutrients_total, type = "l")
  abline(v = changes, col = "red")
}





cor.test(stats$nutrients_total[1:(nrow(stats)-diffsize)],diffs$gametes)

plot(stats$gametes,stats$nutrients_total)
plot(stats$autotrophs,stats$nutrients_total)
plot(stats$autotrophs+stats$gametes,stats$nutrients_total)

plot(stats$time_steps,stats$nutrients_autotroph, type = "l")
abline(v = changes, col = "red")
plot(stats$time_steps,stats$nutrients_gamete, type = "l")
abline(v = changes, col = "red")
plot(stats$time_steps,stats$nutrients_total)
abline(v = changes, col = "red")
abline(lm(nutrients_total~time_steps, data = stats))


plot(stats$time_steps,stats$autotrophs, type = "l")
plot(stats$time_steps,stats$gametes, type = "l")
plot(stats$time_steps,stats$nutrients_locked, type = "l")

par(mfrow=c(1,1))

plot(stats$autotrophs,stats$gametes, type = "l")
plot(stats$nutrients_locked,stats$gametes)


par(mfrow=c(3,1))
plot(stats$time_seconds,stats$nutrients_total, type = "l")
plot(stats$time_seconds,stats$gametes, type = "l")
plot(stats$time_seconds,stats$autotrophs, type = "l")

par(mfrow=c(1,1))
plot(stats$autotrophs,stats$nutrients_total)
cor(stats$autotrophs,stats$nutrients_total)

linmod <- lm(nutrients_total~autotrophs, data = stats)
abline(linmod)
linmod
cor.test(stats$autotrophs,stats$nutrients_total)
