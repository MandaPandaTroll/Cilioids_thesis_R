#Statsanalysis
library(tidyverse)
library(car)

rm(list=ls())
gc()
dev.off()
stats <- read.csv("./PopData/Stats0.csv")
parameters <- read.csv("params.txt")
stats <- stats[2:nrow(stats),]

hist(stats$autotrophs[2:nrow(stats)])
hist(stats$gametes[2:nrow(stats)])
plot(stats$time_steps,stats$autotrophs, type = "l")
plot(stats$time_steps,stats$gametes, type = "l")
plot(stats$autotrophs,stats$gametes)

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
