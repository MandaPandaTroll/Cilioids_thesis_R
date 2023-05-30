#repstats
library(tidyverse)
library(gganimate)
rm(list = ls())
gc()
dev.off()





setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R)")
repStats <- read.csv("./RepData/repStat0.csv")

parameters <- read.csv("params.txt")
parameters <- spread(parameters, key = Parameter, value = Value)

inds <- repStats%>%distinct(individualNumber, .keep_all = T)

length(inds[which(duplicated(inds$parentGameteA)) == TRUE])
length(inds[which(duplicated(inds$parentGameteB)) == TRUE])

lastStat <- repStats%>%filter(time_seconds >= max(time_seconds))
repStats <- repStats%>%group_by(individualNumber)

oldies <- repStats%>%filter(age_steps == max(age_steps))




hist((oldies$age_steps))
hist(lastStat$age_steps)
hist(lastStat$maximumLifeSpan)
hist(oldies$maximumLifeSpan)
hist(lastStat$gametesProduced[which(lastStat$gametesProduced > 0)], breaks = 7)
hist((oldies$gametesProduced[which(oldies$gametesProduced > 0)]), breaks = 8)
#, after_stat(density)

ggplot(inds,aes(maximumLifeSpan,after_stat(density)))+geom_histogram()+transition_time(time_steps)+ease_aes('linear')




hist(lastStat$age_steps)





library(fitdistrplus)

gamsNotZero <- oldies$gametesProduced[which(oldies$gametesProduced > 0)]

plotdist(gamsNotZero)
plotdist(log(gamsNotZero))


expfit <- fitdist(gamsNotZero,"exp")
gfit <- fitdist(gamsNotZero,"gamma")
poisfit <- fitdist(gamsNotZero,"pois")

gamsNotZero01 <- (gamsNotZero-min(gamsNotZero))/(max(gamsNotZero-min(gamsNotZero)))

#norm, lnorm, exp, pois, cauchy, gamma, logis, nbinom, geom, beta, weibull from the stats package; invgamma, llogis, invweibull, pareto1, pareto
plot(fitdist(gamsNotZero,"norm"))
plot(fitdist(gamsNotZero,"lnorm"))
plot(fitdist(gamsNotZero,"exp"))
plot(fitdist(gamsNotZero,"pois"))
plot(fitdist(gamsNotZero,"cauchy"))
plot(fitdist(gamsNotZero,"gamma"))
plot(fitdist(gamsNotZero,"logis"))
plot(fitdist(gamsNotZero,"nbinom"))
plot(fitdist(gamsNotZero,"geom"))
plot(fitdist(gamsNotZero,"weibull"))
plot(fitdist(gamsNotZero01,"beta"))

geomfit <- fitdist(gamsNotZero,"geom")
plot(gfit)
plot(expfit)
plot(poisfit)
plot(geomfit)
nbinomfit <- fitdist(gamsNotZero,"nbinom")

gofstat(geomfit)
gofstat(expfit)
gofstat(nbinomfit)
gofstat(gfit)

plot(bootdist(nbinomfit))
plotdist(gamsNotZero)
descdist(gamsNotZero, discrete =T, boot = 256)

llplot(nbinomfit)




c(0,1,2,3)
c(0,1,2,3,4)

