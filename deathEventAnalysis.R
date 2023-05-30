#deathEventAnalysis
#
library(tidyverse)
library(ggplot2)
library(fitdistrplus)
rm(list=ls())
dev.off()
gc()





{
  
  POI <- data.frame(y = rep(8,8), x = c(8,9,10,11,12,13,14,15))
  
  simname <- "mat02_mig_8e4"
  #
  thisdir = r"(C:\Users\gushanamc\UnityProjects_Local\builde\FINALFORPAPERS\mat02_mig_8e4)"
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






allDeathAges <- list()

for(i in 1:10){
  allDeathAges[[i]] <- deathEventMetaList[[i]]$age 
}


allDeathAges <- unlist(allDeathAges)
theticks = pretty(seq(50:400),8)

pdf("allDeathAges_histogram_mat04mige4.pdf")
hist(allDeathAges, main = "Age at death\nn = 118 844 857", xlab = "steps", xaxt = "n", breaks = seq(0,450,10))

axis(side = 1, at = theticks, labels = FALSE, tick = TRUE, lwd.ticks = 1.5)
axis(side = 1, at = pretty(seq(50:450),32), labels = FALSE, lwd.ticks = 1, )
mtext(side = 1, text = theticks, at = theticks, line = 1,las=0)

dev.off()
gc()




onlypoi.run1 <- deathEventMetaList[[1]]%>%filter(pos_x >7 & pos_x < 12 & pos_y == 8)




{
  # testdf <- data.frame(angle = runif(256*100000)*pi, magnitude = rnorm(256*100000,0,0.15))
  # 
  # testdf2 <- data.frame(x = cos(testdf$angle)*testdf$magnitude,y = sin(testdf$angle)*testdf$magnitude)
  # 
  # testdf3 <- data.frame(x = round(testdf2$x), y = round(testdf2$y))
  # 
  # plot(testdf3)
  # 
  # ggplot(testdf3,aes(x,y))+geom_count()
  # 
  # plot(seq(-1,1,0.1),dnorm(seq(-1,1,0.1),0,0.1)) 
}
  setwd("./PopData/")
deathList <- lapply(list.files(pattern = "deathEvents*"),read.csv,header = TRUE)



deathEvents<-bind_rows(deathList)
rm(deathList)
gc()


#ggplot(deathEvents,aes(time_steps,generation))+geom_count(alpha = 0.01)
setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R)")
setwd("./PopData/")
deathList <- lapply(list.files(pattern = "deathEvents*"),read.csv,header = TRUE)



deathEvents<-bind_rows(deathList)
rm(deathList)
gc()
setwd(r"(C:/Users/gushanamc/RstudioStuff/Cilioids_thesis_R)")

parameters <- read.csv("params.txt")
parameters <- spread(parameters, key = Parameter, value = Value)

deathPositions <- data.frame(x = deathEvents$pos_x, y = deathEvents$pos_y)

deathPositions.last <- data.frame(x = deathEvents$pos_x[which(deathEvents$time_steps > max(deathEvents$time_steps)*0.95)], y = deathEvents$pos_y[which(deathEvents$time_steps > max(deathEvents$time_steps)*0.95)])

summary(deathPositions)

deathposTab <- table(deathPositions[5.99e5:6e5,])

library(epiDisplay)
tab1(deathPositions.last[,1], sort.group ="decreasing", cum.percent = TRUE)



image(deathposTab)
deathpos.df <- as.data.frame(deathposTab)

plot(deathEvents$time_steps, deathEvents$maximumLifeSpan)

mean(deathEvents$maximumLifeSpan)

table(factor(deathEvents$causeOfDeath))
hist(deathEvents$age)

hist(deathEvents$nutrientLevel, breaks = 16)
hist(deathEvents$reproductiveEvents, breaks = 8)
hist(deathEvents$spentNutrients)
hist(deathEvents$pos_x, breaks = c(seq(-1,16,0.5)))
hist(deathEvents$pos_y, breaks = c(seq(-1,16,0.5)))

boxplot(deathEvents$age~deathEvents$causeOfDeath)

boxplot(deathEvents$spentNutrients~deathEvents$causeOfDeath)


deathEvents$ofMaxLifeSpan <- deathEvents$age/deathEvents$maximumLifeSpan

hist(deathEvents$ofMaxLifeSpan[which(deathEvents$ofMaxLifeSpan < 1)], breaks = 64)
plot(deathEvents$ofMaxLifeSpan,deathEvents$spentNutrients, type = "l")

dp.df <- as.data.frame(deathposTab)

dp.df$x = as.numeric(dp.df$x)+5
dp.df$y = as.numeric(dp.df$y)+4
dp.df$Freq = as.numeric(dp.df$Freq)

dp.df <- arrange(dp.df, x)




winder <- owin(c(6,9),c(5,9))


pepe <- ppp(dp.df$x,dp.df$y, winder, marks = dp.df$Freq)

plot(Kest(pepe))

plot(pepe)

testpep <- as.ppp(table(deathPositions))

meanActualLifeSpan = mean(deathEvents$age)
especial <- matrix(nrow = 5, ncol = 5)

deathposMat <- matrix(as.numeric(deathposTab), ncol = 5)
 dimnames(deathposMat) <- list(seq(1,4), seq(0:4))

scaledDeathPosMat <- deathposMat/sum(deathposMat)






lolo <- matrix(rep(0,20), ncol=5)
lele <- as.ppp(scaledDeathPosMat, winder)

squaq <- multiplicity(dp.df)

ppp(x = deathposMat[,1:5],y = deathposMat[1:4,], xrange = c(0,15), yrange = c(0,15))

centerDeaths <- deathEvents%>%filter(pos_x > 6 & pos_x < 9 & pos_y > 6 & pos_y < 9)

centerDeaths <- centerDeaths%>%group_by(generation)
centerMeanAgeAtDeath <-  centerDeaths%>%summarise(AgeAtDeath.mean = mean(age), AgeAtDeath.sd = sd(age))

ggplot(centerDeaths,aes(factor(generation),age))+geom_boxplot()
plot(centerMeanAgeAtDeath$generation,centerMeanAgeAtDeath$AgeAtDeath.sd)

ggplot(centerDeaths,aes(age, fill = factor(generation)))+geom_boxplot()

hist(deathEvents$age)
hist(deathEvents$reproductiveEvents, breaks = 16)

deathEvents$ofMaxLifeSpan <- deathEvents$age/deathEvents$maximumLifeSpan

hist(deathEvents$ofMaxLifeSpan[which(deathEvents$ofMaxLifeSpan < 1)], breaks = 64)

hist(deathEvents$ofMaxLifeSpan)


plotdist(deathEvents$ofMaxLifeSpan[which(deathEvents$ofMaxLifeSpan < 1)])



#deathEvents <- read.csv("./PopData/DeathEvents1000.csv")
deathEvents$migrations.sum <- deathEvents$migrations_left+deathEvents$migrations_right+deathEvents$migrations_up+deathEvents$migrations_down+deathEvents$migrations_upLeft+deathEvents$migrations_upRight+deathEvents$migrations_downLeft+deathEvents$migrations_downRight


migrationMatrix = matrix(c(rep(0,9)), nrow = 3, ncol = 3)


#cos v = a / sqrt(2)
#sqrt(2) * cos 0.25pi = a

migrationMatrix[1,2] = sum(deathEvents$migrations_up)
migrationMatrix[3,2] = sum(deathEvents$migrations_down)
migrationMatrix[2,1] = sum(deathEvents$migrations_left)
migrationMatrix[2,3] = sum(deathEvents$migrations_right)
migrationMatrix[1,1] = sum(deathEvents$migrations_upLeft)
migrationMatrix[3,1] = sum(deathEvents$migrations_downLeft)
migrationMatrix[1,3] = sum(deathEvents$migrations_upRight)
migrationMatrix[3,3] = sum(deathEvents$migrations_downRight)
migrationMatrix[2,2] = length(deathEvents$migrations.sum[which(deathEvents$migrations.sum == 0)])

migrationMatrixFreq = matrix(c(rep(0,9)), nrow = 3, ncol = 3)


for(i in 1:3){
  for(j in 1:3){
    migrationMatrixFreq[i,j] = migrationMatrix[i,j]/sum(migrationMatrix)
  }
}



t.test(deathEvents$migrations_left,deathEvents$migrations_right)

library(plot.matrix)
par(mar = c(6,6,6,6));plot(migrationMatrix)

probMigration.lifetime <- sum(migrationMatrixFreq)-migrationMatrixFreq[2,2]
probMigration.perStep <- mean(deathEvents$migrations.sum/deathEvents$age)

plot(seq(0,2,1),dpois(seq(0,2,1),probMigration.lifetime))
plot(seq(0,256,1),qpois(seq(0,1,1/256),1-probMigration.perStep))
hist(rbinom(10000,4,probMigration.lifetime))
hist(deathEvents$migrations.sum)
plot(seq(0,2,1),dbinom(seq(0,2,1),2,probMigration.lifetime), type = "p")
barplot(dbinom(seq(0,3,1),2,probMigration.lifetime))



deathEvents$dx = 
  (deathEvents$migrations_right+
  deathEvents$migrations_upRight+deathEvents$migrations_downRight)-
  (deathEvents$migrations_left+
  deathEvents$migrations_upLeft+deathEvents$migrations_downLeft)
  
deathEvents$dy = 
  (deathEvents$migrations_up+
     deathEvents$migrations_upRight+deathEvents$migrations_upLeft)-
  (deathEvents$migrations_down+
     deathEvents$migrations_downLeft+deathEvents$migrations_downRight)

summary(deathEvents$dx)
summary(deathEvents$migrations.sum)
mean(deathEvents$migrations.sum/deathEvents$age)

deathEvents$migrationsPerStep <- deathEvents$migrations.sum/deathEvents$age

hist((deathEvents$migrationsPerStep[which(deathEvents$migrationsPerStep > 0)]), breaks = 128)

ggplot(deathEvents,aes(dx/age,dy/age))+geom_bin2d()



deathEvents$magnitude <- sqrt(deathEvents$dx^2+deathEvents$dy^2)
hist(deathEvents$magnitude[which(deathEvents$magnitude > 0)])

hist(deathEvents$magnitude[which(deathEvents$magnitude > 0)]/deathEvents$age[which(deathEvents$magnitude > 0)],breaks = 128)

ggplot(deathEvents, aes(magnitude/age))+geom_histogram()








summary(deathEvents$migrations.sum)
summary(rbinom(nrow(deathEvents),1,0.002397 ))
sum(deathEvents$migrations.sum)

hist(log10(deathEvents$migrationsPerStep),breaks = 32)











repStats <- read.csv("./RepData/repStat0.csv")
theoreticalLife <- data.frame(mean = mean(repStats$maximumLifeSpan), sd = sd(repStats$maximumLifeSpan))

gc()


par(mfrow = c(2,2))
hist(deathEvents$spentNutrients[which(deathEvents$causeOfDeath == "age")],breaks = 256, main = "Lifetime excreted nutrients\nCoD = Age",xlab = "Excreted nutrients")

hist(deathEvents$spentNutrients[which(deathEvents$causeOfDeath == "age")]/deathEvents$age[which(deathEvents$causeOfDeath == "age")],breaks = 256, main = "Lifetime excreted nutrients\nCoD = Age",xlab = "Excreted nutrients")


hist(deathEvents$spentNutrients[which(deathEvents$causeOfDeath == "starvation")],breaks = 256, main = "Lifetime excreted nutrients\nCoD = Starvation",xlab = "Excreted nutrients")


hist(deathEvents$nutrientLevel[which(deathEvents$causeOfDeath == "age")],breaks = 16, main = "Nutrient level at death\nCoD = Age",xlim = c(0,16),xlab = "Current nutrients")
hist(deathEvents$nutrientLevel[which(deathEvents$causeOfDeath == "starvation")],breaks = 16, main = "Nutrient level at death\nCoD = Starvation",xlim = c(0,16),xlab = "Current nutrients")





hist(deathEvents$age,breaks = 256)

par(mfrow = c(1,1))
hist((deathEvents$energyLevel[which(deathEvents$causeOfDeath == "age")]+1),breaks = 256, main = "Energy level at death\nCoD = Age",xlab = "Log10(Energy+1)")

hist(deathEvents$energyLevel[which(deathEvents$causeOfDeath == "starvation")],breaks = 16, main = "Energy level at death\nCoD = Starvation",xlab = "(Energy+1)")



codtab <- table(factor(deathEvents$causeOfDeath))
codtabSc <- (codtab/sum(codtab))*100

par(mfrow=c(1,1))
barplot(codtabSc, main = "Cause of death (%)")

library(MASS)
install.packages("mnormt")
library(mnormt)

migdist <- data.frame (x = rnorm(1000000,0,0.25),y = rnorm(100000,0,0.25))
migdist <- data.frame (x = dnorm(seq(-1.5,1.5,0.01),0,0.25),y = dnorm(seq(-1.5,1.5,0.01),0,0.25))


integrate(migdist)


length(migdist$x[which(migdist$x > 1 | migdist$x < -1)])




ggplot(migdist,aes(x,y))+geom_density2d()


mgmat <- matrix(nrow = 101, ncol = 101)

plot(dnorm(seq(-1.1,1.1,0.1),0,0.25))

normPDF <- function(x, sigma = 1, mu = 0){
  base = (1/(sigma*sqrt(2*pi)))
  expo = (-1/2)*( ((x-mu)/sigma)^2 )
  
  out = base*exp(expo)
  return(out)
}

plot(seq(-1.1,1.1,0.1),normPDF(x = seq(-1.1,1.1,0.1), sigma = 0.25, mu = 0))

normPDF2D <- function(x,y, sigma = 1, mu = 0){
  base.x = (1/(sigma*sqrt(2*pi)))
  expo.x = (-1/2)*( ((x-mu)/sigma)^2 )
  
  out.x = base.x*exp(expo.x)
  
  base.y = (1/(sigma*sqrt(2*pi)))
  expo.y = (-1/2)*( ((y-mu)/sigma)^2 )
  
  out.y = base.y*exp(expo.y)
  out = data.frame(x = out.x, y = out.y, z = sqrt(out.x^2 + out.y ^2))
  
  
  return(out)
}


normPDF2D_density <- function(x,y, sigma = 1, mu = 0){
  base.x = (1/(sigma*sqrt(2*pi)))
  expo.x = (-1/2)*( ((x-mu)/sigma)^2 )
  
  out.x = base.x*exp(expo.x)
  
  base.y = (1/(sigma*sqrt(2*pi)))
  expo.y = (-1/2)*( ((y-mu)/sigma)^2 )
  
  out.y = base.y*exp(expo.y)
  out =  sqrt(out.x^2 + out.y ^2)
  
  
  return(out)
}

circle.x <- function(r,stepSize){
  
  out = cos(seq(0,2*pi,stepSize))*r
  
  return(out)
  
}

circle.y <- function(r,stepSize){
  
  out = sin(seq(0,2*pi,stepSize))*r
  
  return(out)
  
}

plot(circle.x(1.1,0.01),circle.y(1.1,0.01), type = "l")
lines(circle.x(1,0.01),circle.y(1,0.01), type = "l")


owo <- data.frame(x = seq(-1.1,1.1,0.1),y= seq(-1.1,1.1,0.1), z = owo$z)

ggplot(owo, aes(circle.x,y))+geom_line()

nrow(owo)

plot(circle.x(0.5,0.01),circle.y(0.5,0.01), type = "l")

owo <- normPDF2D(x = seq(-1.1,1.1,0.1),y= seq(-1.1,1.1,0.1), sigma = 0.25, mu = 0)

plot(owo$z)

ggplot(owo)+geom_line(aes(x = cos(x*pi), y = sin(y*pi)))


plot(normPDF2D(x = seq(-1.1,1.1,0.1),y= seq(-1.1,1.1,0.1), sigma = 0.25, mu = 0))


owoe <- data.frame(radius.x = seq(-1.1,1.1,0.1), radius.y = seq(-1.1,1.1,0.1), density = owo$z)

plot(owoe$radius.x,owoe$radius.y)

#vector of radii
#written in a way that's easily changable
n_circles <- 8
?seq()
my_circles <- seq(-1.1,1.1,0.1)
#generate x and y
x <- rep(-1.1,n_circles)
y <- rep(1.1, n_circles)

#plot
symbols(rep(0,23),rep(0,23),1/owoe$density)
?symbols

integrate()
integrate(normPDF,-Inf,-1)

ggplot(owo,aes(seq(-1.1,1.1,0.1),z))+geom_line()

ggplot(owo, aes(x=x, y=y) ) +
  geom_density_2d()


gaga <- matrix(nrow = 23,ncol = 23)
gago <- data.frame(x = rep(0,23), y = rep(0,23))
coordseq <- seq(-1.1,1.1,0.1)
coordseq2 <- seq(-1.1,1.1,0.1)


normo.df = data.frame(x = rnorm(1000000,0,0.25),
                      y = rnorm(1000000,0,0.25))
                      
n = 1e5
neues.df <- data.frame(angle = runif(n,0,2*pi), magnitude = rnorm(n,0,0.25))

plot(cos(neues.df$angle)*neues.df$magnitude,sin(neues.df$angle)*neues.df$magnitude)


normo.df <- data.frame(x = round(cos(neues.df$angle)*neues.df$magnitude), y = round(sin(neues.df$angle)*neues.df$magnitude))
plot(normo.df)

summary(normo.df)

ggplot(normo.df,aes(x,y))+geom_count()

matt <- matrix(nrow = 3, ncol = 3)
for (i in 1:3){
  for(j in 1:3){
    matt[i,j] = 0
    }
    }

for(i in 1:nrow(normo.df)){
  
  if(normo.df$x[i] > 1 & normo.df$y[i] > -1 &  normo.df$y[i] < 1){
    matt[3,2] = matt[3,2]+ 1
  }
  
  else if(normo.df$x[i] < -1 & normo.df$y[i] > -1 &  normo.df$y[i] < 1){
    matt[1,2] = matt[1,2]+ 1
  }
  
  else if(normo.df$y[i] > 1 & normo.df$x[i] > -1 &  normo.df$x[i] < 1){
    matt[2,3] = matt[2,3]+ 1
  }
  
  else if(normo.df$y[i] < -1 & normo.df$x[i] > -1 &  normo.df$x[i] < 1){
    matt[2,1] = matt[2,1]+ 1
  }
  
  
  else if(normo.df$x[i] > 1 & normo.df$y[i] > 1){
    matt[3,3] = matt[3,3]+ 1
  }
  else if(normo.df$x[i] < -1 & normo.df$y[i] < -1){
    matt[1,1] = matt[1,1]+ 1
  }
  
  else if(normo.df$x[i] < -1 & normo.df$y[i] > 1){
    matt[1,3] = matt[1,3]+ 1
  }
  
  else if(normo.df$y[i] < -1 & normo.df$x[i] > 1){
    matt[3,1] = matt[3,1]+ 1
  }
  
  else if(normo.df$y[i] > -1 & normo.df$y[i] < 1 & normo.df$x[i] > -1 & normo.df$x[i] < 1){
    matt[2,2] = matt[2,2]+ 1
  }
  
  
  }
  image(matt)

plot(floor(filnordf$x),floor(filnordf$y))



 matt[2,2] = length(x[which(x < 1 & x > -2 & x < 1 & x > -2)])
  


for(i in 1:23){
  
  for(j in 1:23){
    gaga[i,j] = normPDF2D(i,j,0.25,0)
    
    }
  
}


  plot(normPDF2D(seq(-1.1,1.1,0.1)))
plot(gago)




v <- ggplot(owoe, aes(radius.x, radius.y, z = density))
v + geom_contour()


?dmnorm
library(mnormt)

x     <- seq(-1.1, 1.1, 0.1) 
y     <- seq(-1.1, 1.1, 0.1)
mu    <- c(0, 0)
sigma <- matrix(c(0.125, 0, 0, 0.125), nrow = 2)
f     <- function(x, y) dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x, y, f)





summary(z)

contour(x, y, z);abline(h = c(-1,1), v = c(-1,1))

table.paint(as.data.frame(z), clabel.c = 0, clabel.r = 0, cleg = 0)
image(z, add = T)




plot(xlim = c(-1.1,1), ylim = c(-1.1,1))

gc()


persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "gold", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")

?persp
?contour()

?outer

plot(qnorm(seq(0,1,0.01),0,0.25))

#create bivariate normal distribution
x     <- seq(-3, 3, 0.1) 
y     <- seq(-3, 3, 0.1)
mu    <- c(0, 0)
sigma <- matrix(c(2, -2, -2, 2), nrow=2)
f     <- function(x, y) dmnorm(cbind(x, y), mu, sigma)
z     <- outer(x, y, f)

#create contour plot
contour(x, y, z)


contour()



ggplot(migdist,aes(x,y))+geom_density_2d_filled()

summary(deathEvents$spentNutrients[which(deathEvents$causeOfDeath == "starvation")])

hist(sqrt(
  sample(deathEvents$nutrientLevel[which(deathEvents$causeOfDeath == "age")],5000) +1),breaks = 20 )

hist(deathEvents$age[which(deathEvents$causeOfDeath == "starvation")],breaks = 32)
summary(deathEvents$age[which(deathEvents$causeOfDeath == "starvation")])
summary(deathEvents$nutrientLevel)
hist(deathEvents$energyLevel)#,breaks = 256)
hist(deathEvents$energyLevel[which(deathEvents$energyLevel > 1)],breaks = 64)

plot(deathEvents$age,deathEvents$energyLevel)

summary(factor(deathEvents$causeOfDeath))

summary(deathEvents$energyLevel)
hist(deathEvents$age,breaks = 256)


par(mfrow=c(1,2))
hist(deathEvents$reachedMaturityAge[which(deathEvents$reachedMaturityAge > -1)],main  ="Age at maturity", xlab = "Age (steps)", xlim =c(0,400))

hist(deathEvents$age[which(deathEvents$reachedMaturityAge == -1)],main  ="Age at death\n maturity not reached", xlab = "Age (steps)", xlim =c(0,400))

par(mfrow=c(1,3))
hist(deathEvents$reproductiveEvents, main = "Lifetime reproductive events\n(all)", xlab = "Gametes", xlim = c(0,16), breaks = 16)
hist(deathEvents$reproductiveEvents[which(deathEvents$maturity == 1)], main = "Lifetime reproductive events\n(mature at death)", xlab = "Gametes", xlim = c(0,16), breaks = 32)



hist(deathEvents$reproductiveEvents[which(deathEvents$reproductiveEvents > 0)], main = "Lifetime reproductive events\n(>0)", xlab = "Gametes", xlim = c(0,16), breaks = 16)

migrationStats <- data.frame(
  left.mean  = mean(deathEvents$migrations_left),
  left.sd    = sd(deathEvents$migrations_left),
  right.mean = mean(deathEvents$migrations_right),
  right.sd   = sd(deathEvents$migrations_right),
  x.mean     = mean(deathEvents$migrations_right-deathEvents$migrations_left),
  x.sd       = sd(deathEvents$migrations_right-deathEvents$migrations_left),
  
  up.mean  = mean(deathEvents$migrations_up),
  up.sd    = sd(deathEvents$migrations_up),
  down.mean = mean(deathEvents$migrations_down),
  down.sd   = sd(deathEvents$migrations_down),
  y.mean     = mean(deathEvents$migrations_up-deathEvents$migrations_down),
  y.sd       = sd(deathEvents$migrations_up-deathEvents$migrations_down),
  total.mean = mean( sqrt(
    (deathEvents$migrations_right-deathEvents$migrations_left)^2 +
    (deathEvents$migrations_up-deathEvents$migrations_down)^2) ),
  total.sd = sd( sqrt(
    (deathEvents$migrations_right-deathEvents$migrations_left)^2 +
      (deathEvents$migrations_up-deathEvents$migrations_down)^2) ) 
  )



displacement.df <- data.frame(
  ind = seq(1:nrow(deathEvents)),
   left = deathEvents$migrations_left,
   right = deathEvents$migrations_right,
   up = deathEvents$migrations_up,
  down = deathEvents$migrations_down,
  total = deathEvents$migrations.sum,
  x = (deathEvents$migrations_right-deathEvents$migrations_left),
  y = (deathEvents$migrations_up-deathEvents$migrations_down),
  
   
                              )
                              




  

GetMSD <- function(x1, x2){
  N = length(x1)
  msd = (1/N)*(sum(x1^2)+sum(x2^2))
  return(msd)
  }


sum(rbinom(256,1,1/3))


mean(deathEvents$migrations.sum)

msd <- (GetMSD((deathEvents$migrations_right - deathEvents$migrations_left ),(deathEvents$migrations_up - deathEvents$migrations_down )))

sqmagnitude = mean(
  (
    ((deathEvents$migrations_right-deathEvents$migrations_left)^2) + 
      ((deathEvents$migrations_up-deathEvents$migrations_down)^2)
  )
)


sqrt(msd)
sqrt(sqmagnitude)


mean(
  sqrt(
    ((deathEvents$migrations_right-deathEvents$migrations_left)^2) + 
      ((deathEvents$migrations_up-deathEvents$migrations_down)^2)
  )
)

(mean( 
(deathEvents$migrations_right - deathEvents$migrations_left)^2 +(deathEvents$migrations_up - deathEvents$migrations_down)^2))

mean(displacement.df$magnitude)

MSD.perGeneration <- GetMSD((deathEvents$migrations_right - deathEvents$migrations_left ),(deathEvents$migrations_up - deathEvents$migrations_down ))

mean(rpois(nrow(deathEvents),((1/3)*(1/4))^2))

meanDispersalPerGeneration <- sqrt(MSD.perGeneration)

binomo <-rbinom(nrow(deathEvents),1,mean(deathEvents$migrations_up))
summary(binomo)
summary(deathEvents$migrations_up)
summary(deathEvents$migrations_down)
summary(deathEvents$migrations_left)
summary(deathEvents$migrations_right)
summary(deathEvents$migrations.sum)

mean(displacement.df$magnitude)
mean(deathEvents$migrations.sum)

plot(seq(1:1000)*meanDispersalPerGeneration)

a = 1/3
b = 0.25

(a*b)^2


displacement.df$magnitude <- sqrt((displacement.df$right-displacement.df$left)^2 + (displacement.df$up-displacement.df$down)^2)

mean(displacement.df$magnitude)
sd(displacement.df$magnitude)

plot(pnorm(quantile(rnorm(10000000,mean(0),0.25))))



mean(testVals$val)
sd(testVals$val)
summary(testVals)
 
mean(displacement.df$left)
sd(displacement.df$right)
mean(displacement.df$total)
sd(displacement.df$total)



mean(displacement.df$magnitude)



0.25*(1/3)

sd(displacement.df$magnitude)*sqrt(2/pi)


MSD <- (1/nrow(displacement.df))*(sum(displacement.df$corn^2))
dispSig
sqrt(MSD)

hist(rnorm(10000,mean = mean(displacement.df$magnitude),sd = sd(displacement.df$magnitude)),breaks = 256)
hist(rnorm(10000,0,0.25)*1/3, breaks = 256)

theoDist <- data.frame(fromdata = rnorm(100000,mean = mean(displacement.df$magnitude),sd = sd(displacement.df$magnitude)), theor = rnorm(100000,0,0.25)*1/3)

ggplot(gather(theoDist),aes(value, fill = key))+geom_boxplot(alpha = 0.5)

summary(displacement.df)





GetMSD(deathEvents$migrations_right,deathEvents$migrations_left)

gc()

sigma <- 0.25
sigma*sqrt(2/pi)



# sigma*sqrt(2/pi)
# Sigma is the standard deviation of gaussian dispersal kernel
# 
# mean absolute displacement


sd(deathEvents$reachedMaturityAge[which(deathEvents$reachedMaturityAge > -1 & deathEvents$reachedMaturityAge < 200) ])
hist(deathEvents$reachedMaturityAge[which(deathEvents$reachedMaturityAge > -1)])

hist(deathEvents$maturity,breaks = 10)
hist(deathEvents$maturity[which(deathEvents$maturity < 1)],breaks = 10)

plot(deathEvents$age, deathEvents$nutrientLevel)
plot(deathEvents$age, deathEvents$energyLevel)
ggplot(deathEvents,aes(age,nutrientLevel))+geom_point(alpha = 0.1)
ggplot(deathEvents,aes(age,energyLevel))+geom_point(alpha = 0.1)
ggplot(deathEvents,aes(age))+geom_density()

plot(spentNutrients~age, deathEvents, type= "l")

hist(deathEvents$reproductiveEvents)

hist(log(deathEvents$reproductiveEvents+1),breaks = 9)


sampleStats <- slice_sample(deathEvents,n = 5000)


summary(deathEvents$migrations.sum)

shapiro.test(log(sampleStats$reproductiveEvents+1))


hist(deathEvents$reproductiveEvents[which(deathEvents$reproductiveEvents > 0)])

plotdist(deathEvents$reproductiveEvents)

plot(fitdist(deathEvents$reproductiveEvents, "exp"))
plot(fitdist(deathEvents$reproductiveEvents[which(deathEvents$reproductiveEvents > 0)], "pois"))

summary(factor(deathEvents$causeOfDeath))
dev.off()


ageEmpvThe <- data.frame(empirical = deathEvents$age, theoretical = floor(rnorm(nrow(deathEvents),theoreticalLife$mean,theoreticalLife$sd)))

ageEmpvThe <- ageEmpvThe%>%gather()

ggplot(ageEmpvThe,aes(value, group = key, fill = key))+geom_histogram(alpha = 1, binwidth = 3)

hist()




downSampleFunc <- function(x,n){
  return( (floor(x*(n-1)+0.5))/(n-1))
  }

testmat <- matrix(nrow = 256, ncol = 256)

for(x in 1:256){
  for(y in 1:256){
    testmat[x,y] <- runif(1,0,1)
  } 
}

image(testmat, col = gray.colors(4096))

tempmat <- matrix(nrow = 3, ncol = 3)
testmat2 <- matrix(nrow = 256,ncol = 256)
for(x in 2:255){
  for(y in 2:255){
    
    
    tempsum <-(testmat[x-1,y+1]+testmat[x,y+1]+testmat[x+1,y+1]+
    testmat[x-1,y]+testmat[x,y]+testmat[x+1,y]+
    testmat[x-1,y]+testmat[x,y-1]+testmat[x+1,y-1])
    
    
    testmat[x,y] <- tempsum/8
  } 
}

    
image(testmat, col = gray.colors(4096))

downSampleFunc(0.8,4)

for(x in 1:256){
  for(y in 1:256){
    testmat[x,y] <- downSampleFunc(testmat[x,y],4)
  } 
}
