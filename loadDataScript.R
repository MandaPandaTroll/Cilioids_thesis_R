#loadDataScript

library(tidyverse)
rm(list = ls())
gc()

thisdir = r"(C:\Users\gushanamc\RStudioStuff\Cilioids_thesis_R\AllSieved)"
setwd(thisdir)
#hiv <- read.csv(r"(C:\Users\gushanamc\Downloads\hiv-death-rates.csv)")


#max(hiv%>%filter( Entity == "South Africa",  Year <= 2005)%>%select(-Entity,-Year, -Code))


#max(hiv%>%filter( Entity == "South Africa",  Year <= 2005)%>%select(-Entity,-Year, -Code))



#ggplot(hiv%>%filter( Entity == "South Africa",  Year <= 2010), aes(Year,Deaths...HIV.AIDS...Sex..Both...Age..Age.standardized..Rate. ))+geom_line()+geom_point()+geom_hline(aes( yintercept = 663.44), linetype = 2, colour = "red")+labs(y = "Annual deaths per 100 000", title = "Annual deaths from HIV/AIDS in South Africa\nper 100 000 individuals ")+scale_x_continuous(breaks = c(seq(1990,2010,5)))+theme_bw()

experimentNames <- list.files("./","mat*")[1:9]
matCoefs <- c(rep(0.2,3),rep(0.4,3),rep(0.8,3))
pmigs <- c(rep(c(2e-4,4e-4,8e-4),3))

getGlobal <- function(varname){
  experimentNames <- list.files("./","mat*")[1:9]
  matCoefs <- c(rep(0.2,3),rep(0.4,3),rep(0.8,3))
  pmigs <- c(rep(c(2e-4,4e-4,8e-4),3))
  templist <- list()
  
  
  tempPaths <-  Sys.glob(paste0("./*/global",varname,"*.csv"))
  
  for (i in 1:9){
    templist[[i]] <- read.csv(tempPaths[i])
    templist[[i]]$experiment.ID <- experimentNames[i]
    templist[[i]]$maturity.coefficient = matCoefs[i]
    templist[[i]]$p.migration = pmigs[i]
    templist[[i]] <- templist[[i]]%>%select(-X)
  }  
  
  
  return(templist)
}

makeSuper <- function(inlist){
  df <- bind_rows(inlist)
  
  return(df)
  
}




getglobalMeanDF <- function(df){
  
  out.df <- df[,c(1,12:17)]
}

  


get1dims <- function(varname){
  matCoefs <- c(rep(0.2,3),rep(0.4,3),rep(0.8,3))
  pmigs <- c(rep(c(2e-4,4e-4,8e-4),3))
  outlist <- list()
  path <-  Sys.glob(paste0("./*/*",varname,"*.csv"))
  for(i in 1:9){
    outlist[[i]] <- read.csv(path[i])
    outlist[[i]] <- outlist[[i]]%>%filter(time_steps < 30756)
    outlist[[i]] <- outlist[[i]]%>%select(-X,-sample)
    outlist[[i]]$maturity.coefficient <- matCoefs[i]
    outlist[[i]]$p.migration <- pmigs[i]
  }
  return(outlist)
}

  
  
  


getEvent1dim <- function(varname){
  matCoefs <- c(rep(0.2,3),rep(0.4,3),rep(0.8,3))
  pmigs <- c(rep(c(2e-4,4e-4,8e-4),3))
  metapaths <- Sys.glob(paste0("./*/",varname,"Events_1dim*"))
  inner.list <- list()
  outer.list <- list()
  
  
  for(i in 1:9){
    paths <- Sys.glob(paste0(metapaths[i],"*/*.csv"))
    for(j in 1: 10){
      inner.list[[j]] = read.csv(paths[j])
      inner.list[[j]] <- inner.list[[j]]%>%filter(time_steps < 30900)
      inner.list[[j]]$maturity.coefficient = matCoefs[i]
      inner.list[[j]]$p.migration = pmigs[i]
    }
    outer.list[[i]] = inner.list
  }
  return(outer.list)
}


globalPopList <- getGlobal("Pop")
superPop.df <- makeSuper(globalPopList)
meanPops.df <- getglobalMeanDF(superPop.df)
superPop.gat <- superPop.df%>%select(-mean.pop,-SD,-SE)%>%gather(key = "run", value = "N", - maturity.coefficient, -p.migration, -time_steps,-experiment.ID)
superPop.gat <- superPop.gat%>%group_by(experiment.ID,time_steps)%>%mutate( percentile.05 = quantile(N,0.05), percentile.95 = quantile(N,0.95))                                  
meanPops.df <- left_join(meanPops.df,superPop.gat)




globalSelfList <-  getGlobal("Self")
superSelf.df <- makeSuper(globalSelfList)
meanSelf.df <- getglobalMeanDF(superSelf.df)
superSelf.gat <- superSelf.df%>%select(-mean.self,-SD,-SE)%>%gather(key = "run", value = "N", - maturity.coefficient, -p.migration, -time_steps, -experiment.ID)
superSelf.gat <- superSelf.gat%>%group_by(experiment.ID,time_steps)%>%mutate( percentile.05 = quantile(N,0.05), percentile.95 = quantile(N,0.95))                                 
meanSelf.df <- left_join(meanSelf.df,superSelf.gat)  


globalfreeNuteList <- getGlobal("FreeNute")
superfreeNute.df <- makeSuper(globalfreeNuteList)
meanfreeNute.df <- getglobalMeanDF(superfreeNute.df)
superfreeNute.gat <- superfreeNute.df%>%select(-mean.freenute,-SD,-SE)%>%gather(key = "run", value = "N", - maturity.coefficient, -p.migration, -time_steps, -experiment.ID)
superfreeNute.gat <- superfreeNute.gat%>%group_by(experiment.ID,time_steps)%>%mutate( percentile.05 = quantile(N,0.05), percentile.95 = quantile(N,0.95))                    
meanfreeNute.df <- left_join(meanfreeNute.df,superfreeNute.gat%>%select(time_steps,percentile.05,percentile.95, experiment.ID)) 



globalLockedNuteList <- getGlobal("LockedNute")
superlockedNute.df <- makeSuper(globalLockedNuteList)
meanlockedNute.df <- getglobalMeanDF(superlockedNute.df)
superlockedNute.gat <- superlockedNute.df%>%select(-mean.lockednute,-SD,-SE)%>%gather(key = "run", value = "N", - maturity.coefficient, -p.migration, -time_steps, -experiment.ID)
#superlockedNute.gat <- superlockedNute.gat%>%group_by(experiment.ID,time_steps)%>%mutate( percentile.05 = quantile(N,0.05), percentile.95 = quantile(N,0.95))                    
#meanlockedNute.df <- left_join(meanlockedNute.df,superlockedNute.gat%>%select(time_steps,percentile.05,percentile.95, experiment.ID))
 


plot(N~p.migration, superPop.gat)
cor.test(superPop.gat$N[which(superPop.gat$time_steps > 30000)],superPop.gat$maturity.coefficient[which(superPop.gat$time_steps > 30000)])
plot(superPop.gat$maturity.coefficient[which(superPop.gat$time_steps > 30000)],superPop.gat$N[which(superPop.gat$time_steps > 30000)], col = factor(superPop.gat$p.migration[which(superPop.gat$time_steps > 30000)]))

plot(superPop.gat$p.migration[which(superPop.gat$time_steps > 30000)],superPop.gat$N[which(superPop.gat$time_steps > 30000)], col = factor(superPop.gat$maturity.coefficient[which(superPop.gat$time_steps > 30000)]))

abline(lm(superPop.gat$N[which(superPop.gat$time_steps > 30000)]~superPop.gat$p.migration[which(superPop.gat$time_steps > 30000)]), col = "red")

abline(lm(superPop.gat$N[which(superPop.gat$time_steps > 30000)]~superPop.gat$maturity.coefficient[which(superPop.gat$time_steps > 30000)]), col = "red")


hist(superPop.gat$N[which(superPop.gat$time_steps > 25000)], breaks = 256)

totalnutelist_1dim <- get1dims("totalnute")
lockednutelist_1dim <- get1dims("lockednute_allruns")
freenutelist_1dim <- get1dims("freenute_allruns")
poplist_1dim <- get1dims("pop_allruns")
selflist_1dim <- get1dims("self_allruns")
selfratiolist_1dim <- get1dims("selfratio_allruns")

deathEventList_1dim <- getEvent1dim("death")
repEventList_1dim <- getEvent1dim("rep")






getMeans_grid_1dim <- function(inlist){
  templist <- list()
  
  for(i in 1:9){
    templist[[i]] <- data.frame(time_steps = rep(inlist[[i]]$time_steps[1:961],1))
    templist[[i]]$maturity.coefficient <- matCoefs[i]
    templist[[i]]$p.migration <- pmigs[i]
    
    templist[[i]]$C1 <- aggregate(inlist[[i]]$C1,by = list(inlist[[i]]$time_steps), mean )$x
    templist[[i]]$C1.sd <- aggregate(inlist[[i]]$C1,by = list(inlist[[i]]$time_steps), sd )$x
    templist[[i]]$C1.se <- templist[[i]]$C1.sd/sqrt(10)
    
    templist[[i]]$C2 <- aggregate(inlist[[i]]$C2,by = list(inlist[[i]]$time_steps), mean )$x
    templist[[i]]$C2.sd <- aggregate(inlist[[i]]$C2,by = list(inlist[[i]]$time_steps), sd )$x
    templist[[i]]$C2.se <- templist[[i]]$C2.sd/sqrt(10)
    
    templist[[i]]$C3 <- aggregate(inlist[[i]]$C3,by = list(inlist[[i]]$time_steps), mean )$x
    templist[[i]]$C3.sd <- aggregate(inlist[[i]]$C3,by = list(inlist[[i]]$time_steps), sd )$x
    templist[[i]]$C3.se <- templist[[i]]$C3.sd/sqrt(10)
    
    templist[[i]]$C4 <- aggregate(inlist[[i]]$C4,by = list(inlist[[i]]$time_steps), mean )$x
    templist[[i]]$C4.sd <- aggregate(inlist[[i]]$C4,by = list(inlist[[i]]$time_steps), sd )$x
    templist[[i]]$C4.se <- templist[[i]]$C4.sd/sqrt(10)
    
    templist[[i]]$C5 <- aggregate(inlist[[i]]$C5,by = list(inlist[[i]]$time_steps), mean )$x
    templist[[i]]$C5.sd <- aggregate(inlist[[i]]$C5,by = list(inlist[[i]]$time_steps), sd )$x
    templist[[i]]$C5.se <- templist[[i]]$C5.sd/sqrt(10)
    
    templist[[i]]$C6 <- aggregate(inlist[[i]]$C6,by = list(inlist[[i]]$time_steps), mean )$x
    templist[[i]]$C6.sd <- aggregate(inlist[[i]]$C6,by = list(inlist[[i]]$time_steps), sd )$x
    templist[[i]]$C6.se <- templist[[i]]$C6.sd/sqrt(10)
    
    templist[[i]]$C7 <- aggregate(inlist[[i]]$C7,by = list(inlist[[i]]$time_steps), mean )$x
    templist[[i]]$C7.sd <- aggregate(inlist[[i]]$C7,by = list(inlist[[i]]$time_steps), sd )$x
    templist[[i]]$C7.se <- templist[[i]]$C7.sd/sqrt(10)
    
    templist[[i]]$C8 <- aggregate(inlist[[i]]$C8,by = list(inlist[[i]]$time_steps), mean )$x
    templist[[i]]$C8.sd <- aggregate(inlist[[i]]$C8,by = list(inlist[[i]]$time_steps), sd )$x
    templist[[i]]$C8.se <- templist[[i]]$C8.sd/sqrt(10)
  }
  
  
  
  mean.df_1dim <- bind_rows(templist)
  
  return(mean.df_1dim)
}


getGat_1dim <- function(df, yvar){
  
  df.gat <- df%>%select(time_steps,C1,C2,C3,C4,C5,C6,C7,C8,p.migration,maturity.coefficient)%>%gather(key = "grid.cell", value = yvar, -time_steps, -maturity.coefficient,-p.migration)
  
  df.gat.sd <- df%>%select(C1.sd,C2.sd,C3.sd,C4.sd,C5.sd,C6.sd,C7.sd,C8.sd)%>%gather(key = "grid.cell", value = "sd")
  
  df.gat.se <- df%>%select(C1.se,C2.se,C3.se,C4.se,C5.se,C6.se,C7.se,C8.se)%>%gather(key = "grid.cell", value = "se")
  
  
  df.gat$sd <- df.gat.sd[,2]
  df.gat$se <- df.gat.se[,2]
  names(df.gat)[5] = yvar
  names(df.gat)[6] = paste0(yvar,".sd")
  names(df.gat)[7] = paste0(yvar,".se")
  df.gat$p.migration <- as.factor(df.gat$p.migration)
  
  df.gat$maturity.coefficient <- factor(df.gat$maturity.coefficient, levels = c(0.2,0.4,0.8))
  
  return(df.gat)
}



meanPop_1dim <- getMeans_grid_1dim(poplist_1dim)

meanPop_1dim.gat <- getGat_1dim(meanPop_1dim, "N")


meanlockednute_1dim <- getMeans_grid_1dim(lockednutelist_1dim)

meanlockednute_1dim.gat <- getGat_1dim(meanlockednute_1dim, "locked.nutrients")

meanfreenute_1dim <- getMeans_grid_1dim(freenutelist_1dim)

meanfreenute_1dim.gat <- getGat_1dim(meanfreenute_1dim, "free.nutrients")



meanfreenute_1dim <- getMeans_grid_1dim(freenutelist_1dim)
meanfreenute_1dim.gat <- getGat_1dim(meanfreenute_1dim, "free.nutrients")

meantotalnute_1dim <- getMeans_grid_1dim(totalnutelist_1dim)
meantotalnute_1dim.gat <- getGat_1dim(meantotalnute_1dim, "total.nutrients")



meanselfratio_1dim <- getMeans_grid_1dim(selfratiolist_1dim)
meanselfratio_1dim.gat <- getGat_1dim(meanselfratio_1dim, "self.ratio")




meanlockedtofreenute_1dim <- meanlockednute_1dim%>%select(-time_steps,-maturity.coefficient,-p.migration)
meanlockedtofreenute_1dim <- meanlockedtofreenute_1dim/meanfreenute_1dim%>%select(-time_steps,-maturity.coefficient,-p.migration)
meanlockedtofreenute_1dim <- replace(meanlockedtofreenute_1dim,is.na(meanlockedtofreenute_1dim), 0)


meanlockedtofreenute_1dim <- meanlockedtofreenute_1dim%>%mutate(time_steps = meanlockednute_1dim$time_steps,maturity.coefficient = meanlockednute_1dim$maturity.coefficient,p.migration = meanlockednute_1dim$p.migration) 

#meanlockedtofree_1dim <- meanlockedtofree_1dim%>%select(C1,C2,C3,C4,C5,C6,C7,C8)
 
meanlockedtofreenute_1dim.gat <- getGat_1dim(meanlockedtofreenute_1dim, "lockedtofree.nutrients")


snipEvent <- function(eventlist){
  outlist = list()
  inner.list <- list()
  for(i in 1:9){
    for(j in 1:10){
      inner.list[[j]] <- eventlist[[i]][[j]]%>%filter(time_steps < 30721)
      if("X" %in% names(inner.list[[j]])){
        inner.list[[j]] <- inner.list[[j]]%>%select(-X)
      }
    }
    outlist[[i]] <- bind_rows(inner.list)
  }
  return(outlist)
}

deathEventList_1dim <- snipEvent(deathEventList_1dim)

for(i in 1:9){
  deathEventList_1dim[[i]]$ofMaxLifeSpan <- deathEventList_1dim[[i]]$age/deathEventList_1dim[[i]]$maximumLifeSpan
  
}


longDeath <- bind_rows(deathEventList_1dim)


longDeath$migrations.sum <- longDeath$migrations_left+longDeath$migrations_right+longDeath$migrations_up+longDeath$migrations_down+longDeath$migrations_upLeft+longDeath$migrations_upRight+longDeath$migrations_downLeft+longDeath$migrations_downRight




meanmigrationSum <- aggregate.data.frame(longDeath$migrations.sum, list(maturity.coefficient = longDeath$maturity.coefficient, p.migration = longDeath$p.migration), mean)

meanmigrationSum.05 <- aggregate.data.frame(longDeath$migrations.sum, list(maturity.coefficient = longDeath$maturity.coefficient, p.migration = longDeath$p.migration), quantile, 0.05)

meanmigrationSum.95 <- aggregate.data.frame(longDeath$migrations.sum, list(maturity.coefficient = longDeath$maturity.coefficient, p.migration = longDeath$p.migration), quantile, 0.95)


meanmigrationSum.df <- data.frame(migrations.mean = meanmigrationSum$x, migrations.05 = meanmigrationSum.05$x, migrations.95 = meanmigrationSum.95,maturity.coefficient = meanmigrationSum$maturity.coefficient, p.migration = meanmigrationSum$p.migration)










mean.reps <- aggregate.data.frame(longDeath$reproductiveEvents, list(maturity.coefficient = longDeath$maturity.coefficient, p.migration = longDeath$p.migration), mean)

reps.05 <- aggregate.data.frame(longDeath$reproductiveEvents, list(maturity.coefficient = longDeath$maturity.coefficient, p.migration = longDeath$p.migration), quantile, 0.05)

reps.95 <- aggregate.data.frame(longDeath$reproductiveEvents, list(maturity.coefficient = longDeath$maturity.coefficient, p.migration = longDeath$p.migration), quantile, 0.95)


meanrep.df <- data.frame(reps.mean = mean.reps$x, reps.05 = reps.05$x, reps.95 = reps.95$x,maturity.coefficient = mean.reps$maturity.coefficient, p.migration = mean.reps$p.migration)








mean.spentNutrients <- aggregate.data.frame(longDeath$spentNutrients, list(maturity.coefficient = longDeath$maturity.coefficient, p.migration = longDeath$p.migration), mean)

spentNutrients.05 <- aggregate.data.frame(longDeath$spentNutrients, list(maturity.coefficient = longDeath$maturity.coefficient, p.migration = longDeath$p.migration), quantile, 0.05)

spentNutrients.95 <- aggregate.data.frame(longDeath$spentNutrients, list(maturity.coefficient = longDeath$maturity.coefficient, p.migration = longDeath$p.migration), quantile, 0.95)


spentNutrients.df <- data.frame(spentNutrients.mean = mean.spentNutrients$x, spentNutrients.05 = spentNutrients.05$x, spentNutrients.95 = spentNutrients.95$x,maturity.coefficient = mean.reps$maturity.coefficient, p.migration = mean.reps$p.migration)



pdf("gamsMigs.pdf")

ggplot(meanmigrationSum.df,aes(p.migration,migrations.mean, colour = factor(maturity.coefficient),ymin = migrations.05,ymax = migrations.95.x,fill = factor(maturity.coefficient), ))+geom_point(aes(shape = factor(maturity.coefficient)))+geom_line(linetype = 2, alpha = 0.3)+geom_errorbar()+theme_bw()+labs(y = "migration events / (lifetime x ind)", x = "P(migration)", title = "Mean lifetime migration events")+scale_fill_discrete(name = "maturity coefficient")+scale_color_discrete(name = "maturity coefficient")+scale_shape_discrete(name = "maturity coefficient")


pdf("meanNutes.pdf")
ggplot(spentNutrients.df,aes(p.migration,spentNutrients.mean, colour = factor(maturity.coefficient),fill = factor(maturity.coefficient), ymin = spentNutrients.05, ymax =spentNutrients.95))+geom_errorbar(alpha = 0.3)+geom_point(aes(shape = factor(maturity.coefficient)))+geom_line(linetype = 2, alpha = 0.3)+theme_bw()+labs(y = "excreted nutrients", x = "P(migration)", title = "Mean amount of excreted nutrients")+scale_fill_discrete(name = "maturity coefficient")+scale_color_discrete(name = "maturity coefficient")+scale_shape_discrete(name = "maturity coefficient")

dev.off()

ggplot(meanrep.df,aes(p.migration,reps.mean, colour = factor(maturity.coefficient),fill = factor(maturity.coefficient), ymin = reps.05, ymax = reps.95))+geom_errorbar()+geom_point(aes(shape = factor(maturity.coefficient)))+geom_line(linetype = 2, alpha = 0.3)+theme_bw()+labs(y = "gametes / (lifetime x ind)", x = "P(migration)", title = "Mean number of gametes produced")+scale_fill_discrete(name = "maturity coefficient")+scale_color_discrete(name = "maturity coefficient")+scale_shape_discrete(name = "maturity coefficient")

dev.off()




migrationSumTable <-table(longDeath%>%select(maturity.coefficient,p.migration, migrations.sum))

migrationSumTable <- as.data.frame(migrationSumTable)
boxplot(Freq~p.migration, migrationSumTable)
migrationSumTable<- migrationSumTable%>%group_by(migration.coefficient, p.migration)%>%

ggplot(migrationSumTable, aes(y = migrations.sum,x = log10(Freq), colour = factor(maturity.coefficient)))+geom_point()

totalo <- sum(migrationSumTable$Freq)


boxplot(migrations.sum~p.migration+maturity.coefficient, longDeath)
ggplot(longDeath,aes(migrations.sum))




repEventList_1dim <- snipEvent(repEventList_1dim)

for(i in 1:9){
  repEventList_1dim[[i]] <- rename(repEventList_1dim[[i]],"maximumLifeSpan"="maxlifespan")
  repEventList_1dim[[i]]$ofMaxLifeSpan <- repEventList_1dim[[i]]$age/repEventList_1dim[[i]]$maximumLifeSpan
}


longRep <- bind_rows(repEventList_1dim)

FirstRep.df <- longRep%>%filter(repEventNumber == 1)%>%select(time_steps, repEventNumber, age, ofMaxLifeSpan, maximumLifeSpan, pos_x, maturity.coefficient, p.migration)

hist(FirstRep.df$ofMaxLifeSpan[which(FirstRep.df$maturity.coefficient == 0.4 & FirstRep.df$p.migration == 2e-4)], breaks = 16)


ggplot(FirstRep.df,aes(y = ofMaxLifeSpan, x = factor(pos_x)))+facet_grid(scales = "free", rows = vars(maturity.coefficient), cols = vars(p.migration))+geom_col()


mean.Age <- aggregate.data.frame(FirstRep.df$age, list(maturity.coefficient = FirstRep.df$maturity.coefficient, p.migration = FirstRep.df$p.migration, pos_x = FirstRep.df$pos_x), mean)

Age.05 <- aggregate.data.frame(FirstRep.df$age, list(maturity.coefficient = FirstRep.df$maturity.coefficient, p.migration = FirstRep.df$p.migration, pos_x = FirstRep.df$pos_x), quantile,0.05)

Age.95 <- aggregate.data.frame(FirstRep.df$age, list(maturity.coefficient = FirstRep.df$maturity.coefficient, p.migration = FirstRep.df$p.migration, pos_x = FirstRep.df$pos_x), quantile,0.95)

mean.ofMaxLifeSpan <- aggregate.data.frame(FirstRep.df$ofMaxLifeSpan, list(maturity.coefficient = FirstRep.df$maturity.coefficient, p.migration = FirstRep.df$p.migration, pos_x = FirstRep.df$pos_x), mean)

iq.ofMaxLifeSpan <- aggregate.data.frame(FirstRep.df$ofMaxLifeSpan, list(maturity.coefficient = FirstRep.df$maturity.coefficient, p.migration = FirstRep.df$p.migration, pos_x = FirstRep.df$pos_x), IQR)





meanFirstReps.df <- data.frame(
  mean.age = mean.Age$x,
  age.05 = Age.05$x,
  age.95 = Age.95$x,
  pos_x = mean.Age$pos_x,
  maturity.coefficient = mean.Age$maturity.coefficient,
  p.migration = mean.Age$p.migration)


pdf("meanFirstReps.pdf")

ggplot(meanFirstReps.df,aes(p.migration,(mean.age),colour = factor(maturity.coefficient), fill = factor(maturity.coefficient)))+geom_point()+theme_bw()+labs(x = "P(migration)", y = "age", title = "Mean age at first reproductive event")+scale_fill_discrete(name = "maturity coefficient")+scale_color_discrete(name = "maturity coefficient")+geom_smooth(linetype = 2, alpha = 0.3, method = "lm", se = F, linewidth = 0.2)

dev.off()

ggplot(meanFirstReps.df,aes((mean.age),maturity.coefficient,colour = factor(p.migration), fill = factor(p.migration)))+geom_point()+theme_bw()+labs(y = "maturity.coefficient", x = "age", title = "Mean age at first reproductive event")+scale_fill_discrete(name = "P(migration)")+scale_color_discrete(name = "P(migration)")+geom_smooth(linetype = 2, alpha = 0.3, method = "lm", se = F, linewidth = 0.2)



ggplot(meantime,aes(maturity.coefficient,colon.meanExp, fill = factor(maturity.coefficient),color = factor(maturity.coefficient), ymin = colon.Exp.05,ymax = colon.Exp.95))+scale_x_continuous(breaks = c(0.2,0.4, 0.6,0.8))+geom_errorbar(alpha = 0.1, position = "dodge")+geom_line(linetype = 2, alpha = 0.3)+geom_point(size = 2, aes(shape = factor(p.migration)))+theme_bw()+labs(y = "time-steps", x = "maturity coefficient", title = "Mean persistence time")+scale_fill_discrete(name = "Pmaturity.coefficient")+scale_color_discrete(name = "maturity.coefficient")+scale_shape_discrete(name = "maturity.coefficient")





ggplot(meanFirstReps.df,aes(maturity.coefficient,mean.ofMaxLifeSpan, fill = factor(p.migration)))+geom_col(position = "dodge")


MaxReps <- aggregate.data.frame(longRep$repEventNumber,list(maturity.coefficient = longRep$maturity.coefficient, p.migration = longRep$p.migration, pos_x = longRep$pos_x), max)

splittepoplist <- list()
inner.splittepoplist <- list()

for(i in 1:9){
  for(j in 1:10){
     
    inner.splittepoplist[[j]] <- poplist_1dim[[i]]%>%filter(run == j)
  }
  splittepoplist[[i]] <- inner.splittepoplist
}



for(i in 1:9){
  for(j in 1:10){
    splittepoplist[[i]][[j]] <- splittepoplist[[i]][[j]]%>%gather(key = "grid.cell", value = "population.size", -run, -time_steps, -maturity.coefficient, -p.migration)
  }
}


for(i in 1:9){
  for(j in 1:10){
    splittepoplist[[i]][[j]]$initColonized = FALSE
    splittepoplist[[i]][[j]]$ColonStart = FALSE
    splittepoplist[[i]][[j]]$ExtinctStart = FALSE
    #splittepoplist[[i]][[j]]$EmptyCounter = 0
    #splittepoplist[[i]][[j]]$PopulatedCounter = 0
    splittepoplist[[i]][[j]]$isPopulatedEvent = 0
    splittepoplist[[i]][[j]]$isEmptyEvent = 0
    

  }
  
}


isPopulatedEventCounter = 0
isEmptyEventCounter = 0
for(a in 1:9){
  for (b in 1:10){
    isPopulatedEventCounter = 0
    isEmptyEventCounter = 0
    for(c in 1:7687){
      if(c == 1){
        if(
          splittepoplist[[a]][[b]]$population.size[1] > 0 ){
          
          splittepoplist[[a]][[b]]$initColonized[c] = TRUE
          isPopulatedEventCounter = isPopulatedEventCounter+1
          splittepoplist[[a]][[b]]$isPopulatedEvent[c] = isPopulatedEventCounter
          
        }else{
          splittepoplist[[a]][[b]]$initColonized[c] = FALSE
          isEmptyEventCounter = isEmptyEventCounter+1
          splittepoplist[[a]][[b]]$isEmptyEvent[c] = isEmptyEventCounter
          
        }
      }else{
        if(splittepoplist[[a]][[b]]$population.size[c] == 0 ){
          
          splittepoplist[[a]][[b]]$isPopulatedEvent[c] = -1
          
          if(splittepoplist[[a]][[b]]$population.size[c-1] > 0 ){
            
            splittepoplist[[a]][[b]]$ExtinctStart[c] = TRUE
            isEmptyEventCounter = isEmptyEventCounter +1
            splittepoplist[[a]][[b]]$isEmptyEvent[c] = isEmptyEventCounter
            
          }else{
            
            splittepoplist[[a]][[b]]$isEmptyEvent[c] = isEmptyEventCounter
          }
          
        }else{
          
          splittepoplist[[a]][[b]]$isEmptyEvent[c] = -1
          
          if(splittepoplist[[a]][[b]]$population.size[c-1] == 0 ){
            
            splittepoplist[[a]][[b]]$ColonStart[c] = TRUE
            isPopulatedEventCounter = isPopulatedEventCounter +1
            splittepoplist[[a]][[b]]$isPopulatedEvent[c] = isPopulatedEventCounter
            
          }else{
            
            splittepoplist[[a]][[b]]$isPopulatedEvent[c] = isPopulatedEventCounter
          }
        }
      }
      
      
     
      
#       if(
# splittepoplist[[a]][[b]]$population.size[c-1] > 0 &   splittepoplist[[a]][[b]]$population.size[c] == 0){
#         splittepoplist[[a]][[b]]$ExtinctStart[c] = TRUE
# 
# }
#       if(
#         splittepoplist[[a]][[b]]$population.size[c-1] == 0 &   splittepoplist[[a]][[b]]$population.size[c] > 0){
#         splittepoplist[[a]][[b]]$ColonStart[c] = TRUE
#       }
      
      # if(splittepoplist[[a]][[b]]$population.size[c] == 0){
      #   splittepoplist[[a]][[b]]$EmptyCounter[c] =     splittepoplist[[a]][[b]]$EmptyCounter[c-1] +1
      #   splittepoplist[[a]][[b]]$PopulatedCounter[c] = 0
      #   
      #   
      # }
      # if(splittepoplist[[a]][[b]]$population.size[c] > 0){
      #     splittepoplist[[a]][[b]]$EmptyCounter[c] = 0  
      #     splittepoplist[[a]][[b]]$PopulatedCounter[c] =
      #       splittepoplist[[a]][[b]]$PopulatedCounter[c-1]+1
      #   }
      }
    }
  }


# for(a in 1:9){
#   for (b in 1:10){
#     for(c in 2:7687){
#       if(splittepoplist[[a]][[b]]$population.size[c-1] == 0){
#       }
#       
#     }
#   }
# }











splittepoplist.bound <- list()

for(i in 1:9){
  splittepoplist.bound[[i]] <- bind_rows(splittepoplist[[i]])
}

colEve.df <- as.data.frame(table(splittepoplist[[1]][[1]]%>%select(isPopulatedEvent, grid.cell)))

extEve.df <- as.data.frame(table(splittepoplist[[1]][[1]]%>%select(isEmptyEvent, grid.cell)))



lengthOfPopEve.e1.c1 <- list()

for(i in 1:10){
  lengthOfPopEve.e1.c1[[i]] = splittepoplist[[1]][[i]]%>%filter(isPopulatedEvent == 1 & grid.cell == "C1")%>%filter(time_steps == min(time_steps))
}



firstLastIndex = 0

getFirstLasts <- function(inlist){
  gridCellVector <- c("C1","C2","C3","C4","C5","C6","C7","C8")
  bigList <- list()
  expList.A <- list()
  runList.A <- list()
  cellList.A <- list()
  expList.B <- list()
  runList.B <- list()
  cellList.B <- list()
  eventList.A <- list()
  eventList.B <- list()
  for(a in 1:9){
    for(b in 1:10){
      for(c in 1:8){
        tempNumPopEvents <- max(splittepoplist[[a]][[b]]$isPopulatedEvent[which(splittepoplist[[a]][[b]]$grid.cell == gridCellVector[c])])

        tempNumEmptyEvents <- max(splittepoplist[[a]][[b]]$isEmptyEvent[which(splittepoplist[[a]][[b]]$grid.cell == gridCellVector[c])])
        
        for(i in 0:tempNumPopEvents){
          eventList.A[[i+1]] = splittepoplist[[a]][[b]]%>%filter(isPopulatedEvent == i  & grid.cell == gridCellVector[c])%>%filter(time_steps == min(time_steps) | time_steps == max(time_steps))
        }
        cellList.A[[c]] = eventList.A
        
        for(i in 0:tempNumEmptyEvents){
          eventList.B[[i+1]] = splittepoplist[[a]][[b]]%>%filter(isEmptyEvent == i  & grid.cell == gridCellVector[c])%>%filter(time_steps == min(time_steps) | time_steps == max(time_steps))
        }
        cellList.B[[c]] = eventList.B

      }
      runList.A[[b]] = cellList.A
      runList.B[[b]] = cellList.B

    }
    expList.A[[b]] = runList.A
    expList.B[[b]] = runList.B
  }
  big.list [[1]] = expList.A
  big.list [[2]] = expList.B
  
  return(big.list)
}

testget <- getFirstLasts(splittepoplist)







gridCellVector <- c("C1","C2","C3","C4","C5","C6","C7","C8")

splittepoplist[[1]][[1]]$isPopulatedEvent[which(splittepoplist[[1]][[1]]$grid.cell == gridCellVector[1])]






splittepoplist[[1]][[1]]%>%filter(splittepoplist[[1]][[1]]$isPopulatedEvent == 1)





summary(splittepoplist.bound[[1]]%>%filter(grid.cell == "C1"))

what <- splittepoplist.bound[[1]]%>%filter(grid.cell == "C1")
summary(what)






for(nExp in 1:9){
  for(nRun in 1:10){
    for(nCell in 1:8){
      for(nRow in 1:8){
        
      }
    }
    
    splittepoplist[[i]][[j]]$isEmpty <- is.
  }
}




getPlateau <- function(df){
  
  temp.df <- df%>%group_by(experiment.ID)%>%filter(time_steps >= 25000 & time_steps <= 30000)
  
  out.df <- data.frame(experiment.ID = experimentNames)
  out.df$maturity.coefficient <- matCoefs
  out.df$p.migration <- pmigs
  out.df$N <- 0
  out.df$percentile.05 <- 0
  out.df$percentile.95 <- 0
  out.df$ofTotal <- 0
  out.df$ofTotal.05 <- 0
  out.df$ofTotal.95 <- 0
  for(i in 1:9){
    out.df$N[i] <- mean(temp.df$N[which(temp.df$experiment.ID == experimentNames[i])])
    out.df$percentile.05[i] <- quantile(temp.df$N[which(temp.df$maturity.coefficient == matCoefs[i] & temp.df$p.migration == pmigs[i])], 0.05)
    out.df$percentile.95[i] <- quantile(temp.df$N[which(temp.df$experiment.ID == experimentNames[i])], 0.95)
    out.df$ofTotal[i] <- out.df$N[i]/8192
    out.df$ofTotal.05[i] <- out.df$percentile.05[i]/8192
    out.df$ofTotal.95[i] <- out.df$percentile.95[i]/8192
    
  }
  
  out.df$p.migration <- factor(out.df$p.migration)
  return(out.df)
}


plateau.Pop <- getPlateau(superPop.gat)
plateau.freenute <- getPlateau(superfreeNute.gat)
plateau.locked <- getPlateau(superlockedNute.gat)
plateau.self <- getPlateau(superSelf.gat)



pdf("finalplateaus.pdf")

ggplot(plateau.self,aes(maturity.coefficient,N, fill = p.migration, colour = p.migration,ymin = percentile.05, ymax = percentile.95))+geom_errorbar(alpha = 0.3)+geom_line(linetype = 2, alpha = 0.3)+geom_point()+theme_bw()+
  labs(x = "maturity coefficient", y = "mean selfing ratio", title = "Mean global selfing ratio\nat population plateau", subtitle = "Means over 10 replicates from t = 2.5e4 to t = 3e4")+scale_fill_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_color_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8))



ggplot(plateau.locked,aes(maturity.coefficient,ofTotal*100, fill = p.migration, colour = p.migration,ymin = ofTotal.05*100, ymax = ofTotal.95*100))+geom_errorbar(alpha = 0.3)+geom_line(linetype = 2, alpha = 0.3)+geom_point()+theme_bw()+labs(x = "maturity coefficient", y = "locked / total nutrients (%)", title = "Mean proportion of global locked nutrients\nat population plateau", subtitle = "Means over 10 replicates from t = 2.5e4 to t = 3e4")+scale_fill_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_color_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8))


ggplot(plateau.freenute,aes(maturity.coefficient,ofTotal*100, fill = p.migration, colour = p.migration,ymin = ofTotal.05*100, ymax = ofTotal.95*100))+geom_errorbar(alpha = 0.3)+geom_line(linetype = 2, alpha = 0.3)+geom_point()+theme_bw()+labs(x = "maturity coefficient", y = "free / total nutrients (%)", title = "Mean global free nutrients\nat population plateau", subtitle = "Means over 10 replicates from t = 2.5e4 to t = 3e4")+scale_fill_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_color_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8))




ggplot(plateau.Pop,aes(maturity.coefficient,N, fill = p.migration, colour = p.migration,ymin = percentile.05, ymax = percentile.95))+geom_errorbar(alpha = 0.3)+geom_line(linetype = 2, alpha = 0.3)+geom_point()+theme_bw()+
  labs(x = "maturity coefficient", y = "population size", title = "Mean global population size\nat population plateau", subtitle = "Means over 10 replicates from t = 2.5e4 to t = 3e4")+scale_fill_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_color_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8))

dev.off()

#+scale_y_continuous(breaks = seq(78,97,2), limits = c(78,97))


Atplateau2 <- superlockedNute.gat%>%group_by(experiment.ID)%>%filter(time_steps >= 25000 & time_steps <= 30000)

Atplateau2 <- superPop.gat%>%group_by(experiment.ID)%>%filter(time_steps >= 25000 & time_steps <= 30000)

summary(Atplateau2)

meanAtplateau <- data.frame(experiment.ID = experimentNames)
meanAtplateau$maturity.coefficient <- matCoefs
meanAtplateau$p.migration <- pmigs
meanAtplateau$mean.lockednute <- 0
meanAtplateau$percentile.05 <- 0
meanAtplateau$percentile.95 <- 0
meanAtplateau$ofTotalNutrients <- 0
meanAtplateau$ofTotalNutrients.05 <- 0
meanAtplateau$ofTotalNutrients.95 <- 0
for(i in 1:9){
  meanAtplateau$mean.lockednute[i] <- mean(Atplateau$N[which(Atplateau$experiment.ID == experimentNames[i])])
  meanAtplateau$percentile.05[i] <- quantile(Atplateau$N[which(Atplateau$experiment.ID == experimentNames[i])], 0.05)
  meanAtplateau$percentile.95[i] <- quantile(Atplateau$N[which(Atplateau$experiment.ID == experimentNames[i])], 0.95)
  meanAtplateau$ofTotalNutrients[i] <- meanAtplateau$mean.lockednute[i]/8192
  meanAtplateau$ofTotalNutrients.05[i] <- meanAtplateau$percentile.05[i]/8192
  meanAtplateau$ofTotalNutrients.95[i] <- meanAtplateau$percentile.95[i]/8192
  
}



meanAtplateau2 <- meanAtplateau
meanAtplateau2$p.migration <- factor(meanAtplateau2$p.migration)







ggplot(meanAtplateau2,aes(maturity.coefficient,mean.lockednute, fill = p.migration, colour = p.migration,ymin = percentile.05, ymax = percentile.95))+geom_errorbar(alpha = 0.3)+geom_line(linetype = 2, alpha = 0.3)+geom_point()+theme_bw()+labs(x = "maturity coefficient", y = "populat", title = "Mean global population size\nat population plateau", subtitle = "Means over 10 replicates from t = 2.5e4 to t = 3e4")+scale_fill_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_color_discrete("P(migration)", labels = c("2e-4","4e-4","8e-4"))+scale_x_continuous(breaks = c(0.2,0.4,0.8))



ggplot(meanAtplateau,aes(maturity.coefficient,ofTotalNutrients,ymin = ofTotalNutrients.05, ymax = ofTotalNutrients.95))+geom_point()+geom_pointrange()+geom_line(linetype = 2)+facet_grid(rows = vars(p.migration),labeller = label_both)


