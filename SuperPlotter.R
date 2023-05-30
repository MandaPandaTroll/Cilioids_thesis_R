#SuperPlotter
rm(list = ls())
gc()
library(tidyverse)
pdf("GlobalStats_AllExperiments.pdf")


logroes <- data.frame(t = seq(1:1000), r_.01 = logro(2,seq(1:1000),0.01,1000), r_.02 = logro(2,seq(1:1000),0.015,1000), r_.04 = logro(2,seq(1:1000),0.02,1000), r_.08 = logro(2,seq(1:1000),0.025,1000))

logroes <- logroes%>%gather(key = "intrinsic.growth.rate", value = "population.size", -t)


ggplot(logroes,aes(t,population.size, colour = intrinsic.growth.rate))+geom_line(linewidth = 2)+geom_hline(aes(yintercept = 1000), linetype = 2, colour = "black", linewidth = 2)+labs(y = "N(t)")+scale_colour_discrete(name = "intrinsic growth rate (r)", labels = c(0.01, 0.015, 0.02, 0.025) )+theme_bw()



ggplot(meanPops.df,aes(time_steps,mean.pop, fill = experiment.ID, colour =experiment.ID))+geom_ribbon(aes(x = time_steps, ymin = percentile.05, ymax = percentile.95,fill = experiment.ID), alpha = 0.1, linetype = 0)+geom_line( linewidth = 0.5)+labs(y = "mean global population size, (n = 10)", x =  "time-steps",title ="Mean global population size\nwith 5th & 95th percentile (ribbon)\nall experiments", subtitle = "mean over 10 replicates per experiment")+theme_bw()+scale_linetype_manual(values = c(1,1,1,1,1,1,1,1,1))+theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+scale_fill_discrete("Maturity coefficient:P(migration)", labels = c("0.2:2e-4","0.2:4e-4","0.2:8e-4","0.4:2e-4","0.4:4e-4","0.4:8e-4","0.8:2e-4","0.8:4e-4","0.8:8e-4"))+scale_colour_discrete("Maturity coefficient:P(migration)", labels = c("0.2:2e-4","0.2:4e-4","0.2:8e-4","0.4:2e-4","0.4:4e-4","0.4:8e-4","0.8:2e-4","0.8:4e-4","0.8:8e-4"))


ggplot(meanSelf.df,aes(time_steps,mean.self, fill = experiment.ID, colour = experiment.ID))+geom_ribbon(aes(x = time_steps, ymin = percentile.05, ymax = percentile.95), alpha = 0.2, linetype = 0)+geom_line( linewidth = 0.5)+labs(y = "mean global selfing ratio, (n = 10)", x =  "time-steps",title ="Mean global selfing ratio\nwith 5th & 95th percentile (ribbon)\nall experiments", subtitle = "mean over 10 replicates per experiment")+theme_bw()+scale_linetype_manual(values = c(1,1,1,1,1,1,1,1,1))+theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+scale_fill_discrete("Maturity coefficient:P(migration)", labels = c("0.2:2e-4","0.2:4e-4","0.2:8e-4","0.4:2e-4","0.4:4e-4","0.4:8e-4","0.8:2e-4","0.8:4e-4","0.8:8e-4"))+scale_colour_discrete("Maturity coefficient:P(migration)", labels = c("0.2:2e-4","0.2:4e-4","0.2:8e-4","0.4:2e-4","0.4:4e-4","0.4:8e-4","0.8:2e-4","0.8:4e-4","0.8:8e-4"))




ggplot(meanfreeNute.df,aes(time_steps,mean.freenute, fill = experiment.ID, colour = experiment.ID))+geom_ribbon(aes(x = time_steps, ymin = percentile.05, ymax = percentile.95), alpha = 0.2, linetype = 0)+geom_line( linewidth = 0.5)+labs(y = "mean global nutrients, (n = 10)", x =  "time-steps",title ="Mean global free nutrients\nwith 5th & 95th percentile (ribbon)\nall experiments", subtitle = "mean over 10 replicates per experiment")+theme_bw()+scale_linetype_manual(values = c(1,1,1,1,1,1,1,1,1))+theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+scale_fill_discrete("Maturity coefficient:P(migration)", labels = c("0.2:2e-4","0.2:4e-4","0.2:8e-4","0.4:2e-4","0.4:4e-4","0.4:8e-4","0.8:2e-4","0.8:4e-4","0.8:8e-4"))+scale_colour_discrete("Maturity coefficient:P(migration)", labels = c("0.2:2e-4","0.2:4e-4","0.2:8e-4","0.4:2e-4","0.4:4e-4","0.4:8e-4","0.8:2e-4","0.8:4e-4","0.8:8e-4"))

ggplot(meanlockedNute.df,aes(time_steps,mean.lockednute, fill = experiment.ID, colour = experiment.ID))+geom_ribbon(aes(x = time_steps, ymin = percentile.05, ymax = percentile.95), alpha = 0.2, linetype = 0)+geom_line( linewidth = 0.5)+labs(y = "mean global locked nutrients, (n = 10)", x =  "time-steps",title ="Mean global locked nutrients\nwith 5th & 95th percentile (ribbon)\nall experiments", subtitle = "mean over 10 replicates per treatment")+theme_bw()+scale_linetype_manual(values = c(1,1,1,1,1,1,1,1,1))+theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+scale_fill_discrete("Maturity coefficient:P(migration)", labels = c("0.2:2e-4","0.2:4e-4","0.2:8e-4","0.4:2e-4","0.4:4e-4","0.4:8e-4","0.8:2e-4","0.8:4e-4","0.8:8e-4"))+scale_colour_discrete("Maturity coefficient:P(migration)", labels = c("0.2:2e-4","0.2:4e-4","0.2:8e-4","0.4:2e-4","0.4:4e-4","0.4:8e-4","0.8:2e-4","0.8:4e-4","0.8:8e-4"))



dev.off()







pointsize <- 0.25

pdf("popInCellsVbig.pdf", height =16, width = 32)

ggplot(meanPop_1dim.gat%>%filter(p.migration == 2e-4),aes(time_steps, N,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = N-N.sd,ymax = N+N.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean population/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 2e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanPop_1dim.gat$N[which(meanPop_1dim.gat$p.migration == 2e-4)]))))


ggplot(meanPop_1dim.gat%>%filter(p.migration == 4e-4),aes(time_steps, N,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = N-N.sd,ymax = N+N.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean population/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 4e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanPop_1dim.gat$N[which(meanPop_1dim.gat$p.migration == 4e-4)]))))

ggplot(meanPop_1dim.gat%>%filter(p.migration == 8e-4),aes(time_steps, N,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = N-N.sd,ymax = N+N.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean population/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 8e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanPop_1dim.gat$N[which(meanPop_1dim.gat$p.migration == 8e-4)]))))

dev.off()
gc()




pdf("lockedNutrientsInCellsVbig.pdf", height =16, width = 32)


ggplot(meanlockednute_1dim.gat%>%filter(p.migration == 2e-4),aes(time_steps, locked.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = locked.nutrients-locked.nutrients.sd,ymax = locked.nutrients+locked.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean locked nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 2e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanlockednute_1dim.gat$locked.nutrients[which(meanlockednute_1dim.gat$p.migration == 2e-4)]))))


ggplot(meanlockednute_1dim.gat%>%filter(p.migration == 4e-4),aes(time_steps, locked.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = locked.nutrients-locked.nutrients.sd,ymax = locked.nutrients+locked.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean locked nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 4e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanlockednute_1dim.gat$locked.nutrients[which(meanlockednute_1dim.gat$p.migration == 4e-4)]))))

ggplot(meanlockednute_1dim.gat%>%filter(p.migration == 8e-4),aes(time_steps, locked.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = locked.nutrients-locked.nutrients.sd,ymax = locked.nutrients+locked.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean locked nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 8e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanlockednute_1dim.gat$locked.nutrients[which(meanlockednute_1dim.gat$p.migration == 8e-4)]))))

dev.off()
gc()


pdf("freeNutrientsInCellsVbig.pdf", height =16, width = 32)


ggplot(meanfreenute_1dim.gat%>%filter(p.migration == 2e-4),aes(time_steps, free.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = free.nutrients-free.nutrients.sd,ymax = free.nutrients+free.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean free nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 2e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanfreenute_1dim.gat$free.nutrients[which(meanfreenute_1dim.gat$p.migration == 2e-4)]))))


ggplot(meanfreenute_1dim.gat%>%filter(p.migration == 4e-4),aes(time_steps, free.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = free.nutrients-free.nutrients.sd,ymax = free.nutrients+free.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean free nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 4e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanfreenute_1dim.gat$free.nutrients[which(meanfreenute_1dim.gat$p.migration == 4e-4)]))))

ggplot(meanfreenute_1dim.gat%>%filter(p.migration == 8e-4),aes(time_steps, free.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = free.nutrients-free.nutrients.sd,ymax = free.nutrients+free.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean free nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 8e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanfreenute_1dim.gat$free.nutrients[which(meanfreenute_1dim.gat$p.migration == 8e-4)]))))

dev.off()
gc()






pdf("totalNutrientsInCellsVbig.pdf", height =16, width = 32)


ggplot(meantotalnute_1dim.gat%>%filter(p.migration == 2e-4),aes(time_steps, total.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = total.nutrients-total.nutrients.sd,ymax = total.nutrients+total.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean total nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 2e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meantotalnute_1dim.gat$total.nutrients[which(meantotalnute_1dim.gat$p.migration == 2e-4)]))))


ggplot(meantotalnute_1dim.gat%>%filter(p.migration == 4e-4),aes(time_steps, total.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = total.nutrients-total.nutrients.sd,ymax = total.nutrients+total.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean total nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 4e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meantotalnute_1dim.gat$total.nutrients[which(meantotalnute_1dim.gat$p.migration == 4e-4)]))))

ggplot(meantotalnute_1dim.gat%>%filter(p.migration == 8e-4),aes(time_steps, total.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = total.nutrients-total.nutrients.sd,ymax = total.nutrients+total.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean total nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 8e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meantotalnute_1dim.gat$total.nutrients[which(meantotalnute_1dim.gat$p.migration == 8e-4)]))))

dev.off()
gc()





pdf("lockedtofreeNutrientsInCellsVbig.pdf", height =16, width = 32)


ggplot(meanlockedtofreenute_1dim.gat%>%filter(p.migration == 2e-4),aes(time_steps, lockedtofree.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = lockedtofree.nutrients-lockedtofree.nutrients.sd,ymax = lockedtofree.nutrients+lockedtofree.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean lockedtofree nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 2e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanlockedtofreenute_1dim.gat$lockedtofree.nutrients[which(meanlockedtofreenute_1dim.gat$p.migration == 2e-4)]))))


ggplot(meanlockedtofreenute_1dim.gat%>%filter(p.migration == 4e-4),aes(time_steps, lockedtofree.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = lockedtofree.nutrients-lockedtofree.nutrients.sd,ymax = lockedtofree.nutrients+lockedtofree.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean lockedtofree nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 4e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanlockedtofreenute_1dim.gat$lockedtofree.nutrients[which(meanlockedtofreenute_1dim.gat$p.migration == 4e-4)]))))

ggplot(meanlockedtofreenute_1dim.gat%>%filter(p.migration == 8e-4),aes(time_steps, lockedtofree.nutrients,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = lockedtofree.nutrients-lockedtofree.nutrients.sd,ymax = lockedtofree.nutrients+lockedtofree.nutrients.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean lockedtofree nutrients/time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 8e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanlockedtofreenute_1dim.gat$lockedtofree.nutrients[which(meanlockedtofreenute_1dim.gat$p.migration == 8e-4)]))))

dev.off()
gc()
























pdf("selfratioInCellsVbig.pdf", height =16, width = 32)


ggplot(meanselfratio_1dim.gat%>%filter(p.migration == 2e-4),aes(time_steps, self.ratio,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = self.ratio-self.ratio.sd,ymax = self.ratio+self.ratio.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean selfing ratio /time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 2e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanselfratio_1dim.gat$self.ratio[which(meanselfratio_1dim.gat$p.migration == 2e-4)]))))


ggplot(meanselfratio_1dim.gat%>%filter(p.migration == 4e-4),aes(time_steps, self.ratio,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = self.ratio-self.ratio.sd,ymax = self.ratio+self.ratio.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean selfing ratio /time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 4e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanselfratio_1dim.gat$self.ratio[which(meanselfratio_1dim.gat$p.migration == 4e-4)]))))

ggplot(meanselfratio_1dim.gat%>%filter(p.migration == 8e-4),aes(time_steps, self.ratio,colour = maturity.coefficient))+
  geom_ribbon(aes(x = time_steps, ymin = self.ratio-self.ratio.sd,ymax = self.ratio+self.ratio.sd, fill = maturity.coefficient), linetype = 2, linewidth = 0.1, alpha = 0.05)+
  geom_point(size = pointsize)+
  facet_grid(rows = vars(grid.cell), as.table = T)+
  geom_line(linewidth = 0.25, linetype = 1)+
  theme_bw()+
  labs(title = "Mean selfing ratio /time step (over 10x3 runs)\nin cell[8,8] to [15,8], with SD (ribbon)", subtitle = "p.migration = 8e-4")+
  scale_fill_discrete(breaks = c(0.2,0.4,0.8))+
  scale_color_discrete(breaks = c(0.2,0.4,0.8))+
  theme(legend.position = "bottom" ,legend.text = element_text(size = 6),legend.title = element_text(size = 7))+
  coord_cartesian(ylim = c(0,(max(meanselfratio_1dim.gat$self.ratio[which(meanselfratio_1dim.gat$p.migration == 8e-4)]))))

dev.off()
gc()












#

#

longDeath <- longDeath%>%group_by(maturity.coefficient,p.migration,causeOfDeath)%>%mutate(age.mean = mean(age), ofmax.mean = mean(ofMaxLifeSpan), age.05 = quantile(age,0.05), age.95 = quantile(age,0.95), ofmax.05 = quantile(ofMaxLifeSpan,0.05), ofmax.95 = quantile(ofMaxLifeSpan,0.95))
longDeath <- longDeath%>%ungroup()%>%group_by(maturity.coefficient,p.migration)%>%mutate(mean.age.byExp = mean(age))

summary(longDeath)
longDeath <- longDeath%>%filter(causeOfDeath != "age_starvation")

longDeath$allmeanAge <- mean(longDeath$age)
longDeath$allmeanOfMax <- mean(longDeath$ofMaxLifeSpan)




onlymeans <- longDeath%>%distinct(p.migration,maturity.coefficient,causeOfDeath, .keep_all = T)
onlymeans$maturity.coefficient = factor(onlymeans$maturity.coefficient)
ggplot(onlymeans%>%filter(causeOfDeath == "starvation"),aes(p.migration,age.mean, ymin = age.05, ymax = age.95, colour = maturity.coefficient))+geom_errorbar()+geom_point()



cod.df <- as.data.frame(table(longDeath%>%select(maturity.coefficient,p.migration,causeOfDeath)))
cod.df <- cod.df%>%group_by(p.migration,maturity.coefficient)%>%mutate(total = sum(Freq))
cod.df <- cod.df%>%group_by(p.migration,maturity.coefficient,causeOfDeath)%>%mutate(percy = Freq/total)



pdf("FINALglobalDeathAgeHistograms.pdf")

ggplot(longDeath,aes(age), fill = "grey")+geom_histogram()+geom_vline(aes(xintercept = mean.age.byExp))+facet_grid(rows = vars(maturity.coefficient),cols = vars(p.migration),labeller = label_both, as.table = F)+theme_bw()+labs(x ="Age (steps)", title = "Age at death")


ggplot(longDeath,aes(age, fill = causeOfDeath, xintercept = age.mean))+geom_histogram(alpha = 0.9,colour = "black", bins=32, linewidth = 0.05)+geom_vline(aes(xintercept = age.mean,colour = causeOfDeath) )+geom_vline(aes(xintercept = mean.age.byExp))+facet_grid(rows = vars(maturity.coefficient),cols = vars(p.migration),labeller = label_both, as.table = F)+labs(x ="Age (time-steps)", title = "Age at death & cause of death")+theme_bw()+scale_fill_discrete(name = "cause of death", labels = c("age", "E = 0"))+scale_color_discrete(name = "cause of death", labels = c("age", "E = 0"))



ggplot(longDeath%>%filter(causeOfDeath != "age"),aes(age))+geom_histogram(fill = "grey", alpha = 0.9,colour = "black", bins = 32, linewidth = 0.05)+geom_vline(aes(xintercept = age.mean)) +facet_grid(rows = vars(maturity.coefficient),cols = vars(p.migration),labeller = label_both, as.table = F)+labs(x ="Age (steps)", title = "Age at death\ncause of death: E = 0")+theme_bw()



ggplot(longDeath%>%filter(ofMaxLifeSpan < 1),aes(ofMaxLifeSpan*100 ))+geom_histogram(fill = "grey", alpha = 0.9,colour = "black", bins = 32, linewidth = 0.05)+geom_vline(aes(xintercept = ofmax.mean*100))+facet_grid(rows = vars(maturity.coefficient),cols = vars(p.migration),labeller = label_both, as.table = F)+labs(x ="Age (% of max age) ", title = "Percent of maximum age at death\ncause of death: E = 0")+theme_bw()



ggplot(cod.df,aes(causeOfDeath,percy*100))+geom_col(fill = "grey", colour = "black")+facet_grid()+facet_grid(rows = vars(maturity.coefficient),cols = vars(p.migration),labeller = label_both, as.table = F)+labs(y = "deaths (%)", x = "cause of death" , title = "Cause of death proportion")+scale_x_discrete(labels = c("age", "E = 0"))+theme_bw()





dev.off()




ggplot(longDeath%>%filter(ofMaxLifeSpan < 1),aes(ofMaxLifeSpan*100))+geom_histogram(fill = "grey", alpha = 0.9,colour = "black", bins = 32, linewidth = 0.05)+labs(x ="Age (% of maxLifeSpan)", title = "% of max life span at death & cause of death\n over all experiments, histogram")





pdf("numRepeventsBarplots.pdf")


ggplot(longDeath,aes(x = reproductiveEvents))+geom_bar(position = "dodge")+facet_grid(rows =vars(maturity.coefficient), cols = vars(p.migration))+scale_x_discrete("reproductiveEvents", limits = c(0,1,2,3,4,5,6,7,8,9,10))+labs(title = "Reproductive events")



ggplot(longDeath%>%filter(reproductiveEvents > 0),aes(x = reproductiveEvents))+geom_bar(position = "dodge")+facet_grid(rows =vars(maturity.coefficient), cols = vars(p.migration))+scale_x_discrete("reproductiveEvents", limits = c(1,2,3,4,5,6,7,8,9,10))+labs(title = "Reproductive events, > 0")




meanRepList_death.mean <- list()
meanRepList_death.quant <- list()
for(i in 1:9){
 meanRepList_death.mean[[i]] <- mean(deathEventList_1dim[[i]]$reproductiveEvents)
 meanRepList_death.quant[[i]] <- IQR(deathEventList_1dim[[i]]$reproductiveEvents)
 
}

meanrepDeath.df <- data.frame(mean.reproductiveEvents = unlist(meanRepList_death.mean), quartile = unlist(meanRepList_death.quant))
meanrepDeath.df$maturity.coefficient <-matCoefs
meanrepDeath.df$p.migration <- pmigs




dev.off()


longPop <- bind_rows(poplist_1dim)

AAexpLevel <- list()
ABrunLevel <- list()
ACcellevel <- list()

indexo <- 1

longPop$isEmpty = FALSE

for(x in 1:9){
  for(y in 1:10){
    for(z in 1:8){
     
     ACcellevel[[z]] <- poplist_1dim[[x]]$time_steps[which(poplist_1dim[[x]]$run == y & poplist_1dim[[x]][,z+1] == 0)]
     names(ACcellevel)[z] =paste0("cell",z,"run",y,"exp",x)
     
     indexo = indexo+1
    }
    ABrunLevel[[y]] = ACcellevel
  }
  AAexpLevel[[x]] = ABrunLevel
}


longestpopCell <- longPop%>%select( -run, -maturity.coefficient, -p.migration)%>%gather(key = "cell", value = "population.size", - time_steps)



exp1c1times <- list()

for(i in 1:10){
  exp1c1times[[i]] <- ((AAexpLevel[[1]][[i]][[1]]))
}
exp1c1timesun <- unlist(exp1c1times)

ggplot(poplist_1dim[[1]],aes(time_steps,C1,colour = factor(run)))+geom_line()+facet_grid(rows = vars(run))





plot(poplist_1dim[[1]]$time_steps[which(poplist_1dim[[1]]$run == 2)],poplist_1dim[[1]]$C1[which(poplist_1dim[[1]]$run == 2)])
abline(v = (exp1c1times[[2]]))

ggplot(longPop%>%filter(run == 1, p.migration == 2e-4, maturity.coefficient == 0.2), aes(time_steps,C1 ))+geom_line()+
  geom_vline(xintercept = longPop$time_steps[which(longPop$C1 == 0 & longPop$run == 1 & longPop$p.migration == 2e-4 & longPop$maturity.coefficient == 0.2)])




zerotimes[1] = 0
which(poplist_1dim[[1]]$C1 == 0 & poplist_1dim[[1]]$run == 1)
zerotimes[2:18] <- poplist_1dim[[1]]$time_steps



splittepoplist.long <- bind_rows(splittepoplist)



selfratiolist_1dim.gat = selfratiolist_1dim
for(i in 1:9){
  selfratiolist_1dim.gat[[i]] = selfratiolist_1dim.gat[[i]]%>%gather(key = "grid.cell", value = "value", - run, -time_steps, -maturity.coefficient, -p.migration)
}
selfratiolist_1dim.gat <- bind_rows(selfratiolist_1dim.gat)


pdf("localpopnself_run3_patch7.pdf")

ggplot(selfratiolist_1dim.gat%>%filter(run == 3, grid.cell == "C7"), aes(time_steps,value,fill = run))+facet_grid(rows = vars(maturity.coefficient), cols = vars(p.migration), labeller = label_both)+geom_line(alpha = 0.7, linewidth = 0.5)+theme_bw()+labs(y = "selfing ratio", x = "time-steps", title = "Local selfing ratios")


ggplot(splittepoplist.long%>%filter(run == 3, grid.cell == "C7"), aes(time_steps,population.size,fill = run))+facet_grid(rows = vars(maturity.coefficient), cols = vars(p.migration), labeller = label_both)+geom_line(alpha = 0.7, linewidth = 0.5)+theme_bw()+labs(y = "population size", x = "time-steps", title = "Local population sizes")

dev.off()


iqrList <- list()

for(i in 1:9){
  iqrList[[i]] <- IQR(deathEventList_1dim[[i]]$reproductiveEvents)
}

