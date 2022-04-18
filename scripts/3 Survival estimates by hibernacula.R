getwd()
setwd('E:/R/pittag project/pittag analysis')
library(ggplot2)
library(dplyr)
rm(list=ls())

####ROUND 1####
####SG and SG##
#load the data and check the structure
surv.est<-read.csv("model estimates SG and AB.csv")
str(surv.est)

#subset by data type (eg survival prob vs recapture prob)
phi<-surv.est[which(surv.est$label=='Phi'), ]
p<-surv.est[which(surv.est$label=='p'), ]

#plot out to visualize
theme <-theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 45, hjust = 1))

#apparent survival plots
phiplot<-ggplot(phi, aes(index, estimate, color=cave)) + 
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25)+ 
  xlab('Group')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
phiplot + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

#recapture probability plots
recapplot<-ggplot(p, aes(index, estimate, colour=cave, group=cave)) + 
  geom_point(size=3)+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI, width=0.25)) + theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('Overwinter Year')+ylab('Recapture probability')
recapplot + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

#test for differences between groups
cave <- aov(estimate ~ cave, data=phi)
summary(cave)
group <- aov(estimate ~ index, data=phi)
summary(group)


####SQ##
SQ<-read.csv("model estimates SQ.csv")
str(SQ)
SQphi<-SQ[which(SQ$label=='Phi'), ]
SQp<-SQ[which(SQ$label=='p'), ]

#plot 
SQphiplot<-ggplot(SQphi, aes(year, estimate, color=sex)) + 
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25)+ 
  xlab('Overwintering year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SQphiplot + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

SQpplot<-ggplot(SQp, aes(year, estimate)) + 
  geom_point(size=3)+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25)+ 
  xlab('Overwintering year')+ylab('Recapture probability')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SQpplot + scale_y_continuous(limits = c(0, 1))

####ROUND 2####
est<-read.csv("Model averaging output.csv")
str(est)

all.phi<-est[which(est$label=='Phi'), ]
all.p<-est[which(est$label=='p'), ]
str(all.phi)
str(all.p)

#subset survival estimates
ABadult.phi<-all.phi[which(all.phi$index=='F-MA'& all.phi$cave=='AB'| all.phi$index=='M-MA' & all.phi$cave=='AB'), ]
SGadult.phi<-all.phi[which(all.phi$index=='F-MA'& all.phi$cave=='SG'| all.phi$index=='M-MA' & all.phi$cave=='SG'), ]
SQadult.phi<-all.phi[which(all.phi$index=='F-MA'& all.phi$cave=='SQ'| all.phi$index=='M-MA' & all.phi$cave=='SQ'), ]

ABmy.phi<-all.phi[which(all.phi$index=='F-MY'& all.phi$cave=='AB'| all.phi$index=='M-MY' & all.phi$cave=='AB'), ]
SGmy.phi<-all.phi[which(all.phi$index=='F-MY'& all.phi$cave=='SG'| all.phi$index=='M-MY' & all.phi$cave=='SG'), ]
SQmy.phi<-all.phi[which(all.phi$index=='F-MY'& all.phi$cave=='SQ'| all.phi$index=='M-MY' & all.phi$cave=='SQ'), ]

ABjuv.phi<-all.phi[which(all.phi$index=='F-JUV'& all.phi$cave=='AB'| all.phi$index=='M-JUV' & all.phi$cave=='AB'), ]
SGjuv.phi<-all.phi[which(all.phi$index=='F-JUV'& all.phi$cave=='SG'| all.phi$index=='M-JUV' & all.phi$cave=='SG'), ]
SQjuv.phi<-all.phi[which(all.phi$index=='F-JUV'& all.phi$cave=='SQ'| all.phi$index=='M-JUV' & all.phi$cave=='SQ'), ]

ABrt.phi<-all.phi[which(all.phi$index=='F-RT'& all.phi$cave=='AB'| all.phi$index=='M-RT' & all.phi$cave=='AB'), ]
SGrt.phi<-all.phi[which(all.phi$index=='F-RT'& all.phi$cave=='SG'| all.phi$index=='M-RT' & all.phi$cave=='SG'), ]
SQrt.phi<-all.phi[which(all.phi$index=='F-RT'& all.phi$cave=='SQ'| all.phi$index=='M-RT' & all.phi$cave=='SQ'), ]

#subset recapture estimates
AB.p<-all.p %>% filter(cave=='AB', index=='F-MA' &cave=='AB'| index=='F-MY'&cave=='AB'| index=='M-MA' & cave=='AB'| index=='M-MY')
SG.p<-all.p %>% filter(cave=='SG', index=='F-MA' &cave=='SG'| index=='F-MY'&cave=='SG'| index=='M-MA' & cave=='SG'| index=='M-MY')
SQ.p<-all.p %>% filter(cave=='SQ', index=='F-MA' &cave=='SQ'| index=='F-MY'&cave=='SQ'| index=='M-MA' & cave=='SQ'| index=='M-MY')

#plot out to visualize
#general theme format
theme <-theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 45, hjust = 1))

###survival plots
##Abyss
#marked as adults (just resident survival)
ABad.phip<-ggplot(ABadult.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
ABad.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))
##Is the sharp decline just age mortality? We don't know how old they were at the start...

#marked as juveniles
ABmy.phip<-ggplot(ABmy.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
ABmy.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

#first year juvenile survival
ABjuv.phip<-ggplot(ABjuv.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
ABjuv.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

#First year resident + transients
ABrt.phip<-ggplot(ABrt.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
ABrt.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

##St George
#marked as adults (just resident survival)
SGad.phip<-ggplot(SGadult.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SGad.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1)) 

#marked as juveniles
SGmy.phip<-ggplot(SGmy.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SGmy.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

#first year juvenile survival
SGjuv.phip<-ggplot(SGjuv.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SGjuv.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

#First year resident + transients
SGrt.phip<-ggplot(SGrt.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SGrt.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

##Squeaky
#adult survival (just residents)
SQad.phip<-ggplot(SQadult.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SQad.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

#adult survival (residents+transients)
SQrt.phip<-ggplot(SQrt.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_path()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SQrt.phip + scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

###recapture probability plots
#ABYSS
ABp.p<-ggplot(AB.p, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25, position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
ABp.p + scale_color_manual(values=c("red1", "blue1", "orange1", "green4")) + scale_y_continuous(limits = c(0, 1))

#ST. GEORGE
SGp.p<-ggplot(SG.p, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25, position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SGp.p + scale_color_manual(values=c("red1", "blue1", "orange1", "green4")) + scale_y_continuous(limits = c(0, 1))

#SQUEAKY
SQp.p<-ggplot(SQ.p, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25, position = position_dodge(0.25))+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme
SQp.p + scale_color_manual(values=c("red1", "blue1", "orange1", "green4")) + scale_y_continuous(limits = c(0, 1))
