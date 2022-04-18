getwd()
setwd('E:/R/pittag project/pittag analysis')
library(ggplot2)
library(dplyr)
library(lme4)
library(lmerTest)
rm(list=ls())

#read.csv
est_w<-read.csv('Survival estimates with weather data.csv')
str(est_w)
head(est_w)

###subsetting
#subset survival estimates
ABadult.phi<-est_w[which(est_w$index=='F-MA'& est_w$cave=='AB' & est_w$label=='Phi'| est_w$index=='M-MA' & est_w$cave=='AB' & est_w$label=='Phi'), ]
SGadult.phi<-est_w[which(est_w$index=='F-MA'& est_w$cave=='SG' & est_w$label=='Phi'| est_w$index=='M-MA' & est_w$cave=='SG' & est_w$label=='Phi'), ]
SQadult.phi<-est_w[which(est_w$index=='F-MA'& est_w$cave=='SQ' & est_w$label=='Phi'| est_w$index=='M-MA' & est_w$cave=='SQ' & est_w$label=='Phi'), ]

ABmy.phi<-est_w[which(est_w$index=='F-MY'& est_w$cave=='AB' & est_w$label=='Phi'| est_w$index=='M-MY' & est_w$cave=='AB' & est_w$label=='Phi'), ]
SGmy.phi<-est_w[which(est_w$index=='F-MY'& est_w$cave=='SG' & est_w$label=='Phi'| est_w$index=='M-MY' & est_w$cave=='SG' & est_w$label=='Phi'), ]

ABjuv.phi<-est_w[which(est_w$index=='F-JUV'& est_w$cave=='AB' & est_w$label=='Phi'| est_w$index=='M-JUV' & est_w$cave=='AB'& est_w$label=='Phi'), ]
SGjuv.phi<-est_w[which(est_w$index=='F-JUV'& est_w$cave=='SG' & est_w$label=='Phi'| est_w$index=='M-JUV' & est_w$cave=='SG'& est_w$label=='Phi'), ]

ABrt.phi<-est_w[which(est_w$index=='F-RT'& est_w$cave=='AB' & est_w$label=='Phi'| est_w$index=='M-RT' & est_w$cave=='AB'& est_w$label=='Phi'), ]
SGrt.phi<-est_w[which(est_w$index=='F-RT'& est_w$cave=='SG' & est_w$label=='Phi'| est_w$index=='M-RT' & est_w$cave=='SG'& est_w$label=='Phi'), ]
SQrt.phi<-est_w[which(est_w$index=='F-RT'& est_w$cave=='SQ' & est_w$label=='Phi'| est_w$index=='M-RT' & est_w$cave=='SQ'& est_w$label=='Phi'), ]

#subset recapture estimates
AB.p<-est_w %>% filter(cave=='AB', index=='F-MA' &cave=='AB' & est_w$label=='p'| index=='F-MY'&cave=='AB' & est_w$label=='p'| index=='M-MA' & cave=='AB'& est_w$label=='p'| index=='M-MY' & est_w$label=='p')
SG.p<-est_w %>% filter(cave=='SG', index=='F-MA' &cave=='SG' & est_w$label=='p'| index=='F-MY'&cave=='SG' & est_w$label=='p'| index=='M-MA' & cave=='SG'& est_w$label=='p'| index=='M-MY' & est_w$label=='p')
SQ.p<-est_w %>% filter(cave=='SQ', index=='F-MA' &cave=='SQ' & est_w$label=='p'| index=='F-MY'&cave=='SQ' & est_w$label=='p'| index=='M-MA' & cave=='SQ'& est_w$label=='p'| index=='M-MY' & est_w$label=='p')

####regression analysis####
###abyss
##winter weather
summary(lm(estimate ~ W_mintemp, data=ABadult.phi))
summary(lm(estimate ~ W_maxtemp, data=ABadult.phi))
summary(lm(estimate ~ W_meantemp, data=ABadult.phi))
summary(lm(estimate ~ W_heatdegdays, data=ABadult.phi))
summary(lm(estimate ~ W_totalprecip, data=ABadult.phi))
summary(lm(estimate ~ W_maxwind, data=ABadult.phi)) 

summary(lm(estimate ~ W_mintemp, data=ABmy.phi))
summary(lm(estimate ~ W_maxtemp, data=ABmy.phi))
summary(lm(estimate ~ W_meantemp, data=ABmy.phi))
summary(lm(estimate ~ W_heatdegdays, data=ABmy.phi))
summary(lm(estimate ~ W_totalprecip, data=ABmy.phi))
summary(lm(estimate ~ W_maxwind, data=ABmy.phi))  

summary(lm(estimate ~ W_mintemp, data=ABjuv.phi))
summary(lm(estimate ~ W_maxtemp, data=ABjuv.phi))
summary(lm(estimate ~ W_meantemp, data=ABjuv.phi))
summary(lm(estimate ~ W_heatdegdays, data=ABjuv.phi))
summary(lm(estimate ~ W_totalprecip, data=ABjuv.phi))
summary(lm(estimate ~ W_maxwind, data=ABjuv.phi))

summary(lm(estimate ~ W_mintemp, data=ABrt.phi))
summary(lm(estimate ~ W_maxtemp, data=ABrt.phi))
summary(lm(estimate ~ W_meantemp, data=ABrt.phi))
summary(lm(estimate ~ W_heatdegdays, data=ABrt.phi))
summary(lm(estimate ~ W_totalprecip, data=ABrt.phi))
summary(lm(estimate ~ W_maxwind, data=ABrt.phi))

##summer weather
summary(lm(estimate ~ S_mintemp, data=ABadult.phi))
summary(lm(estimate ~ S_maxtemp, data=ABadult.phi))
summary(lm(estimate ~ S_meantemp, data=ABadult.phi))
summary(lm(estimate ~ S_heatdegdays, data=ABadult.phi))
summary(lm(estimate ~ S_totalprecip, data=ABadult.phi))
summary(lm(estimate ~ S_maxWind, data=ABadult.phi)) 

summary(lm(estimate ~ S_mintemp, data=ABmy.phi))
summary(lm(estimate ~ S_maxtemp, data=ABmy.phi))
summary(lm(estimate ~ S_meantemp, data=ABmy.phi))
summary(lm(estimate ~ S_heatdegdays, data=ABmy.phi))
summary(lm(estimate ~ S_totalprecip, data=ABmy.phi))
summary(lm(estimate ~ S_maxwind, data=ABmy.phi))  

summary(lm(estimate ~ S_mintemp, data=ABjuv.phi))
summary(lm(estimate ~ S_maxtemp, data=ABjuv.phi))
summary(lm(estimate ~ S_meantemp, data=ABjuv.phi))
summary(lm(estimate ~ S_heatdegdays, data=ABjuv.phi))
summary(lm(estimate ~ S_totalprecip, data=ABjuv.phi))
summary(lm(estimate ~ S_maxwind, data=ABjuv.phi))

summary(lm(estimate ~ S_mintemp, data=ABrt.phi))
summary(lm(estimate ~ S_maxtemp, data=ABrt.phi))
summary(lm(estimate ~ S_meantemp, data=ABrt.phi))
summary(lm(estimate ~ S_heatdegdays, data=ABrt.phi))
summary(lm(estimate ~ S_totalprecip, data=ABrt.phi))
summary(lm(estimate ~ S_maxwind, data=ABrt.phi))

###St.george
##winter weather
summary(lm(estimate ~ W_mintemp, data=SGadult.phi))
summary(lm(estimate ~ W_maxtemp, data=SGadult.phi))
summary(lm(estimate ~ W_meantemp, data=SGadult.phi))
summary(lm(estimate ~ W_heatdegdays, data=SGadult.phi))
summary(lm(estimate ~ W_totalprecip, data=SGadult.phi))
summary(lm(estimate ~ W_maxwind, data=SGadult.phi)) 

summary(lm(estimate ~ W_mintemp, data=SGmy.phi))
summary(lm(estimate ~ W_maxtemp, data=SGmy.phi))
summary(lm(estimate ~ W_meantemp, data=SGmy.phi))
summary(lm(estimate ~ W_heatdegdays, data=SGmy.phi))
summary(lm(estimate ~ W_totalprecip, data=SGmy.phi))
summary(lm(estimate ~ W_maxwind, data=SGmy.phi))  

summary(lm(estimate ~ W_mintemp, data=SGjuv.phi))
summary(lm(estimate ~ W_maxtemp, data=SGjuv.phi))
summary(lm(estimate ~ W_meantemp, data=SGjuv.phi))
summary(lm(estimate ~ W_heatdegdays, data=SGjuv.phi))
summary(lm(estimate ~ W_totalprecip, data=SGjuv.phi))
summary(lm(estimate ~ W_maxwind, data=SGjuv.phi))

summary(lm(estimate ~ W_mintemp, data=SGrt.phi))
summary(lm(estimate ~ W_maxtemp, data=SGrt.phi))
summary(lm(estimate ~ W_meantemp, data=SGrt.phi))
summary(lm(estimate ~ W_heatdegdays, data=SGrt.phi))
summary(lm(estimate ~ W_totalprecip, data=SGrt.phi))
summary(lm(estimate ~ W_maxwind, data=SGrt.phi))

##summer weather
summary(lm(estimate ~ S_mintemp, data=SGadult.phi))
summary(lm(estimate ~ S_maxtemp, data=SGadult.phi))
summary(lm(estimate ~ S_meantemp, data=SGadult.phi))
summary(lm(estimate ~ S_heatdegdays, data=SGadult.phi))
summary(lm(estimate ~ S_totalprecip, data=SGadult.phi))
summary(lm(estimate ~ S_maxwind, data=SGadult.phi)) 

summary(lm(estimate ~ S_mintemp, data=SGmy.phi))
summary(lm(estimate ~ S_maxtemp, data=SGmy.phi))
summary(lm(estimate ~ S_meantemp, data=SGmy.phi))
summary(lm(estimate ~ S_heatdegdays, data=SGmy.phi))
summary(lm(estimate ~ S_totalprecip, data=SGmy.phi))
summary(lm(estimate ~ S_maxwind, data=SGmy.phi))  

summary(lm(estimate ~ S_mintemp, data=SGjuv.phi))
summary(lm(estimate ~ S_maxtemp, data=SGjuv.phi))
summary(lm(estimate ~ S_meantemp, data=SGjuv.phi))
summary(lm(estimate ~ S_heatdegdays, data=SGjuv.phi))
summary(lm(estimate ~ S_totalprecip, data=SGjuv.phi))
summary(lm(estimate ~ S_maxwind, data=SGjuv.phi))

summary(lm(estimate ~ S_mintemp, data=SGrt.phi))
summary(lm(estimate ~ S_maxtemp, data=SGrt.phi))
summary(lm(estimate ~ S_meantemp, data=SGrt.phi))
summary(lm(estimate ~ S_heatdegdays, data=SGrt.phi))
summary(lm(estimate ~ S_totalprecip, data=SGrt.phi))
summary(lm(estimate ~ S_maxwind, data=SGrt.phi))

###Squeaky
##winter weather
summary(lm(estimate ~ W_mintemp, data=SQadult.phi))
summary(lm(estimate ~ W_maxtemp, data=SQadult.phi))
summary(lm(estimate ~ W_meantemp, data=SQadult.phi))
summary(lm(estimate ~ W_heatdegdays, data=SQadult.phi))
summary(lm(estimate ~ W_totalprecip, data=SQadult.phi)) ###significant
summary(lm(estimate ~ W_maxwind, data=SQadult.phi)) 

summary(lm(estimate ~ W_mintemp, data=SQrt.phi))
summary(lm(estimate ~ W_maxtemp, data=SQrt.phi))
summary(lm(estimate ~ W_meantemp, data=SQrt.phi))
summary(lm(estimate ~ W_heatdegdays, data=SQrt.phi))
summary(lm(estimate ~ W_totalprecip, data=SQrt.phi)) ###significant
summary(lm(estimate ~ W_maxwind, data=SQrt.phi)) 

##summer weather
summary(lm(estimate ~ S_mintemp, data=SQadult.phi))
summary(lm(estimate ~ S_maxtemp, data=SQadult.phi))
summary(lm(estimate ~ S_meantemp, data=SQadult.phi))
summary(lm(estimate ~ S_heatdegdays, data=SQadult.phi))
summary(lm(estimate ~ S_totalprecip, data=SQadult.phi))
summary(lm(estimate ~ S_maxwind, data=SQadult.phi)) 

summary(lm(estimate ~ S_mintemp, data=SQrt.phi))
summary(lm(estimate ~ S_maxtemp, data=SQrt.phi)) ###significant
summary(lm(estimate ~ S_meantemp, data=SQrt.phi))
summary(lm(estimate ~ S_heatdegdays, data=SQrt.phi)) ###significant
summary(lm(estimate ~ S_totalprecip, data=SQrt.phi)) ###significant
summary(lm(estimate ~ S_maxwind, data=SQrt.phi)) 

###visualization####
SQphi<-ggplot(SQadult.phi, aes(W_totalprecip, estimate, color=index, group=index)) + 
  geom_point(size=3, position = position_dodge(0.25))+
  xlab('Average amount of winter precipitation (mm)')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(limits = c(0, 1))
SQphi
