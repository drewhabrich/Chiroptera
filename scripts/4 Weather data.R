getwd()
setwd('E:/R/pittag project/pittag analysis')
library(ggplot2)
library(dplyr)
library(lubridate)
rm(list=ls())

#read.csv
w<-read.csv('weather summary seasons.csv')
str(w)
head(w)

w$Date=as.Date(w$Date, "%Y-%m-%d")

#subset the weather stations
gr<-w[which(w$Site=='GR'), ]
sg<-w[which(w$Site=='SG'), ]

#average by season and add to one dataframe
w_s<-aggregate(maxtemp ~ season, gr, mean) #create the dataframe with aggregate
w_s$avg_mintemp<-tapply(gr$mintemp, gr$season, mean, na.rm=T) #add to dataframe using tapply
w_s$avg_meantemp<-tapply(gr$meantemp, gr$season, mean, na.rm=T)
w_s$avg_heatdegdays<-tapply(gr$heatdegdays, gr$season, mean, na.rm=T)
w_s$avg_totalprecip<-tapply(gr$totalprecip, gr$season, mean, na.rm=T)
w_s$avg_maxwind<-tapply(gr$maxwind, gr$season, mean, na.rm=T)

w_x<-aggregate(maxtemp ~ season, sg, mean) #create the dataframe with aggregate
w_x$avg_mintemp<-tapply(sg$mintemp, sg$season, mean, na.rm=T) #add to dataframe using tapply
w_x$avg_meantemp<-tapply(sg$meantemp, sg$season, mean, na.rm=T)
w_x$avg_heatdegdays<-tapply(sg$heatdegdays, sg$season, mean, na.rm=T)
w_x$avg_totalprecip<-tapply(sg$totalprecip, sg$season, mean, na.rm=T)
w_x$avg_maxwind<-tapply(sg$maxwind, sg$season, mean, na.rm=T)

#write csv
write.csv(w_s, file = "mean weather data by season at GR.csv")
write.csv(w_x, file = "mean weather data by season at SG.csv")
