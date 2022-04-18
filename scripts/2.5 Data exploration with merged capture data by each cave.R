library(ggplot2)
library(dplyr)

getwd()

pitsummary<-read.csv("PIT for general statistics.csv")
str(pitsummary)
#summary for groups
sum(pitsummary$Fa) #3163
sum(pitsummary$Fj) #1096
sum(pitsummary$Ma) #4272
sum(pitsummary$Mj) #1366
3163+1096+4272+1366

#subset each cave
AB <- pitsummary[ which(pitsummary$AB==1), ]
DA <- pitsummary[ which(pitsummary$DA==1), ]
FC <- pitsummary[ which(pitsummary$FC==1), ]
IG <- pitsummary[ which(pitsummary$IG==1), ]
LP <- pitsummary[ which(pitsummary$LP==1), ]
MA <- pitsummary[ which(pitsummary$MA==1), ]
MW <- pitsummary[ which(pitsummary$MW==1), ]
OK <- pitsummary[ which(pitsummary$OK==1), ]
PC <- pitsummary[ which(pitsummary$PC==1), ]
RL <- pitsummary[ which(pitsummary$RL==1), ]
SG <- pitsummary[ which(pitsummary$SG==1), ]
SQ <- pitsummary[ which(pitsummary$SQ==1), ]

#summary BY cave
#AB
norecap <- pitsummary[ which(pitsummary$sumobs==1), ]
recap <- pitsummary[ which(pitsummary$sumobs>1), ]

#counts per group with recaps and captures
count(recap,group)
count(norecap,group)

#counts per cave
count(AB,group)
count(DA,group)
count(FC,group)
count(IG,group)
count(LP,group)
count(MA,group)
count(MW,group)
count(OK,group)
count(PC,group)
count(RL,group)
count(SG,group)
count(SQ,group)

##recap counts for each cave
count(recap[ which(recap$AB==1), ],group)
count(norecap[ which(norecap$AB==1), ],group)

count(recap[ which(recap$DA==1), ],group)
count(norecap[ which(norecap$DA==1), ],group)

count(recap[ which(recap$FC==1), ],group)
count(norecap[ which(norecap$FC==1), ],group)

count(recap[ which(recap$IG==1), ],group)
count(norecap[ which(norecap$IG==1), ],group)

count(recap[ which(recap$LP==1), ],group)
count(norecap[ which(norecap$LP==1), ],group)

count(recap[ which(recap$MA==1), ],group)
count(norecap[ which(norecap$MA==1), ],group)

count(recap[ which(recap$MW==1), ],group)
count(norecap[ which(norecap$MW==1), ],group)

count(recap[ which(recap$OK==1), ],group)
count(norecap[ which(norecap$OK==1), ],group)

count(recap[ which(recap$PC==1), ],group)
count(norecap[ which(norecap$PC==1), ],group)

count(recap[ which(recap$RL==1), ],group)
count(norecap[ which(norecap$RL==1), ],group)

count(recap[ which(recap$SG==1), ],group)
count(norecap[ which(norecap$SG==1), ],group)

count(recap[ which(recap$SQ==1), ],group)
count(norecap[ which(norecap$SQ==1), ],group)

#comparison of redetection rates
recap.rates<-read.csv("PIT recap rates.csv")
str(recap.rates)

#detections per year
sum(AB$X2008)
sum(AB$X2009)
sum(AB$X2010)
sum(AB$X2011)
sum(AB$X2012)
sum(AB$X2013)
sum(AB$X2014)
sum(AB$X2015)
sum(AB$X2016)
sum(AB$X2017)

sum(DA$X2008)
sum(DA$X2009)
sum(DA$X2010)
sum(DA$X2011)
sum(DA$X2012)
sum(DA$X2013)
sum(DA$X2014)
sum(DA$X2015)
sum(DA$X2016)
sum(DA$X2017)

sum(FC$X2008)
sum(FC$X2009)
sum(FC$X2010)
sum(FC$X2011)
sum(FC$X2012)
sum(FC$X2013)
sum(FC$X2014)
sum(FC$X2015)
sum(FC$X2016)
sum(FC$X2017)

sum(IG$X2008)
sum(IG$X2009)
sum(IG$X2010)
sum(IG$X2011)
sum(IG$X2012)
sum(IG$X2013)
sum(IG$X2014)
sum(IG$X2015)
sum(IG$X2016)
sum(IG$X2017)

sum(LP$X2008)
sum(LP$X2009)
sum(LP$X2010)
sum(LP$X2011)
sum(LP$X2012)
sum(LP$X2013)
sum(LP$X2014)
sum(LP$X2015)
sum(LP$X2016)
sum(LP$X2017)

sum(MA$X2008)
sum(MA$X2009)
sum(MA$X2010)
sum(MA$X2011)
sum(MA$X2012)
sum(MA$X2013)
sum(MA$X2014)
sum(MA$X2015)
sum(MA$X2016)
sum(MA$X2017)

sum(MW$X2008)
sum(MW$X2009)
sum(MW$X2010)
sum(MW$X2011)
sum(MW$X2012)
sum(MW$X2013)
sum(MW$X2014)
sum(MW$X2015)
sum(MW$X2016)
sum(MW$X2017)

sum(OK$X2008)
sum(OK$X2009)
sum(OK$X2010)
sum(OK$X2011)
sum(OK$X2012)
sum(OK$X2013)
sum(OK$X2014)
sum(OK$X2015)
sum(OK$X2016)
sum(OK$X2017)

sum(PC$X2008)
sum(PC$X2009)
sum(PC$X2010)
sum(PC$X2011)
sum(PC$X2012)
sum(PC$X2013)
sum(PC$X2014)
sum(PC$X2015)
sum(PC$X2016)
sum(PC$X2017)

sum(RL$X2008)
sum(RL$X2009)
sum(RL$X2010)
sum(RL$X2011)
sum(RL$X2012)
sum(RL$X2013)
sum(RL$X2014)
sum(RL$X2015)
sum(RL$X2016)
sum(RL$X2017)

sum(SG$X2008)
sum(SG$X2009)
sum(SG$X2010)
sum(SG$X2011)
sum(SG$X2012)
sum(SG$X2013)
sum(SG$X2014)
sum(SG$X2015)
sum(SG$X2016)
sum(SG$X2017)

sum(SQ$X2008)
sum(SQ$X2009)
sum(SQ$X2010)
sum(SQ$X2011)
sum(SQ$X2012)
sum(SQ$X2013)
sum(SQ$X2014)
sum(SQ$X2015)
sum(SQ$X2016)
sum(SQ$X2017)

#check descriptive statistics
levels(recap.rates$group)
group_by(recap.rates, group) %>%
  summarise(
    count = n(),
    mean = mean(ratio, na.rm = TRUE),
    sd = sd(ratio, na.rm = TRUE)
  )
#anova
recap.anova<-aov(recap~group, data=recap.rates)
summary(recap.anova)
