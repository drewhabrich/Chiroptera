
library(lme4)
library(MCMCglmm)


#CLASSIC COUNTER
classic.counter=function(var.to.count)
{# Quinn Fletcher
 # 21-Jan-2011
 # May work better with as.numeric() 
 # Counts var.to.count, giving consecutive numbers if var.to.count
 # is the same as the cell above, and giving "1" if var.to.count is
 # a new value (in Excel: if(var.to.count2=var.to.count1,counter1+1,1)
 #  1. Sort by what you want to count: var.to.count etc.
       #SORTS BY THE COLUMN VAR1
          # new.data=old.data[order(old.data$var1), ]
       #SORTS BY VAR1 FIRST THEN VAR2
          # new.data=old.data[order(old.data$var1, old.data$var2), ]
       #SORTS BY VAR1 FIRST THEN IN REVERSE BY VAR2
          # new.data=old.data[order(old.data$var1, -old.data$var2), ]
 # 2. Type "c=classic.counter(data.table$var.to.count)"
 # 3. Append counter to table: data.table$counter.name=c$counter}
counter=mat.or.vec(length(var.to.count),1)
counter=1
for (i in 2:length(var.to.count)) {
	counter[i]=ifelse(var.to.count[i-1]==
	var.to.count[i], counter[i-1]+1,1) }
z=list(counter=counter)
}

## convert date to julian day 
days_since_1_jan=function(date.variable) 
{# Quinn Fletcher 
 # 21-Jan-2011  
 # Calculates the days since Jan 1st for a date value where Jan 2=1, 
 # Jan 3=2 
 # 1. format the date.variable using something like 
 #    data.table$date.variable=as.Date(data.table$date.variable, 
 #        "%d/%m/ %Y") 
 # 2. Run Function = d=days_since_1_jan(data.table$date.variable) 
 # 3. Append ds_1_jan to table: data.table$jules=d$ds_fst_jan} 
year=format(date.variable, format="%Y") 
fst_jan=paste("01/01/",year) 
fst_jan=as.Date(fst_jan,"%d/%b/%Y") 
ds_fst_jan= date.variable - fst_jan 
}

pit2=read.table("pit.data.2008-2014.txt",header=T)
str(pit2)

head(pit2)

#pit2 $datetime<-do.call(paste, (pit2[,c("date", "time")]))
#pit2 $time2<-as.POSIXct(pit2 $datetime,format="%m-%d-%Y %H:%M:%S")
#str(pit2)
#pit2 $noon=c("43200") ## number of seconds in 12 hours
#pit2 $noon<-as.numeric(pit2 $noon)
#pit2 $time3= pit2 $time2 - pit2 $noon 
#str(pit2)


head(pit2,2500)

## convert to Julian days for full pit tag database
pit2$date=as.Date(pit2$date, "%Y-%m-%d")
str(pit2)
pit2$year=format(pit2$date, format="%Y") 
pit2$year=as.factor(pit2$year)
unique(pit2$year)
#year=format(pit2$date2, format="%Y") 
fst_jan=paste("01/01/",pit2$year) 
fst_jan=as.Date(fst_jan,"%d/%m/%Y") 
ds_fst_jan= pit2$date - fst_jan 
pit2$julian=as.integer(ds_fst_jan)
str(pit2)

## load data from 2015
pit.2015=read.table("pit.data.2015.txt",header=T)
str(pit.2015)

## change day to start at noon instead of mid-night
#pit.2015$datetime<-do.call(paste, (pit.2015[,c("date", "time")]))
#pit.2015$time2<-as.POSIXct(pit.2015$datetime,format="%m-%d-%Y %H:%M:%S")
#str(pit.2015)
#pit.2015$noon=c("43200") ## number of seconds in 12 hours
#pit.2015$noon<-as.numeric(pit.2015$noon)
#pit.2015$time3=pit.2015$time2 - pit.2015$noon 
#str(pit.2015)

pit.2015$date=as.Date(pit.2015$date,"%m-%d-%Y")
str(pit.2015)
pit.2015$year=format(pit.2015$date, format="%Y") 
pit.2015$year=as.factor(pit.2015$year)
unique(pit.2015$year)

#year=format(pit.2015$date2, format="%Y") 
fst_jan=paste("01/01/",pit.2015$year) 
fst_jan=as.Date(fst_jan,"%d/%m/%Y") 
ds_fst_jan= pit.2015$date - fst_jan 
pit.2015$julian=as.integer(ds_fst_jan)
str(pit.2015)

head(pit.2015)

pit2=rbind(pit2,pit.2015)
str(pit2)
unique(pit2$year)

## data cleaning: 
y0008 = subset(pit2, year=="0008")
y0001 = subset(pit2, year=="0001")
all=subset(pit2,year!="0008" & year!="0001")

y0008$year=as.factor("2014")
y0001$year=as.factor("2014")
pit2=rbind(all,y0008,y0001)
str(pit2)

## restrict dataset between day 90 (March 31) and day 166 (June 15)
pit_emerge=subset(pit2,julian >= 90 & julian <= 166)
str(pit_emerge)

## remove multiple detections of same individual at the same time:
pit_emerge2=subset(pit_emerge,!duplicated(subset(pit_emerge,select=c(PIT.tag,time,year))))
str(pit_emerge2)
head(pit_emerge2)

## remove multiple detections of same individual on the same day:
pit_emerge3=subset(pit_emerge2,!duplicated(subset(pit_emerge2,select=c(PIT.tag,julian,year))))
str(pit_emerge3)

## order by PIT tag and julian date for classic counter: 
pit_emerge3 = pit_emerge3[order(pit_emerge3 $PIT.tag, pit_emerge3 $julian), ]
head(pit_emerge3,25)


## make data frame to highlight bats that were detected >1 time. 
edu= pit_emerge3
ed=pit_emerge3
edu$PIT.tag=as.factor(edu$PIT.tag)
edu=edu[order(edu$PIT.tag), ]
c=classic.counter(edu$PIT.tag)
edu$count=c$counter
str(edu)
head(edu,10)
edu=edu[order(edu$PIT.tag, -edu$count), ]
dim(edu)
counts=edu$count
edu=subset(edu, count == 2)
head(edu,10)
str(edu)
subset(edu, PIT.tag == "00067591CA")
edu=subset(edu, select = c(PIT.tag, count))
edu$count="multiple"
names(edu)[2]="mult"
head(edu)
dim(ed)
ed=merge(ed, edu, by = "PIT.tag", all.x=T)
dim(ed)
subset(ed, PIT.tag == "00067591CA")
subset(edu, PIT.tag == "00067591CA")
ed$mult=as.factor(ed$mult)
head(ed,10)
summary(ed)
str(ed)
ed$count=counts
names(ed)[7]="julian_emerge" ## code as emergence so I remember which number represents emergence. 

ed$PIT.tag
str(ed)
## separate data to make sure all tags have "000" in front of them. 
tags1=paste("",ed$PIT.tag[1:2170],sep="")
tags2=paste("000",ed$PIT.tag[2171:2271],sep="")
tags3=paste("",ed$PIT.tag[2272:2561],sep="")
tags4=paste("000",ed$PIT.tag[2562: 2569],sep="")
tags5=c(tags1,tags2,tags3,tags4)

ed$PIT.tag2=as.factor(tags5)
str(ed)

head(ed)

pit_emergence=data.frame(ed$PIT.tag2,ed$site,ed$year,ed$julian_emerge,ed$mult,ed$time,ed$count,ed$date)
names(pit_emergence)[1]="PIT.tag"
names(pit_emergence)[2]="site"
names(pit_emergence)[3]="year"
names(pit_emergence)[4]="julian_emerge"
names(pit_emergence)[5]="multiple"
names(pit_emergence)[6]="time"
names(pit_emergence)[7]="count"
str(pit_emergence)
summary(as.factor(pit_emergence $count))

head(pit_emergence,20)
unique(pit_emergence$year)

pit_emergence $pit=as.factor(pit_emergence$PIT.tag)
str(pit_emergence)
pit_emergence = pit_emergence[order(pit_emergence $PIT.tag),]
pit_emergence$PIT.tag
pit_emergence = pit_emergence[-c(1:18,1402:1408),] ## ROWS 1:18 HAVE E+0


pit_emergence2=sqldf("
SELECT pit, year, site, count,min(julian_emerge) AS julian_emerge 
FROM pit_emergence
GROUP BY   pit, year
")

pit_emergence2$site=as.factor(pit_emergence2$site)
head(pit_emergence2,50)

unique(pit_emergence2$site)

#### write data frame for EMERGENCE PIT TAG DETECTIONS: 
write.csv(pit_emergence2,file="emergence_all.csv")


##### IMMERGENCE DATA ########
## restrict dataset between day 244 (sept 1) and day 274 (Oct 15)
pit_immerge=subset(pit2,julian >= 244 & julian <= 274)
str(pit_immerge)

pit_immerge2=subset(pit_immerge,!duplicated(subset(pit_immerge,select=c(PIT.tag,time,year))))
str(pit_immerge2)

pit_immerge3=subset(pit_immerge2,!duplicated(subset(pit_immerge2,select=c(PIT.tag,julian,year))))
str(pit_immerge3)
head(pit_immerge3)
length(unique(pit_immerge3$PIT.tag))

## make data frame to highlight bats that were detected >1 time. 
edu= pit_immerge3
ed= pit_immerge3
edu$PIT.tag=as.factor(edu$PIT.tag)
edu=edu[order(edu$PIT.tag), ]
c=classic.counter(edu$PIT.tag)
edu$count=c$counter
str(edu)
head(edu,10)
edu=edu[order(edu$PIT.tag, -edu$count), ]
dim(edu)
counts=edu$count
edu=subset(edu, count == 2)
head(edu,10)
str(edu)
subset(edu, PIT.tag == "0006967DEB")
edu=subset(edu, select = c(PIT.tag, count))
edu$count="multiple"
names(edu)[2]="mult"
head(edu)
dim(ed)
ed=merge(ed, edu, by = "PIT.tag", all.x=T)
dim(ed)
subset(ed, PIT.tag == "0006967DEB")
subset(edu, PIT.tag == "0006967DEB")
ed$mult=as.factor(ed$mult)
head(ed,10)
summary(ed)
str(ed)
ed$count=counts
names(ed)[7]="julian_immerge" ## code as emergence so I remember which number represents emergence. 


ed$PIT.tag
str(ed)
## make sure all tags have "000" in front of them. 
tags1=paste("",ed$PIT.tag[1:1850],sep="")
tags2=paste("000",ed$PIT.tag[1851:1939],sep="")
tags3=c(tags1,tags2)
str(tags3)
ed$PIT.tag2=as.factor(tags3)
str(ed)

pit_immergence=data.frame(ed$PIT.tag2,ed$site,ed$year,ed$julian_immerge,ed$mult,ed$time,ed$count)
names(pit_immergence)[1]="pit"
names(pit_immergence)[2]="site"
names(pit_immergence)[3]="year"
names(pit_immergence)[4]="julian_immerge"
names(pit_immergence)[5]="multiple"
names(pit_immergence)[6]="time"
names(pit_immergence)[7]="count"

str(pit_immergence)
head(pit_immergence)

pit_immergence= pit_immergence[-c(1851:1860,1917:1919),]
str(pit_immergence)
pit_immergence$pit

pit_immergence2=sqldf("
SELECT pit, year, site, count,max(julian_immerge) AS julian_immerge 
FROM pit_immergence
GROUP BY   pit, year
")

pit_immergence3=subset(pit_immergence2,!duplicated(subset(pit_immergence2,select=c(pit,year))))
str(pit_immergence3)

str(pit_immergence3)
head(pit_immergence3,50)
length(unique(pit_immergence3 $pit))

## WRITE CSV FOR IMMERGENCE PIT-TAG DATA: 
write.csv(pit_immergence,file="pit_immergence.csv")


pit_immergence2=merge(pit_immergence, capture,by="PIT.tag")
head(pit_immergence2,20)
pit_immergence3 =subset(pit_immergence2,!duplicated(subset(pit_immergence2,select=c(PIT.tag,time))))
head(pit_immergence3,20)
dim(pit_immergence3)

write.csv(pit_immergence3,file="pit_immergence.csv")



## merge capture data with pit-tag data... 
capture=read.table("/Users/quinnwebber/Dropbox/Manuscripts/Emergence - share/Data/Initial data/all.capture.txt",header=T)
str(capture)

capture=subset(capture,species=="MYLU")
str(capture)

## remove all NA's from PIT.tag column
capture=capture[complete.cases(capture[,3]),]
str(capture)

head(capture,50)

capture $date2=as.Date(capture $date, "%d-%b-%y")
str(capture)
capture $year=format(capture $date2, format="%Y") 
capture $year=as.factor(capture $year)
unique(capture $year)

#year=format(capture$date2, format="%Y") 
fst_jan=paste("01/01/",capture$year) 
fst_jan=as.Date(fst_jan,"%d/%m/%Y") 
ds_fst_jan= capture$date2 - fst_jan 
capture $julian=as.integer(ds_fst_jan)
str(capture)

unique(capture$year)

## CLEAN UP DATASET: 

str(capture)
unique(capture$recap)
recaps0 <-subset(capture,recap=="N")
recaps1 <- subset(capture, recap=="YP")
nrow(recaps1) #"Y" 363
recaps2 <- subset(capture, recap=="Yp")
nrow(recaps2) # "n" 40
recaps3 <- subset(capture, recap=="yp")
nrow(recaps3) # "N " 41
recaps4 <- subset(capture, recap=="YB-j")
nrow(recaps4) # "N " 92
recaps5 <- subset(capture, recap=="ybp")
nrow(recaps5) # "N " 59
recaps6 <- subset(capture, recap=="YB")
nrow(recaps6) # "N " 66
recaps7 <- subset(capture, recap=="yb")
nrow(recaps7) # "N " 31
recaps8 <- subset(capture, recap=="YBP")
nrow(recaps8) # "N " 3
recaps9 <- subset(capture, recap=="Yb-j")
nrow(recaps9) # "N " 1
recaps10 <- subset(capture, recap=="yb-lostpit")
nrow(recaps10) # "N " 6
recaps11 <- subset(capture, recap=="yb-2ndpit")
nrow(recaps11) # "N " 1
recaps12 <- subset(capture, recap=="1rec")
nrow(recaps12) # "N " 63
recaps13 <- subset(capture, recap=="1strecord")
nrow(recaps13) # "N " 7

recaps0$recap <- "N"
recaps1$recap <- "Y"
recaps2 $recap <- "Y"
recaps3 $recap <- "Y"
recaps4 $recap <- "Y"
recaps5 $recap <- "Y"
recaps6 $recap <- "Y"
recaps7 $recap <- "Y"
recaps8 $recap <- "Y"
recaps9 $recap <- "Y"
recaps10 $recap <- "Y"
recaps11 $recap <- "Y"
recaps12 $recap <- "Y"
recaps13 $recap <- "Y"

capture2 <- rbind(recaps0,recaps1,recaps2,recaps3,recaps4,recaps5,recaps6,recaps7,recaps8,recaps9, recaps10, recaps11, recaps12, recaps13)

capture2$recap <- as.factor(capture2 $recap)
str(capture2)

## subset out maternity colonies and only keep frequently visited caves (dales, abyss, firecamp, st. george, porcupine, squeaky, richard lake & little pig)

capture3=subset(capture2, site=="AB" | site=="SG" | site =="PC" | site=="SQ"| site=="RL" )
str(capture3)

J=subset(capture3,age=="J")
str(J)



capture3=subset(capture3,age=="A")
str(capture3)

capture3=capture3[order(capture3$PIT.tag),]
capture3$PIT.tag

capture4=capture3[-c(6147:6161,6176:6177,6227:6249),] ## ditch PIT tags with E's
str(capture4)

capture4$PIT.tag
cap1=paste("", capture4$PIT.tag[1: 6146],sep="")
cap2=paste("000", capture4$PIT.tag[6147: 6326],sep="")
cap3=c(cap1, cap2)
str(cap3)
capture4$PIT.tag=as.factor(cap3)
str(capture4)

write.csv(capture4,file="capture_clean.csv") ## write clean capture data file

## capture during fall: 
capture_fall=subset(capture4,julian >= 244 & julian <= 274) ## sept 1 to sept 30
str(capture_fall)


## remove captures from same day: 
capture_fall$pit=capture_fall$PIT.tag
capture_fall2=sqldf("
SELECT pit, year, site,mass, recap,sex, faavg, max(julian) AS julian_immerge 
FROM capture_fall
GROUP BY   pit, year
")
str(capture_fall2)
capture_fall2$site=as.factor(capture_fall2$site)
capture_fall2$sex=as.factor(capture_fall2$sex)

## WRITE FILE FOR AUTUMN CAPTURE
write.csv(capture_fall2,file="capture_swarm.csv")


capture_spring=subset(capture4,julian >=91 & julian <=166) ## april 1 to june 15
## remove captures from same day: 
capture_spring$pit= capture_spring $PIT.tag
str(capture_spring)
capture_spring2=sqldf("
SELECT pit, year, site,mass, recap,sex, faavg, max(julian) AS julian_emerge 
FROM capture_spring
GROUP BY   pit, year
")
capture_spring2 $site=as.factor(capture_spring2 $site)
capture_spring2 $sex=as.factor(capture_spring2 $sex)
str(capture_spring2)
## WRITE FILE FOR SPRING CAPTURE
write.csv(capture_spring2,file="capture_spring.csv")


capture_fall=data.frame(capture$sex,capture$PIT.tag,capture$mass,capture$year,capture$recap,capture$site)
names(capture_fall)[1]="sex"
names(capture_fall)[2]="PIT.tag"
names(capture_fall)[3]="mass"
names(capture_fall)[4]="year"
names(capture_fall)[5]="recap"
names(capture_fall)[6]="site_capt"

## remove all recaps
unique(capture_fall $recap)

capture_fall=subset(capture_fall,recap=="N")
str(capture_fall)

pit_emergence2=merge(pit_emergence, capture_fall,by="PIT.tag")
str(pit_emergence2)
names(pit_emergence2)[3]="year_emerge"
names(pit_emergence2)[10]="year_capture"

pit_immergence2=merge(pit_immergence,capture_fall,by="PIT.tag")
str(pit_immergence2)
names(pit_immergence2)[3]="year_immerge"
names(pit_immergence2)[10]="year_capture"

write.csv(pit_immergence2,file="immergence_all.csv")


unique(pit_immergence2$site)
## latitudes: SG = 51.5, AB = 53.7; SQ = 53.65; FC = 53.49; RL = 49.8;   PC = 48.3

AB=subset(pit_emergence2,site=="AB")
AB$lat=as.numeric(53.7)
DA=subset(pit_emergence2,site=="DA")
DA$lat=as.numeric(53.49)
SQ=subset(pit_emergence2,site=="SQ")
SQ$lat=as.numeric(53.65)
FC=subset(pit_emergence2,site=="FC")
FC$lat=as.numeric(53.49)
RL=subset(pit_emergence2,site=="RL")
RL$lat=as.numeric(49.8)
SG=subset(pit_emergence2,site=="SG")
SG$lat=as.numeric(51.5)
MW=subset(pit_emergence2,site=="MW")
MW$lat=as.numeric(53.49)
PC=subset(pit_emergence2,site=="PC")
PC$lat=as.numeric(48.3)

pit_emergence2=rbind(AB,DA,SQ,FC,MW,RL,SG,PC)
str(pit_emergence2)

pit_M=subset(pit_emergence2,sex=="M")
pit_F=subset(pit_emergence2,sex=="F")

plot(mass~julian_emerge,data= pit_M)
plot(mass~julian_emerge,data= pit_F)


# write csv out to check for:
## 1) bats moving between sites (removed 2 records.. that showed bat moving between RL & SG - likely human error, one record was M and other was F)
## 2) change julian days to start at noon so that bats emerging before mid-night are counted on the same say as bats emerging after mid-night. 


## If you want to merge hand capture and detected animals 
capture_spring=subset(capture,julian > 90 & julian < 166)
str(capture_spring)
capture_spring=subset(capture_spring,year == "2013" | year =="2014")
unique(capture_spring$site)
capture_spring=subset(capture_spring,site =="DA" | site=="AB" | site=="FC" | site=="SG" | site =="PC" | site=="SQ" )

capture_spring2=data.frame(capture_spring$PIT.tag, capture_spring$site, capture_spring$year, capture_spring$julian, capture_spring$sex,capture_spring$mass)
names(capture_spring2)[1]="PIT.tag"
names(capture_spring2)[2]="site"
names(capture_spring2)[3]="year_emerge"
names(capture_spring2)[4]="julian_emerge2"
names(capture_spring2)[5]="sex"
names(capture_spring2)[6]="mass"

str(capture_spring2)

capture_spring2 $capt=as.factor("hand")

spring_mass=merge(capture_spring,emergence3)
write.csv(capture_spring2,file="spring")


unique(capture_spring$site)

#write.csv(pit_emergence2,file=="pit")
pit_emergence3=read.table("/Users/quinnwebber/Dropbox/Manuscripts/Emergence - share/Data/pit_emergence2.txt",header=T)
str(pit_emergence3)

## If you want to merge hand capture and detected animals 
pit_emergence3$year_emerge=as.factor(pit_emergence3$year_emerge)
pit_emergence4=data.frame(pit_emergence3 $PIT.tag, pit_emergence3 $site, pit_emergence3 $year_emerge, pit_emergence3 $julian_emerge2, pit_emergence3 $sex)
names(pit_emergence4)[1]="PIT.tag"
names(pit_emergence4)[2]="site"
names(pit_emergence4)[3]="year_emerge"
names(pit_emergence4)[4]="julian_emerge2"
names(pit_emergence4)[5]="sex"
str(pit_emergence4)
head(pit_emergence4,40)
pit_emergence4$capt=as.factor("detect")

emerge=rbind(pit_emergence4, capture_spring2)
str(emerge)
emerge = emerge[order(emerge $PIT.tag),]
head(emerge,30)

emerge2=subset(emerge,!duplicated(subset(emerge,select=c(PIT.tag,year_emerge))))
str(emerge2)
head(emerge2,50)

edu= emerge2
ed= emerge2
edu$PIT.tag=as.factor(edu$PIT.tag)
edu=edu[order(edu$PIT.tag), ]
c=classic.counter(edu$PIT.tag)
edu$count=c$counter
str(edu)
head(edu,10)
edu=edu[order(edu$PIT.tag, -edu$count), ]
dim(edu)
counts=edu$count
edu=subset(edu, count == 2)
head(edu,10)
str(edu)
subset(edu, PIT.tag == "0006966C8B")
edu=subset(edu, select = c(PIT.tag, count))
edu$count="multiple"
names(edu)[2]="mult"
head(edu)
dim(ed)
ed=merge(ed, edu, by = "PIT.tag", all.x=T)
dim(ed)
subset(ed, PIT.tag == "0006966C8B")
subset(edu, PIT.tag == "0006966C8B")
ed$mult=as.factor(ed$mult)
head(ed,10)
summary(ed)
str(ed)
ed$count=counts
names(ed)[7]="julian_immerge" ## code as emergence so I remember which number represents emergence. 


emerge3=ed
str(emerge3)
write.csv(emerge3,file="emergence_all")

str(emerge2)
unique(emerge2$year_emerge)
unique(emerge2$site)

emerge3=subset(emerge2,site !="DA" & site !="MW" & site !="FC" & year_emerge !="2009")
str(emerge3)

emerge_F=subset(emerge3,sex=="F")
str(emerge_F)

emerge_M=subset(emerge3,sex=="M")
str(emerge_M)

immerge_F=subset(pit_immergence2,sex=="F")
str(immerge_F)
boxplot(julian_immerge~site,immerge_F)

immerge_M=subset(pit_immergence2,sex=="M")
str(immerge_M)
boxplot(julian_immerge~site,immerge_M)


### emergence date vs. emerge mass
plot(julian_emerge2~mass, emerge_F)
plot(julian_emerge2~mass, emerge_M)

par(mfrow=c(1,2))
plot(mass~julian_immerge,immerge_F)
plot(mass~julian_immerge,immerge_M)

#emerge_person=merge(pit_emergence3,personality,by="PIT.tag")
#str(emerge_person)

#emerge_personF=subset(emerge_person,sex=="F")
#emerge_personM=subset(emerge_person,sex=="M")


#plot(julian_emerege2~pca1,data=emerge_personF)
#abline(lm(julian_emerege2~pca1,data=emerge_personF))
#m1=lm(julian_emerege2~pca1,data=emerge_personF)
#summary(m1)
#plot(julian_emerege2~pca2,data=emerge_personM)
#abline(lm(julian_emerege2~pca2,data=emerge_personM))


multiple_emerge =subset(pit_emergence3,multiple=="multiple")
str(multiple_emerge)


multiple_emerge_F=subset(multiple_emerge,sex=="F")
str(multiple_emerge_F)
multiple_emerge_F2= multiple_emerge_F[!duplicated(multiple_emerge_F $time), ]
head(multiple_emerge_F2,25)
str(multiple_emerge_F2)

multiple_emerge_M=subset(multiple_emerge,sex=="M")
str(multiple_emerge_M)
multiple_emerge_M2= multiple_emerge_M[!duplicated(multiple_emerge_M $time), ]
head(multiple_emerge_M2,25)
str(multiple_emerge_M2)

multiple_immerge=subset(pit_immergence2,multiple=="multiple")
str(multiple_immerge)
multiple_immerge_F=subset(multiple_immerge,sex=="F")
str(multiple_immerge_F)
multiple_immerge_F2= multiple_immerge_F[!duplicated(multiple_immerge_F $time), ]
head(multiple_immerge_F2,25)
str(multiple_immerge_F2)

multiple_immerge_M=subset(multiple_immerge,sex=="M")
str(multiple_immerge_M)
multiple_immerge_M2= multiple_immerge_M[!duplicated(multiple_immerge_M $time), ]
head(multiple_immerge_M2,25)
str(multiple_immerge_M2)

## distribution of emergece/immergence for all bats: 
par(mfrow=c(1,2))
hist(emerge_F 	$julian_emerge2,col="darkgrey",las=1,main="Females",ylim=c(0,500),xlab="Emergence Date")
abline(v=median(emerge_F $julian_emerge2),col="red",lwd=2)
hist(emerge_M $julian_emerge2,col="darkgrey",las=1,main="Males",ylim=c(0,700),xlab="Emergence Date")
abline(v=median(emerge_M $julian_emerge2),col="red",lwd=2)

hist(immerge_F $julian_immerge,col="darkgrey",las=1,main="Females",ylim=c(0,200),xlab="Immergence Date")
abline(v=median(immerge_F $julian_immerge),col="red",lwd=2)
hist(immerge_M $julian_immerge,col="darkgrey",las=1,main="Males",ylim=c(0,200),xlab="Immergence Date")
abline(v=median(immerge_M $julian_immerge),col="red",lwd=2)


## distribution of emergence for bats with >2 detections
par(mfrow=c(2,2))
hist(multiple_emerge_F2 $julian_emerge2,col="darkgrey",las=1,main="Females",ylim=c(0,50),xlab="Emergence Date")
abline(v=median(multiple_emerge_F2 $julian_emerge2),col="red",lwd=2)
hist(multiple_emerge_M2 $julian_emerge2,col="darkgrey",las=1,main="Males",ylim=c(0,200),xlab="Emergence Date")
abline(v=median(multiple_emerge_M2 $julian_emerge2),col="red",lwd=2)

hist(multiple_immerge_F2 $julian_immerge,col="darkgrey",las=1,main="",ylim=c(0,50),xlab="Immergence Date")
abline(v=median(multiple_immerge_F2 $julian_immerge),col="red",lwd=2)
hist(multiple_immerge_M2 $julian_immerge,col="darkgrey",las=1,main="",ylim=c(0,100),xlab="Immergence Date")
abline(v=median(multiple_immerge_M2 $julian_immerge),col="red",lwd=2)


str(multiple_emerge_F2)
for(i in 1:195){
	multiple_emerge_F2 $julian_emerge2_std[i]=(multiple_emerge_F2 $julian_emerge2[i] - mean(multiple_emerge_F2 $julian_emerge2,na.rm=T))/(sd(multiple_emerge_F2 $julian_emerge2,na.rm=T))
}
str(multiple_emerge_M2)
for(i in 1:437){
	multiple_emerge_M2 $julian_emerge2_std[i]=(multiple_emerge_M2 $julian_emerge2[i] - mean(multiple_emerge_F2 $julian_emerge2,na.rm=T))/(sd(multiple_emerge_M2 $multiple_emerge_M2,na.rm=T))
}


## REPEATABILITY OF EMERGENCE FOR FEMALES
repfunc<-function(i){VarCorr(i)$PIT.tag[1]/(VarCorr(i)$PIT.tag[1]+(attr(VarCorr(i),"sc")^2))} #for models with only ID as a random effect where i is the model name#

## year and site
str(emerge_F)
F4=lmer(julian_emerge~ as.factor(year)+site+(1|PIT.tag),data= emerge_F)
summary(F4)
repfunc(F4)
Vcov <- vcov(F4, useScale = FALSE)
betas <- fixef(F4)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)
par(mfrow=c(2,2))
boxplot(julian_emerge2 ~ as.integer (year_emerge),data=emerge_F,xaxt="n")
axis(side=1,at=1,"2012")
axis(side=1,at=2,"2013")
axis(side=1,at=3,"2014")
axis(side=1,at=4,"2015")
boxplot(julian_emerge2~as.integer(site),data=emerge_F,xaxt="n")
axis(side=1,at=1,"AB")
axis(side=1,at=2,"PC")
axis(side=1,at=3,"RL")
axis(side=1,at=4,"SG")
axis(side=1,at=5,"SQ")

## LME's for repeatability: males - account for year and site 
M4=lmer(julian_emerge~ as.factor(year)+site+(1|PIT.tag),data= emerge_M)
summary(M4)
repfunc(M4)
Vcov <- vcov(M4, useScale = FALSE)
betas <- fixef(M4)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
cbind(betas, se, zval, pval)
boxplot(julian_emerge2 ~as.integer(year_emerge),data=emerge_M,xaxt="n")
axis(side=1,at=1,"2012")
axis(side=1,at=2,"2013")
axis(side=1,at=3,"2014")
axis(side=1,at=4,"2015")
boxplot(julian_emerge2~as.integer(site),data=emerge_M,xaxt="n")
axis(side=1,at=1,"AB")
axis(side=1,at=2,"PC")
axis(side=1,at=3,"RL")
axis(side=1,at=4,"SG")
axis(side=1,at=5,"SQ")


### IMMERGENCE  REPEATABILITY
F4_im=lmer(julian_immerge~ as.factor(year_immerge)+site +(1|PIT.tag),data= immerge_F)
summary(F4_im)
repfunc(F4_im)
boxplot(julian_immerge ~ year_immerge,data=immerge_F)

M4_im=lmer(julian_immerge~ as.factor(year_immerge)+site+(1|PIT.tag),data=immerge_M)
summary(M4_im)
repfunc(M4_im)
boxplot(julian_immerge ~ year_immerge,data=immerge_M)


### EXTRACT SWARMING DATA ONLY - WANT TO GET MASS DURING AUTUMN
## use initial capture data set previously imported/ 
capture_swarm=subset(capture,julian >= 244 & julian <= 274)
str(capture_swarm)
capture_swarm2=data.frame(capture_swarm $site, capture_swarm $PIT.tag, capture_swarm $sex, capture_swarm $mass, capture_swarm $faavg, capture_swarm $year, capture_swarm $julian)
names(capture_swarm2)[1]="site"
names(capture_swarm2)[2]="PIT.tag"
names(capture_swarm2)[3]="sex"
names(capture_swarm2)[4]="mass"
names(capture_swarm2)[5]="faavg"
names(capture_swarm2)[6]="year"
names(capture_swarm2)[7]="julian_mass"
str(capture_swarm2)
write.csv(capture_swarm2,file="capture_swarm.csv")

pit_emerge2=subset(pit_emerge,!duplicated(subset(pit_emerge,select=c(PIT.tag,time))))
str(pit_emerge2)

pit_emerge3=subset(pit_emerge2,!duplicated(subset(pit_emerge2,select=c(PIT.tag,year))))
str(pit_emerge3)


#personality=read.table("/Users/quinnwebber/Dropbox/Manuscripts/Personality - emerge share/Data/personality_list.txt",header=T)
#str(personality)

## run PCA for personality traits
#mod_person=prcomp(personality[2:10],scale=TRUE,centre=TRUE)
#summary(modelperson)

## summary gives sd, prop var for each principal component 

#personality$pca1= mod_person $x[,1]
#personality$pca2= mod_person $x[,2]
#personality$pca3= mod_person $x[,3]

#plot(pca1~locomoton,personality)
#plot(pca2~hd,personality)
#plot(pca3~groom,personality)

#personality$pca1=personality$pca1*(-1)

#person_swarm=merge(personality,capture_swarm2,by="PIT.tag")
#str(person_swarm)
#unique(person_swarm$PIT.tag)

#plot(mass~pca1,person_swarm,pch=20,ylim=c(6,16))
#abline(lm(mass~pca1,person_swarm,na.action=na.omit))

#m1=lm(mass~pca1,person_swarm,na.action=na.omit)
#summary(m1)

## Get Mass for bats emerging from hiernation: April 1 to June  9
capture_emerge=subset(capture,julian >= 91 & julian <= 160)
str(capture_emerge)
emerge_only =data.frame(capture_emerge $site, capture_emerge $PIT.tag, capture_emerge $sex, capture_emerge $mass, capture_emerge $faavg, capture_emerge $year, capture_emerge $julian)
names(emerge_only)[1]="site"
names(emerge_only)[2]="PIT.tag"
names(emerge_only)[3]="sex"
names(emerge_only)[4]="mass"
names(emerge_only)[5]="faavg"
names(emerge_only)[6]="year"
names(emerge_only)[7]="julian_emerge"
str(emerge_only)

head(emerge_only)
head(capture_swarm2)

## merge fall capture data with spring emergence data to compare mass between fall and spring - 
mass_compare=merge(emerge_only, capture_swarm2,by="PIT.tag")
str(mass_compare)
head(mass_compare,15)
## rename variables
names(mass_compare)[1]="PIT.tag"
names(mass_compare)[2]="site_emerge"
names(mass_compare)[3]="sex_emerge"
names(mass_compare)[4]="mass_emerge"
names(mass_compare)[5]="faavg_emerge"
names(mass_compare)[6]="year_emerge"
names(mass_compare)[7]="julian_emerge"
names(mass_compare)[8]="site_immerge"
names(mass_compare)[9]="sex_immerge"
names(mass_compare)[10]="mass_immerge"
names(mass_compare)[11]="faavg_immerge"
names(mass_compare)[12]="year_immerge"
names(mass_compare)[13]="julian_immerge"


unique(mass_compare $year_immerge)
unique(mass_compare $year_emerge)
## year_immerge = 2008, 2009, 2010, 2011, 2012, 2013, 2014
## year_emerge = 2009, 2012, 2013, 2014, 2015
em1=subset(mass_compare, year_immerge =="2008" & year_emerge =="2009")
em2=subset(mass_compare, year_immerge =="2011" & year_emerge =="2012")
em3=subset(mass_compare, year_immerge =="2012" & year_emerge =="2013")
em4=subset(mass_compare, year_immerge =="2013" & year_emerge =="2014")
em5=subset(mass_compare, year_immerge =="2014" & year_emerge =="2015")

mass_compare2=rbind(em1,em2,em3,em4,em5)
str(mass_compare2)
head(mass_compare2,15)

### USING SEPT 15 AS IMMERGENCE DATE FOR ALL BATS
mass_compare2$rate1=(mass_compare2$mass_immerge-mass_compare2 $mass_emerge)/(107+ mass_compare2$julian_emerge)
hist(mass_compare2 $rate)

### USE 1 DAY AFTER DAY OF CAPTURE AS IMMERGENCE DATE
mass_compare2$rate2=(mass_compare2$mass_immerge-mass_compare2 $mass_emerge)/((365-mass_compare2$julian_immerge+1)+ mass_compare2$julian_emerge)
hist(mass_compare2 $rate2)
str(mass_compare)
mass_compare2$duration=(365-mass_compare2$julian_immerge) + (mass_compare2 $julian_emerge)
hist(mass_compare2 $duration)

mass_compare2$massloss=mass_compare2$mass_immerge-mass_compare2 $mass_emerge

mass_compare_M=subset(mass_compare2,sex_emerge=="M")
str(mass_compare_M)
mass_compare_F=subset(mass_compare2, sex_emerge =="F")
str(mass_compare_F)

plot(rate1~rate2,mass_compare2)

str(mass_compare_F)
plot(julian_emerge~julian_immerge,data= mass_compare_F)
a1=lm(julian_emerge~julian_immerge+site_emerge,data= mass_compare_F)
summary(a1)
boxplot(julian_emerge~site_emerge,data= mass_compare_F)

plot(julian_immerge~julian_emerge,data= mass_compare_M)

## compare autumn and spring mass: n = 27 for males; n = 29 for females 
par(mfrow=c(1,2))
plot(mass_immerge~mass_emerge, mass_compare_M,las=1,ylim=c(8,16),xlim=c(6,11),ylab="Autumn Mass",xlab="Emergence Mass",pch=20,main="Males")
abline(lm(mass_immerge~mass_emerge, mass_compare_M))
plot(mass_immerge ~mass_emerge, mass_compare_F,las=1,ylim=c(8,16),xlim=c(6,11),ylab="Autumn Mass",xlab="Emergence Mass",pch=20,main="Females")
abline(lm(mass_immerge~mass_emerge, mass_compare_F))


str(mass_compare_M)
## compare rate of mass loss against emergence phenology
par(mfrow=c(1,2))
plot(rate2~ mass_immerge,data= mass_compare_F,ylim=c(0,0.03),xlim=c(7,16),pch=16,xlab="Immergence Mass",ylab="Rate of Mass loss (g/day)",main="Females")
abline(lm(rate2~ mass_immerge,data= mass_compare_F))
a1=lm(rate2~ mass_immerge,data= mass_compare_F,na.action=na.omit)
summary(a1)
plot(rate2~ mass_immerge,data= mass_compare_M,ylim=c(0,0.03),xlim=c(7,16),pch=16,main="Males",xlab="Immergence Mass",ylab="Rate of Mass loss (g/day)")
abline(lm(rate2~ mass_immerge,data=mass_compare_M))
a2=lm(rate2~ mass_immerge,data=mass_compare_M)
summary(a2)


## compare autumn mass and spring emergence
fallmass_emergedate=merge(pit_emergence, capture_swarm2,by="PIT.tag")
str(fallmass_emergedate)
names(fallmass_emergedate)[1]="PIT.tag"
names(fallmass_emergedate)[2]="site_emerge"
names(fallmass_emergedate)[3]="year_emerge"
names(fallmass_emergedate)[4]="julian_emerge"
names(fallmass_emergedate)[5]="multiple"
names(fallmass_emergedate)[6]="time_emerge"
names(fallmass_emergedate)[7]="count"
names(fallmass_emergedate)[8]="site_immerge"
names(fallmass_emergedate)[9]="sex"
names(fallmass_emergedate)[10]="mass_fall"
names(fallmass_emergedate)[11]="faavg_immerge"
names(fallmass_emergedate)[12]="year_immerge"
names(fallmass_emergedate)[13]="julian_immerge"


## subset into fall year and subsequent spring year
unique(fallmass_emergedate $year_immerge)
unique(fallmass_emergedate $year_emerge)
## year_immerge = 2008, 2009, 2010, 2011, 2012, 2013, 2014
## year_emerge = 2009, 2012, 2013, 2014, 2015
em1=subset(fallmass_emergedate, year_immerge =="2008" & year_emerge =="2009")
em2=subset(fallmass_emergedate, year_immerge =="2011" & year_emerge =="2012")
em3=subset(fallmass_emergedate, year_immerge =="2012" & year_emerge =="2013")
em4=subset(fallmass_emergedate, year_immerge =="2013" & year_emerge =="2014")
em5=subset(fallmass_emergedate, year_immerge =="2014" & year_emerge =="2015")

fallmass_emerge2=rbind(em1,em2,em3,em4,em5)
str(fallmass_emerge2)
head(fallmass_emerge2,10)

fallmass_emerge_F=subset(fallmass_emerge2,sex=="F")
str(fallmass_emerge_F)
fallmass_emerge_M=subset(fallmass_emerge2,sex=="M")
str(fallmass_emerge_M)

a1=lm(julian_emerge~mass_fall,data=fallmass_emerge_F,na.action=na.omit)
summary(a1)

a2=lm(julian_emerge~mass_fall,data=fallmass_emerge_M,na.action=na.omit)
summary(a2)

## autumn mass as a predictor of spring emergence. 
par(mfrow=c(1,2))
plot(julian_emerge~mass_fall, fallmass_emerge_F,las=1,xlim=c(6,16),ylim=c(90,170),ylab="Emergence Date",xlab="Autumn Mass",pch=20,main="Females")
abline(lm(julian_emerge ~mass_fall,fallmass_emerge_F,na.action=na.omit))
plot(julian_emerge ~mass_fall, fallmass_emerge_M,las=1,xlim=c(6,16),ylim=c(90,170),ylab="Emergence Date",xlab="Autumn Mass",pch=20,main="Males")
abline(lm(julian_emerge ~mass_fall, fallmass_emerge_M))


####### compare autumn mass to spring emergence and fall immergence ##########
fallmass_immergedate=merge(pit_immergence, capture_swarm2,by="PIT.tag")
str(fallmass_immergedate)
names(fallmass_immergedate)[1]="PIT.tag"
names(fallmass_immergedate)[2]="site_immerge"
names(fallmass_immergedate)[3]="year_immerge"
names(fallmass_immergedate)[4]="julian_immerge"
names(fallmass_immergedate)[5]="multiple"
names(fallmass_immergedate)[6]="time_immerge"
names(fallmass_immergedate)[7]="count"
names(fallmass_immergedate)[8]="site_capt"
names(fallmass_immergedate)[9]="sex"
names(fallmass_immergedate)[10]="mass_fall"
names(fallmass_immergedate)[11]="faavg_capt"
names(fallmass_immergedate)[12]="year_capt"
names(fallmass_immergedate)[13]="julian_capt"


## subset into fall year and subsequent spring year
unique(fallmass_immergedate $year_immerge)
unique(fallmass_immergedate $year_capt)
## year_immerge = 2011, 2012, 2013, 2014
## year_emerge = 2008, 2010, 2011, 2012, 2013,2014
imm1=subset(fallmass_immergedate, year_immerge =="2011" & year_capt =="2011")
imm2=subset(fallmass_immergedate, year_immerge =="2012" & year_capt =="2012")
imm3=subset(fallmass_immergedate, year_immerge =="2013" & year_capt =="2013")
imm4=subset(fallmass_immergedate, year_immerge =="2014" & year_capt =="2014")

fallmass_immerge2=rbind(imm1,imm2,imm3,imm4)
str(fallmass_immerge2)
head(fallmass_immerge2,25)

fallmass_immerge2$diff= fallmass_immerge2$julian_immerge-fallmass_immerge2$julian_capt
hist(fallmass_immerge2$diff)
fallmass_immerge3=subset(fallmass_immerge2,diff >= 0)

fallmass_immerge_F=subset(fallmass_immerge3,sex=="F")
str(fallmass_immerge_F)
fallmass_immerge_M=subset(fallmass_immerge3,sex=="M")
str(fallmass_immerge_M)

par(mfrow=c(2,2))
plot(julian_emerge~mass_fall, fallmass_emerge_F,las=1,xlim=c(6,16),ylim=c(90,180),ylab="Emergence Date",xlab="Autumn Body Mass (g)",pch=20,main="Females")
abline(lm(julian_emerge ~mass_fall,fallmass_emerge_F,na.action=na.omit))
text(7.5,175,"p = 0.0002")

plot(julian_emerge ~mass_fall, fallmass_emerge_M,las=1,xlim=c(6,16),ylim=c(90,180),ylab="Emergence Date",xlab="Autumn Body Mass (g)",pch=20,main="Males")
#abline(lm(julian_emerge ~mass_fall, fallmass_emerge_M))
text(7,175,"p = 0.16")

plot(julian_immerge~mass_fall,data= fallmass_immerge_F,las=1,ylim=c(240,275),xlim=c(6,16),ylab="Immergence Date",xlab="Autumn Body Mass (g)",pch=20,main="Females")
#abline(lm(julian_immerge~mass_fall,data= fallmass_immerge_F,na.action=na.omit))
text(7,272.5,"p = 0.90")

plot(julian_immerge~mass_fall,data= fallmass_immerge_M,las=1,ylim=c(240,275),xlim=c(6,16),ylab="Immergence Date",xlab="Autumn Body Mass (g)",pch=20,main="Males")
abline(lm(julian_immerge~mass_fall,data= fallmass_immerge_M,na.action=na.omit))
text(7.5,272.5,"p = 0.0001")

str(fallmass_immerge_F)
a1=lm(julian_immerge~mass_fall+year_capt,data= fallmass_immerge_F,na.action=na.omit)
summary(a1)
a2=lm(julian_immerge~mass_fall+ year_capt,data= fallmass_immerge_M,na.action=na.omit)
summary(a2)

## figure out rate of mass decrease over winter: 
### immergence data: 
pit_immerge=subset(pit2,julian <= 274 & julian >= 244)
str(pit_immerge)

pit_immerge$days_dec31=365-pit_immerge$julian ## days between Dec 31 and immergence

## merge pit tag immergence and emmergence data: 
str(pit_immerge)
str(pit_emergence2)

fall_mass_im=merge(capture_swarm2,pit_immerge,by="PIT.tag")
str(fall_mass_im)

fall_mass_im2=subset(fall_mass_im,!duplicated(subset(fall_mass_im,select=c(PIT.tag,time))))
str(fall_mass_im2)

fall_mass_im3=subset(fall_mass_im,!duplicated(subset(fall_mass_im,select=c(PIT.tag,year.x))))
str(fall_mass_im3)

im_F=subset(fall_mass_im3,sex=="F")
im_M=subset(fall_mass_im3,sex=="M")

fall_mass_im=data.frame(fall_mass_im$PIT.tag, fall_mass_im$site.x, fall_mass_im$sex, fall_mass_im$mass, fall_mass_im$julian, fall_mass_im$days_dec31, fall_mass_im$year.y)
names(fall_mass_im)[1]="PIT.tag"
names(fall_mass_im)[2]="site"
names(fall_mass_im)[3]="sex"
names(fall_mass_im)[4]="fall_mass"
names(fall_mass_im)[5]="julian_immerge"
names(fall_mass_im)[6]="days_dec31"
names(fall_mass_im)[7]="year"


pit_emerge2=subset(pit_emerge,!duplicated(subset(pit_emerge,select=c(PIT.tag,time))))
str(pit_emerge2)

pit_emerge3=subset(pit_emerge2,!duplicated(subset(pit_emerge2,select=c(PIT.tag,year))))
str(pit_emerge3)


str(fall_mass_im)



pit_im_em=merge(fall_mass_im, pit_emergence2,by="PIT.tag")
str(pit_im_em)

pit_im_em2=data.frame(pit_im_em$PIT.tag, pit_im_em$site.x, pit_im_em$sex.x, pit_im_em$fall_mass,pit_im_em$julian_immerge, pit_im_em$days_dec31, pit_im_em$year, pit_im_em$julian_emerge, pit_im_em$year_capture)
str(pit_im_em2)
names(pit_im_em2)[1]="PIT.tag"
names(pit_im_em2)[2]="site"
names(pit_im_em2)[3]="sex"
names(pit_im_em2)[4]="fall_mass"
names(pit_im_em2)[5]="julian_immerge"
names(pit_im_em2)[6]="days_dec31"
names(pit_im_em2)[7]="year_immerge"
names(pit_im_em2)[8]="julian_emerge"
names(pit_im_em2)[9]="year_emerge"
str(pit_im_em2)
head(pit_im_em2)

## subset into fall year and subsequent spring year
unique(pit_im_em2 $year_immerge)
unique(pit_im_em2 $year_emerge)
## year_immerge = 2011, 2012, 2013, 2014
## year_emerge = 2008, 2009, 2012, 2013, 2014, 2015
em1=subset(pit_im_em2, year_immerge =="2011" & year_emerge =="2012")
em2=subset(pit_im_em2, year_immerge =="2013" & year_emerge =="2014")
em3=subset(pit_im_em2, year_immerge =="2012" & year_emerge =="2013")
em4=subset(pit_im_em2, year_immerge =="2013" & year_emerge =="2014")
em5=subset(pit_im_em2, year_immerge =="2014" & year_emerge =="2015")

a=rbind(em1,em2,em3,em4,em5)

## recapture data: figure out how many bats there are multiple years of mass measurements
str(capture)
unique(capture$recap)
recaps1 <- subset(capture, recap=="YP")
nrow(recaps1) #"Y" 363
recaps2 <- subset(capture, recap=="Yp")
nrow(recaps2) # "n" 40
recaps3 <- subset(capture, recap=="yp")
nrow(recaps3) # "N " 41
recaps4 <- subset(capture, recap=="YB-j")
nrow(recaps4) # "N " 92
recaps5 <- subset(capture, recap=="ybp")
nrow(recaps5) # "N " 59
recaps6 <- subset(capture, recap=="YB")
nrow(recaps6) # "N " 66
recaps7 <- subset(capture, recap=="yb")
nrow(recaps7) # "N " 31
recaps8 <- subset(capture, recap=="YBP")
nrow(recaps8) # "N " 3
recaps9 <- subset(capture, recap=="Yb-j")
nrow(recaps9) # "N " 1
recaps10 <- subset(capture, recap=="yb-lostpit")
nrow(recaps10) # "N " 6
recaps11 <- subset(capture, recap=="yb-2ndpit")
nrow(recaps11) # "N " 1
recaps12 <- subset(capture, recap=="1rec")
nrow(recaps12) # "N " 63
recaps13 <- subset(capture, recap=="1strecord")
nrow(recaps13) # "N " 7

recaps1$recap <- "Y"
recaps2 $recap <- "Y"
recaps3 $recap <- "Y"
recaps4 $recap <- "Y"
recaps5 $recap <- "Y"
recaps6 $recap <- "Y"
recaps7 $recap <- "Y"
recaps8 $recap <- "Y"
recaps9 $recap <- "Y"
recaps10 $recap <- "Y"
recaps11 $recap <- "Y"
recaps12 $recap <- "Y"
recaps13 $recap <- "Y"

recaps14 <- rbind(recaps1,recaps2,recaps3,recaps4,recaps5,recaps6,recaps7,recaps8,recaps9, recaps10, recaps11, recaps12, recaps13)
str(recaps14) ## n = 773

unique(recaps14$PIT.tag)

recaps14$recap <- as.factor(recaps14$recap)
str(recaps14)
recaps15=subset(recaps14,julian > 200 & julian < 274)
str(recaps15)
recaps16=subset(recaps14,julian > 90 & julian < 160)
str(recaps16)

PITs=data.frame(recaps15$julian, recaps15 $year, recaps15 $mass, recaps15 $sex, recaps15 $faavg, recaps15 $site, recaps15 $PIT.tag)
names(PITs)[1]="julian"
names(PITs)[2]="year"
names(PITs)[3]="mass"
names(PITs)[4]="sex"
names(PITs)[5]="faavg"
names(PITs)[6]="site"
names(PITs)[7]="PIT.tag"
str(PITs)


PITs_emerge=data.frame(recaps16$julian, recaps16 $year, recaps16 $mass, recaps16 $sex, recaps16 $faavg, recaps16 $site, recaps16 $PIT.tag)
names(PITs_emerge)[1]="julian"
names(PITs_emerge)[2]="year"
names(PITs_emerge)[3]="mass"
names(PITs_emerge)[4]="sex"
names(PITs_emerge)[5]="faavg"
names(PITs_emerge)[6]="site"
names(PITs_emerge)[7]="PIT.tag"
str(PITs_emerge)

capture_N=subset(capture,recap =="N")
capture_swarm=subset(capture_N,julian > 200 & julian < 274)
capture_emerge=subset(capture_N,julian > 90 & julian < 160) 

head(capture_emerge)
caps=data.frame(capture_swarm $julian, capture_swarm $year, capture_swarm $mass, capture_swarm $sex, capture_swarm $faavg, capture_swarm $site, capture_swarm $PIT.tag)
names(caps)[1]="julian"
names(caps)[2]="year"
names(caps)[3]="mass"
names(caps)[4]="sex"
names(caps)[5]="faavg"
names(caps)[6]="site"
names(caps)[7]="PIT.tag"
str(caps)

caps_emerge=data.frame(capture_emerge $julian, capture_emerge $year, capture_emerge $mass, capture_emerge $sex, capture_emerge $faavg, capture_emerge $site, capture_emerge $PIT.tag)
names(caps_emerge)[1]="julian"
names(caps_emerge)[2]="year"
names(caps_emerge)[3]="mass"
names(caps_emerge)[4]="sex"
names(caps_emerge)[5]="faavg"
names(caps_emerge)[6]="site"
names(caps_emerge)[7]="PIT.tag"
str(caps_emerge)


recap_bats=merge(PITs,caps,by="PIT.tag")
str(recap_bats)
head(recap_bats,25)
recap_bats $bci_1= recap_bats $mass.x/recap_bats $faavg.x
recap_bats $bci_2= recap_bats $mass.y/recap_bats $faavg.y

recap_emerge=merge(PITs_emerge,caps_emerge,by="PIT.tag")
str(recap_emerge)
head(recap_emerge,25)
recap_emerge$bci_1=recap_emerge$mass.x/recap_emerge$faavg.x
recap_emerge$bci_2=recap_emerge$mass.y/recap_emerge$faavg.y


## only for swarming sites
recap_bats <- subset(recap_bats, site.x == "AB" 
	| site.x == "SG" | site.x == "RL" | site.x == "SQ" 
	| site.x == "PC" | site.x == "LP" | site.x == "DA" 
	| site.x == "FC" | site.x == "MW")
str(recap_bats)

unique(recap_emerge$site.x)
recap_emerge <- subset(recap_emerge, site.x == "AB" 
	| site.x == "SG" | site.x == "RL" | site.x == "SQ" 
	| site.x == "PC" | site.x == "LP" | site.x == "DA" 
	| site.x == "FC" | site.x == "MW")
str(recap_emerge)


recap_bats = recap_bats[complete.cases(recap_bats[1]),]
str(recap_bats)
recap_emerge = recap_emerge[complete.cases(recap_emerge[1]),]
str(recap_emerge)

names(recap_bats)[3]="year_1"
names(recap_bats)[9]="year_2"

names(recap_emerge)[3]="year_1"
names(recap_emerge)[9]="year_2"

unique(recap_bats $year_1)
unique(recap_bats $year_2)
y1=subset(recap_bats,year_1=="2013" & year_2=="2009") ## all NA's
y2=subset(recap_bats, year_1 =="2013" & year_2 =="2010") ## all NA's
y3=subset(recap_bats, year_1 =="2013" & year_2 =="2011") ## n = 2
y4=subset(recap_bats, year_1 =="2013" & year_2 =="2012") ## n = 6
y5=subset(recap_bats, year_1 =="2013" & year_2 =="2014") ## no data

y6=subset(recap_bats,year_1=="2012" & year_2 =="2009") ## no data
y7=subset(recap_bats,year_1=="2012" & year_2 =="2010") ## no data
y8=subset(recap_bats,year_1=="2012" & year_2 =="2011") ## no data
y9=subset(recap_bats,year_1=="2012" & year_2 =="2013") ## no data
y10=subset(recap_bats,year_1=="2012" & year_2 =="2014") ## no data

y11=subset(recap_bats,year_1=="2009" & year_2 =="2010") ## all NA's
y12=subset(recap_bats, year_1 =="2009" & year_2 =="2011")
y13=subset(recap_bats, year_1 =="2009" & year_2 =="2012")
y14=subset(recap_bats, year_1 =="2009" & year_2 =="2013")
y15=subset(recap_bats, year_1 =="2009" & year_2 =="2014")

y16=subset(recap_bats,year_1=="2014" & year_2=="2009")
y17=subset(recap_bats, year_1 =="2014" & year_2 =="2010")
y18=subset(recap_bats, year_1 =="2014" & year_2 =="2011")
y19=subset(recap_bats, year_1 =="2014" & year_2 =="2012")
y20=subset(recap_bats, year_1 =="2014" & year_2 =="2013")

rep_swarm=rbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20)
str(rep_swarm)

## early swarming
early_swarm=subset(rep_swarm,julian.x < 237 & julian.y < 237)
str(early_swarm)
mid_swarm=subset(rep_swarm,julian.x >= 237 & julian.y >= 237 & julian.x <= 247 & julian.y <= 247)
str(mid_swarm)
late_swarm=subset(rep_swarm,julian.x > 247 & julian.y > 247)
str(late_swarm)



unique(recap_emerge $year_1)
unique(recap_emerge $year_2)
y1=subset(recap_emerge,year_1=="2009" & year_2=="2008") ## all NA's
y2=subset(recap_emerge, year_1 =="2009" & year_2 =="2011") ## all NA's
y3=subset(recap_emerge, year_1 =="2009" & year_2 =="2013") ## n = 2
y4=subset(recap_emerge, year_1 =="2009" & year_2 =="2014") ## n = 6

y5=subset(recap_emerge,year_1=="2010" & year_2 =="2008") ## no data
y6=subset(recap_emerge,year_1=="2010" & year_2 =="2009") ## no data
y7=subset(recap_emerge,year_1=="2010" & year_2 =="2011") ## no data
y8=subset(recap_emerge,year_1=="2010" & year_2 =="2013") ## no data
y9=subset(recap_emerge,year_1=="2010" & year_2 =="2014") ## no data

y10=subset(recap_emerge,year_1=="2011" & year_2 =="2008") ## all NA's
y11=subset(recap_emerge, year_1 =="2011" & year_2 =="2009")
y12=subset(recap_emerge, year_1 =="2011" & year_2 =="2013")
y13=subset(recap_emerge, year_1 =="2011" & year_2 =="2014")

y14=subset(recap_emerge,year_1=="2013" & year_2=="2008")
y15=subset(recap_emerge, year_1 =="2013" & year_2 =="2009")
y16=subset(recap_emerge, year_1 =="2013" & year_2 =="2011")
y17=subset(recap_emerge, year_1 =="2013" & year_2 =="2014")

y18=subset(recap_emerge,year_1=="2014" & year_2=="2008")
y19=subset(recap_emerge, year_1 =="2014" & year_2 =="2009")
y20=subset(recap_emerge, year_1 =="2014" & year_2 =="2011")
y21=subset(recap_emerge, year_1 =="2014" & year_2 =="2013")

rep_emerge=rbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21)
str(rep_emerge)
unique(rep_emerge$PIT.tag)
rep_emergeF=subset(rep_emerge,sex.x=="F")
rep_emergeM=subset(rep_emerge,sex.x=="M")
early_swarmF=subset(early_swarm,sex.x=="F")
early_swarmM=subset(early_swarm,sex.x=="M")
mid_swarmF=subset(mid_swarm,sex.x=="F")
mid_swarmM=subset(mid_swarm,sex.x=="M")
late_swarmF=subset(late_swarm,sex.x=="F")
late_swarmM=subset(late_swarm,sex.x=="M")
str(late_swarmM)
plot(mass.x~mass.y,data=rep_emerge)

mass=rbind(early_swarm,mid_swarm,late_swarm, rep_emerge)
str(mass)


for(i in 1:58){
	mass$mass_1_std[i]=(mass$mass.x[i] - mean(mass$mass.x,na.rm=T))/(sd(mass$mass.x,na.rm=T))
}
for(i in 1:58){
	mass$mass_2_std[i]=(mass$mass.y[i] - mean(mass$mass.y,na.rm=T))/(sd(mass$mass.y,na.rm=T))
}


massM=subset(mass,sex.x=="M")
massF=subset(mass,sex.x=="F")

plot(mass.x~ mass.y,data=rep_emergeF,ylim=c(6,16),xlim=c(6,16),pch=17,col="red",ylab="Mass (Year 1)",xlab="Mass (Year 2)",las=1)
par(new=T)
plot(mass.x~ mass.y,data=rep_emergeM,ylim=c(6,16),xlim=c(6,16),pch=16,yaxt="n",xaxt="n",col="red",ylab="",xlab="",las=1)
par(new=T) ## no data for females early swarming
plot(mass.x~ mass.y,data=early_swarmM,ylim=c(6,16),xlim=c(6,16),yaxt="n",xaxt="n",pch=16,col="blue",ylab="",xlab="",las=1)
par(new=T) ## no data for females mid swarming
plot(mass.x~ mass.y,data=mid_swarmM,ylim=c(6,16),xlim=c(6,16),yaxt="n",xaxt="n",pch=16,col="darkgreen",ylab="",xlab="",las=1)
par(new=T)
plot(mass.x~ mass.y,data=late_swarmF,ylim=c(6,16),xlim=c(6,16),yaxt="n",xaxt="n",pch=17,col="purple",ylab="",xlab="",las=1)
par(new=T)
plot(mass.x~ mass.y,data=late_swarmM,ylim=c(6,16),xlim=c(6,16),yaxt="n",xaxt="n",pch=16,col="purple",ylab="",xlab="",las=1)
abline(lm(mass.x~ mass.y,data=mass,na.action=na.omit),lwd=2)
a=c(0:1)
b=c(0:1)
abline(lm(a~b),lty=2)
legend(6,16,c("emergence","early swarm","mid swarm","late swarm","male","female"),col=c("red","blue","darkgreen","purple","black","black"),pch=c(16,16,16,16,17,16),bty="n")

text(7,12.5,"r = 0.87")
text(7,12,"p < 0.001")

massF=subset(mass,sex.x=="F")
massM=subset(mass,sex.x=="M")
plot(mass_1_std ~ mass_2_std,massF,pch=16,ylim=c(-2,3),xlim=c(-2,3))
par(new=T)
plot(mass_1_std ~ mass_2_std,massM,pch=16,ylim=c(-2,3),xlim=c(-2,3),col="red")

library(irr)
mass_icc=data.frame(mass$mass.x,mass$mass.y)
mass_icc_std=data.frame(mass$mass_1_std,mass$mass_2_std)
icc(mass_icc)
icc(mass_icc_std)

mass2=c(mass$mass.x,mass$mass.y)
id=data.frame(mass$PIT.tag)
sex=c(mass$sex.x,mass$sex.y)
year=c(mass$year_1,mass$year_2)
mass_rep=data.frame(mass2,id,sex,year)
str(mass_rep)
names(mass_rep)[2]="PIT.tag"
a1=lmer(mass2 ~1+(1| PIT.tag),data= mass_rep,na.action=na.omit)
summary(a1)
repfunc(a1)
a2=lmer(mass2 ~sex+(1| PIT.tag),data= mass_rep,na.action=na.omit)
summary(a2)
repfunc(a2)
a3=lmer(mass2 ~year+(1| PIT.tag),data= mass_rep,na.action=na.omit)
summary(a3)
repfunc(a3)
a4=lmer(mass2 ~year+sex+(1| PIT.tag),data= mass_rep,na.action=na.omit)
summary(a4)
repfunc(a4)





###### reaction norm figures: 
pit_emergence_M=subset(pit_emergence3,sex=="M" & multiple=="multiple")
str(pit_emergence_M)
pit_emergence_F=subset(pit_emergence3,sex=="F"& multiple=="multiple")
str(pit_emergence_F)
counts1_M=subset(pit_emergence_M,count =="1")
counts2_M=subset(pit_emergence_M,count=="2")
counts3_M=subset(pit_emergence_M,count=="3")
counts4_M=subset(pit_emergence_M,count=="4")

counts1_F=subset(pit_emergence_F,count =="1")
counts2_F=subset(pit_emergence_F,count=="2")
counts3_F=subset(pit_emergence_F,count=="3")
counts4_F=subset(pit_emergence_F,count=="4")

## MALES
par(mfrow=c(1,2))
plot(jitter(counts1_M $count),counts1_M$julian_emerge2,xlim=c(1,4),ylim=c(90,170),ylab="Emergence Date (Julian Day)",xlab="",pch=20,las=1,main="Males",xaxt="n")
axis(side=1,at=1,"1st Detection")
axis(side=1,at=2,"2nd Detection")
axis(side=1,at=3,"3rd Detection")
axis(side=1,at=4,"4th Detection")
par(new=T)
plot(jitter(counts2_M$count),counts2_M$julian_emerge2,xlim=c(1,4),ylim=c(90,170),xaxt="n",yaxt="n",ylab="",xlab="",pch=20)
s <- seq(length(counts1_M$julian_emerge2)-1)  # one shorter than data
s
segments(counts1_M$count[s],counts1_M$julian_emerge2[s],counts2_M$count[s],counts2_M$julian_emerge2[s],col="black")
par(new=T)
plot(counts3_M$count,counts3_M$julian_emerge2,xlim=c(1,4),ylim=c(90,170),xaxt="n",yaxt="n",ylab="",xlab="",pch=20)

s <- seq(length(counts2_M$julian_emerge2)-1)  # one shorter than data
s
segments(counts2_M$count[s],counts2_M$julian_emerge2[s],counts3_M$count[s],counts3_M$julian_emerge2[s],col="black")
par(new=T)
plot(counts4_M$count,counts4_M$julian_emerge2,xlim=c(1,4),ylim=c(90,170),xaxt="n",yaxt="n",ylab="",xlab="",pch=20)
s <- seq(length(counts2_M$julian_emerge2)-1)  # one shorter than data
s
segments(counts3_M$count[s],counts3_M$julian_emerge2[s],counts4_M$count[s],counts4_M$julian_emerge2[s],col="black")
med=c(median(counts1_M$julian_emerge2),median(counts2_M$julian_emerge2),median(counts3_M$julian_emerge2),median(counts4_M$julian_emerge2))
med
quant5=c(quantile(counts1_M$julian_emerge2,0.025),quantile(counts2_M$julian_emerge2,0.025),quantile(counts3_M$julian_emerge2,0.025),quantile(counts4_M$julian_emerge2,0.025))
quant95=c(quantile(counts1_M$julian_emerge2,0.975),quantile(counts2_M$julian_emerge2, 0.975),quantile(counts3_M$julian_emerge2, 0.975),quantile(counts4_M$julian_emerge2, 0.975))

trial=c(1,2,3,4)

lines(med~trial,lwd=4,col="red")
lines(quant5~trial,lwd=2,lty=2,col="red")
lines(quant95~trial,lwd=2,lty=2,col="red")

## FEMALES
plot(jitter(counts1_F $count),counts1_F$julian_emerge2,xlim=c(1,4),ylim=c(90,170),ylab="Emergence Date (Julian Day)",xlab="",pch=20,las=1,main="Females",xaxt="n")
axis(side=1,at=1,"1st Detection")
axis(side=1,at=2,"2nd Detection")
axis(side=1,at=3,"3rd Detection")
axis(side=1,at=4,"4th Detection")
par(new=T)
plot(jitter(counts2_F$count),counts2_F$julian_emerge2,xlim=c(1,4),ylim=c(90,170),xaxt="n",yaxt="n",ylab="",xlab="",pch=20)
s <- seq(length(counts1_F$julian_emerge2)-1)  # one shorter than data
s
segments(counts1_F$count[s],counts1_F$julian_emerge2[s],counts2_F$count[s],counts2_F$julian_emerge2[s],col="black")
par(new=T)
plot(counts3_F$count,counts3_F$julian_emerge2,xlim=c(1,4),ylim=c(90,170),xaxt="n",yaxt="n",ylab="",xlab="",pch=20)

s <- seq(length(counts2_F$julian_emerge2)-1)  # one shorter than data
s
segments(counts2_F$count[s],counts2_F$julian_emerge2[s],counts3_F$count[s],counts3_F$julian_emerge2[s],col="black")
par(new=T)
plot(counts4_F$count,counts4_F$julian_emerge2,xlim=c(1,4),ylim=c(90,170),xaxt="n",yaxt="n",ylab="",xlab="",pch=20)
s <- seq(length(counts2_F$julian_emerge2)-1)  # one shorter than data
s
segments(counts3_F$count[s],counts3_F$julian_emerge2[s],counts4_F$count[s],counts4_F$julian_emerge2[s],col="black")
med=c(median(counts1_F$julian_emerge2),median(counts2_F$julian_emerge2),median(counts3_F$julian_emerge2),median(counts4_F$julian_emerge2))
med
quant5=c(quantile(counts1_F$julian_emerge2,0.025),quantile(counts2_F$julian_emerge2,0.025),quantile(counts3_F$julian_emerge2,0.025),quantile(counts4_F$julian_emerge2,0.025))
quant95=c(quantile(counts1_F$julian_emerge2,0.975),quantile(counts2_F$julian_emerge2, 0.975),quantile(counts3_F$julian_emerge2, 0.975),quantile(counts4_F$julian_emerge2, 0.975))


trial=c(1,2,3,4)

lines(med~trial,lwd=4,col="red")
lines(quant5~trial,lwd=2,lty=2,col="red")
lines(quant95~trial,lwd=2,lty=2,col="red")