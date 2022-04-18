library(dplyr)
library(tidyr)
library(lubridate)
getwd()

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

###############PIT tag data##################
#data organization and management
d20xx<-read.csv('pit.data.2008-2014.csv')
d2015<-read.csv('2015_Pit_tag_cave_data_final.csv')
d2016<-read.csv('2016_Pit_tag_cave_data_final.csv')
d2017<-read.csv('2017_Pit_tag_cave_data_final.csv')

#check the structure and headings of the dataframes
str(d20xx)
str(d2015)
str(d2016)
str(d2017)

#merge dataframes into a complete pittag database
pit<-rbind(d20xx,d2015,d2016,d2017)
str(pit)

#coerce columns to appropriate format
pit$date<-as.Date(pit$date, "%d/%m/%Y")
pit$time<-as.factor(pit$time)
str(pit)

#isolate year as a factor for subsetting later
pit$year=format(pit$date, format="%Y") 
pit$year=as.factor(pit$year)
unique(pit$year)
str(pit)

#assign julian dates for date of recording (to subset emergence and immergence dates later)
pit$day<-yday(pit$date)
pit$month<-month(pit$date, label = TRUE, abbr = TRUE)
pit$day<-as.integer(pit$day)
str(pit)
head(pit,150)

#check and remove duplicates
pit_dupe=subset(pit, !duplicated(subset(pit,select=c(code,time,year))))
#how many unique tags were redetected
sapply(pit_dupe, function(code) length(unique(code))) #3881 unique pittags detected (at least once), this shouldn't change from the raw data

#count freq of detections by month
detec.freq<-count(pit_dupe, month)
barplot(detec.freq$n, names.arg=detec.freq$month, xlab = "months", ylim=c(0,65000), las=2) 
#emergence from april-may and immergence from august-sept

#restrict dataset between day 90 (March 31) and day 166 (June 15) for emergence from hibernation
pit_em<-subset(pit_dupe, day>=90 & day<=166)
str(pit_em)
## restrict dataset between day 227 (aug 15) and day 274 (Oct 15) for immergence into hibernation
pit_im<-subset(pit_dupe, day>=227 & day<=274)
str(pit_im)

###emergence####
str(pit_em)
#remove duplicate records
#by time (recorded twice by the reader)
pit_em2=subset(pit_em, !duplicated(subset(pit_em,select=c(code,time,year))))
#by day (recorded multiple times in one day)
pit_em3=subset(pit_em2,!duplicated(subset(pit_em2,select=c(code,day,year))))

###sapply(pit_em3, function(code) length(unique(code))) #2437 unique tags detected EMERGING

#by year (recorded multiple times in one year) #NOTE might not need this 
#pit_em4=subset(pit_em3,!duplicated(subset(pit_em3,select=c(code,year))))
#str(pit_em4)

#identify individuals that were detected more than once
edi=pit_em3
ed=pit_em3
edi$code=as.factor(edi$code)
edi=edi[order(edi$code), ]
c=classic.counter(edi$code)
edi$count=c$counter

edi=edi[order(edi$code, -edi$count), ]
counts=edi$count
edi=subset(edi, count == 2)
head(edi,10)
str(edi)

edi=subset(edi, select = c(code, count))
edi$count="multiple"
names(edi)[2]="mult"
head(edi)
dim(ed)
ed=merge(ed, edi, by = "code", all.x=T)
dim(ed)
ed$mult=as.factor(ed$mult)
head(ed,10)
summary(ed)
str(ed)
ed$count=counts

#check the structure of the data frame to see how many individuals have multiple readings and how many years have detections
str(ed) #NOTE count is just the counter, 1 just means the first time it was seen, not that there is only one observation!
unique(ed$year)
length(unique(edi$code)) 

#save as a dataframe for merging with capture data
pit_emergence<-ed
str(pit_emergence)

###immergence####
str(pit_im)
#remove duplicate records
#by time (recorded 2+ times by the reader)
pit_im2=subset(pit_im, !duplicated(subset(pit_im,select=c(code,time,year))))
#by day (recorded multiple times in one day)
pit_im3=subset(pit_im2,!duplicated(subset(pit_im2,select=c(code,day,year))))

###sapply(pit_im3, function(code) length(unique(code))) #2302 unique tags detected IMMERGING

#by year (recorded multiple times in one year) #NOTE might not need this
#pit_im4=subset(pit_im3,!duplicated(subset(pit_im3,select=c(code,year))))
#str(pit_im4)

#identify individuals that were detected more than once
eti=pit_im3
et=pit_im3
eti$code=as.factor(eti$code)
eti=eti[order(eti$code), ]
c=classic.counter(eti$code)
eti$count=c$counter

eti=eti[order(eti$code, -eti$count), ]
counts=eti$count
eti=subset(eti, count == 2)
head(eti,10)
str(eti)

eti=subset(eti, select = c(code, count))
eti$count="multiple"
names(eti)[2]="mult"
head(eti)
dim(et)
et=merge(et, eti, by = "code", all.x=T)
dim(et)
et$mult=as.factor(et$mult)
head(et,10)
summary(et)
str(et)
et$count=counts

#check the structure of the data frame to see how many individuals have multiple readings and how many years have detections
str(et) #NOTE count is just the counter, 1 just means the first time it was seen, not that there is only one observation!
unique(et$year)
length(unique(eti$code)) 

#save as a dataframe for merging with capture data
pit_immergence<-et
str(pit_immergence)

###########Capture data##############
capture.data<-read.csv("PITTAG Capture master sheet 2008-2017.csv")
head(capture.data)
capture.data$capdate<-as.Date(capture.data$capdate,"%d/%m/%Y")
str(capture.data)

#copy capture data and remove rows with NAs in the pittag column
cap<-capture.data[complete.cases(capture.data[,4]),] 
str(cap)

#identify year of capture
cap$capyear=format(cap$capdate, format="%Y") 
cap$capyear=as.factor(cap$capyear)
unique(cap$capyear)
unique(cap$recap)
unique(cap$age)
str(cap)

#number of captures per year
cap.info<-count(cap, capyear)
cap.info
#number of recaptures
count(cap, recap)
count(cap, recap)
#select the columns we want for merging with pittag data for encounter histories
eh_cap<-select(cap, code, location, capyear, Age, sex, mass, faavg, bci)

#rename the year column so it will merge with pittag data
eh_cap<-rename(eh_cap, year=capyear)

#merge pittag data with capture data by id=pittag
#merge pit TO capture data
cap_pit_merge<- merge(eh_cap, pit_dupe, by = c("code"), all=T)
head(cap_pit_merge)

#merge capture data to emergence and immergence data
em_cap_merge<- merge(eh_cap,pit_emergence, by = c("code","year"), all=T)
im_cap_merge<- merge(eh_cap,pit_immergence, by = c("code","year"), all=T)

table(em_cap_merge$Age)

#write csvs for emergence and immergence
#write csv
write.csv(em_cap_merge,file="pit_em.csv")
write.csv(im_cap_merge,file="pit_im.csv")
