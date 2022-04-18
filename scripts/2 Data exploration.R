library(ggplot2)

#import data
pit_im<-read.csv('pit_im.csv')
pit_em<-read.csv('pit_em.csv')

#check distribution of emergence and immergence dates by sex
str(pit_em)
str(pit_im)

ggplot(pit_em,aes(x=day))+geom_histogram()+facet_grid(~sex)+theme_bw()
ggplot(pit_im,aes(x=day))+geom_histogram()+facet_grid(~sex)+theme_bw()
