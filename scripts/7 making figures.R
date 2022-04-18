#Figures
rm(list=ls())

getwd()
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

est<-read.csv("Final model estimates RMARK.csv")
str(est)

####subset####
#subset phi vs p 
all.phi<-est[which(est$label=='Phi'), ]
all.p<-est[which(est$label=='p'), ]
str(all.phi)
str(all.p)

#subset survival estimates
ABadult.phi<-all.phi[which(all.phi$index=='F-MA'& all.phi$cave=='AB'| all.phi$index=='M-MA' & all.phi$cave=='AB'), ]
SGadult.phi<-all.phi[which(all.phi$index=='F-MA'& all.phi$cave=='SG'| all.phi$index=='M-MA' & all.phi$cave=='SG'), ]

ABmy.phi<-all.phi[which(all.phi$index=='F-MY'& all.phi$cave=='AB'| all.phi$index=='M-MY' & all.phi$cave=='AB'), ]
SGmy.phi<-all.phi[which(all.phi$index=='F-MY'& all.phi$cave=='SG'| all.phi$index=='M-MY' & all.phi$cave=='SG'), ]

ABjuv.phi<-all.phi[which(all.phi$index=='F-JUV'& all.phi$cave=='AB'| all.phi$index=='M-JUV' & all.phi$cave=='AB'), ]
SGjuv.phi<-all.phi[which(all.phi$index=='F-JUV'& all.phi$cave=='SG'| all.phi$index=='M-JUV' & all.phi$cave=='SG'), ]

ABrt.phi<-all.phi[which(all.phi$index=='F-RT'& all.phi$cave=='AB'| all.phi$index=='M-RT' & all.phi$cave=='AB'), ]
SGrt.phi<-all.phi[which(all.phi$index=='F-RT'& all.phi$cave=='SG'| all.phi$index=='M-RT' & all.phi$cave=='SG'), ]

#subset recapture estimates
AB.p<-all.p %>% filter(cave=='AB', index=='F-MA' &cave=='AB'| index=='F-MY'&cave=='AB'| index=='M-MA' & cave=='AB'| index=='M-MY')
SG.p<-all.p %>% filter(cave=='SG', index=='F-MA' &cave=='SG'| index=='F-MY'&cave=='SG'| index=='M-MA' & cave=='SG'| index=='M-MY')

#plot out to visualize
#general theme format
theme <-theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 45, hjust = 1),
                           axis.text=element_text(size=12), axis.title=element_text(size=16), legend.position="none")

#####survival plots#####
##Abyss
#marked as adults (just resident survival)
ABa<-ggplot(ABadult.phi, aes(year, estimate, group=index, fill=index)) + 
  geom_path(aes(linetype=index),size=1,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  geom_point(size=5, shape=21, stroke= 1, position = position_dodge(0.25))+
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_fill_manual(values=c("black", "white")) + scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1.65, y=1, label= "A - Adult", size =4)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())
ABa

#marked as juveniles #NOTE same as those marked as adults
ABb<-ggplot(ABmy.phi, aes(year, estimate, group=index, fill=index)) + 
  geom_path(aes(linetype=index),size=1,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  geom_point(size=5, shape=21, stroke= 1, position = position_dodge(0.25))+
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_fill_manual(values=c("black", "white")) + scale_y_continuous(limits = c(0, 1))+ 
  annotate("text", x=3.5, y=1, label= "B - Adults marked as YOY", size =4)+
  theme(axis.title.y=element_blank(),axis.text.y=element_text(colour='white'),axis.title.x=element_blank())
ABb

#first year juvenile survival
ABc<-ggplot(ABjuv.phi, aes(year, estimate, group=index, fill=index)) + 
  geom_path(aes(linetype=index),size=1,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  geom_point(size=5, shape=21, stroke= 1, position = position_dodge(0.25))+
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_fill_manual(values=c("black", "white")) + scale_y_continuous(limits = c(0, 1))+ 
  annotate("text", x=1.6, y=1, label= "C - YOY", size =4)+
  theme(axis.title.y=element_blank(),axis.text.y=element_text(colour='white'),axis.title.x=element_blank())
ABc

#First year resident + transients
ABd<-ggplot(ABrt.phi, aes(year, estimate, group=index, fill=index)) + 
  geom_path(aes(linetype=index),size=1,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  geom_point(size=5, shape=21, stroke= 1, position = position_dodge(0.25))+
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_fill_manual(values=c("black", "white")) + scale_y_continuous(limits = c(0, 1))
ABd

####Side by side plots
#Abyss
grid.arrange(ABa,ABb,ABc, nrow = 1, 
             left=textGrob("Estimate of apparent survival", gp=gpar(fontsize=15), rot=90, vjust=0.5),
             bottom=textGrob("Year", gp=gpar(fontsize=15), vjust=0))

#export files
getwd()

ppi=400 #specify ppi
png(file="AB estimates of survival.png",width=2400,height=1200,res=300) #and filetype, name, and size
grid.arrange(ABa,ABb,ABc, nrow = 1, 
             left=textGrob("Estimate of apparent survival", gp=gpar(fontsize=16), rot=90, vjust=0.5, hjust=0.35),
             bottom=textGrob("Year", gp=gpar(fontsize=16), vjust=0, hjust=-0.15))
dev.off() #finish 


##St George
#marked as adults (just resident survival)
SGa<-ggplot(SGadult.phi, aes(year, estimate, group=index, fill=index)) + 
  geom_path(aes(linetype=index),size=1,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  geom_point(size=5, shape=21, stroke= 1, position = position_dodge(0.25))+
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_fill_manual(values=c("black", "white")) + scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1.65, y=1, label= "A - Adult", size =4)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())
SGa

#marked as juveniles #NOTE same as those marked as adults
SGb<-ggplot(SGmy.phi, aes(year, estimate, group=index, fill=index)) + 
  geom_path(aes(linetype=index),size=1,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  geom_point(size=5, shape=21, stroke= 1, position = position_dodge(0.25))+
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_fill_manual(values=c("black", "white")) + scale_y_continuous(limits = c(0, 1))+ 
  annotate("text", x=3.5, y=1, label= "B - Adults marked as YOY", size =4)+
  theme(axis.title.y=element_blank(),axis.text.y=element_text(colour='white'),axis.title.x=element_blank())
SGb 

#first year juvenile survival
SGc<-ggplot(SGjuv.phi, aes(year, estimate, group=index, fill=index)) + 
  geom_path(aes(linetype=index),size=1,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  geom_point(size=5, shape=21, stroke= 1, position = position_dodge(0.25))+
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_fill_manual(values=c("black", "white")) + scale_y_continuous(limits = c(0, 1))+ 
  annotate("text", x=1.6, y=1, label= "C - YOY", size =4)+
  theme(axis.title.y=element_blank(),axis.text.y=element_text(colour='white'),axis.title.x=element_blank())
SGc 

#First year resident + transients
SGd<-ggplot(SGrt.phi, aes(year, estimate, group=index, fill=index)) + 
  geom_path(aes(linetype=index),size=1,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  geom_point(size=5, shape=21, stroke= 1, position = position_dodge(0.25))+
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_fill_manual(values=c("black", "white")) + scale_y_continuous(limits = c(0, 1))+ 
  theme+ scale_fill_manual(values=c("black", "white")) + scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1, y=1, label= "A - Adult", size =4)+
  theme(axis.title.y=element_blank())
SGd

####Side by side plots
#St. George
grid.arrange(SGa,SGb,SGc, nrow = 1, 
left=textGrob("Estimate of apparent survival", gp=gpar(fontsize=15), rot=90, vjust=0.5),
bottom=textGrob("Year", gp=gpar(fontsize=15), vjust=0))

#export files
getwd()

ppi=400 #specify ppi
png(file="SG estimates of survival.png",width=2400,height=1200,res=300) #and filetype, name, and size
grid.arrange(SGa,SGb,SGc, nrow = 1, 
             left=textGrob("Estimate of apparent survival", gp=gpar(fontsize=16), rot=90, vjust=0.5, hjust=0.35),
             bottom=textGrob("Year", gp=gpar(fontsize=16), vjust=0, hjust=-0.15))
dev.off() #finish 

####recapture plot#### ##################NEED TO REDO THESE TO MATCH THE UPDATED FIGURES IN THE DOCUMENT

#filter out the replicated survivals by the group (based on model selection, AB grouped by sex, SG grouped by age)
fAB<-all.p %>% filter(cave=='AB', index=='F-MA' &cave=='AB'| index=='M-MA'&cave=='AB')
fSG<-all.p %>% filter(cave=='SG', index=='F-MA' &cave=='SG'| index=='F-MY'&cave=='SG')

#ABYSS
pAB<-ggplot(fAB, aes(x=year, y=estimate, shape = index)) +
  geom_line(position = position_dodge(0.25),aes(linetype=index),size=1)+       
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25, position = position_dodge(0.25), size=1)+ 
  geom_point(aes(color=index),
             size = 6,
             position=position_dodge(0.25))  +  
  scale_y_continuous(limits = c(0, 1))+
  scale_color_manual(values=c("red1", "blue1"))+ #red=females, blue=males
  annotate("text", x=1, y=1, label= "AB", size =8)+
  theme+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(colour='white'),
        axis.title.y=element_blank())
pAB
#ST. GEORGE
pSG<-ggplot(fSG, aes(x=year, y=estimate, group = index)) +
  geom_line(position = position_dodge(0.25),aes(linetype=index), size=1)+       
  geom_errorbar(aes(ymin=LSE, ymax=USE), 
                width=.25, 
                position = position_dodge(0.25), 
                size=1)+ 
  geom_point(aes(shape=index),   
             fill = "white",   
             size = 6,
             position=position_dodge(0.25))  +  
  scale_shape_manual(values=c(21,24))+ #circle=adult, triangle=juveniles
  scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1, y=1, label= "SG", size =8)+
  theme+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
pSG 

grid.arrange(pAB,pSG, nrow = 2, ncol=1,
             left=textGrob("Recapture probability", gp=gpar(fontsize=18), rot=90, vjust=4),
             bottom=textGrob("Year", gp=gpar(fontsize=18), vjust=0))

#save the figure
ppi=400 #specify ppi
png(file="Recapture probability estimates.png",width=2400,height=2600,res=275) #and filetype, name, and size
grid.arrange(pAB,pSG, nrow = 2, 
             left=textGrob("Recapture probability", gp=gpar(fontsize=18), rot=90, vjust=0.5, hjust=0.2),
             bottom=textGrob("Year", gp=gpar(fontsize=15), vjust=0))
dev.off() #finish 
