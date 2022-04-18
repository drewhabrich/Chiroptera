#this is the same analysis from script 3 but with the output from Rmark instead of regular mark
rm(list=ls())

getwd()
setwd('E:/R/pittag project/pittag analysis')
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
rm(list=ls())

#load the data and check the structure
est<-read.csv("Model estimates RMark.csv")
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

####survival plots#####
##Abyss
#marked as adults (just resident survival)
ABad.phip<-ggplot(ABadult.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25), size=1)+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))
ABad.phip 

ABad.phiSE<-ggplot(ABadult.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme+ scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))
ABad.phiSE 

#marked as juveniles #NOTE same as those marked as adults
ABmy.phip<-ggplot(ABmy.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25), size=1)+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme+ 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))
ABmy.phip 

#first year juvenile survival
ABjuv.phip<-ggplot(ABjuv.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25), size=1)+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme +
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))

ABjuv.phip
#First year resident + transients
ABrt.phip<-ggplot(ABrt.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25), size=1)+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme+ 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))
ABrt.phip 

##St George
#marked as adults (just resident survival)
SGad.phip<-ggplot(SGadult.phi, aes(year, estimate, color=index, group=index)) +
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25), size=1)+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme +
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1)) 
SGad.phip 

#marked as juveniles
SGmy.phip<-ggplot(SGmy.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25), size=1)+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme + 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))
SGmy.phip 

#first year juvenile survival
SGjuv.phip<-ggplot(SGjuv.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25), size=1)+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme + 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))
SGjuv.phip 

#First year resident + transients
SGrt.phip<-ggplot(SGrt.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=.25,position = position_dodge(0.25), size=1)+ 
  xlab('Year')+ylab('Estimate of apparent survival')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme + 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))
SGrt.phip 

#####recapture probability plots####
#filter out the replicated survivals by the group (based on model selection, AB grouped by sex, SG grouped by age)
fAB<-all.p %>% filter(cave=='AB', index=='F-MA' &cave=='AB'| index=='M-MA'&cave=='AB')
fSG<-all.p %>% filter(cave=='SG', index=='F-MA' &cave=='SG'| index=='F-MY'&cave=='SG')

#ABYSS
pAB<-ggplot(fAB, aes(x=year, y=estimate, group = index)) +
  geom_line(position = position_dodge(0.25),aes(linetype=index),size=1)+       
  geom_errorbar(aes(ymin=LSE, ymax=USE), 
                width=.25, 
                position = position_dodge(0.25), 
                size=1)+ 
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

####Side by side plots####
#SG survival
#marked as adults (just resident survival)
SGa<-ggplot(SGadult.phi, aes(year, estimate, color=index, group=index)) +
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme +
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))+ 
  annotate("text", x=1, y=1, label= "A", size =8)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(colour='white'),
        axis.title.y=element_blank())
#marked as juveniles
SGb<-ggplot(SGmy.phi, aes(year, estimate, color=index, group=index)) +  
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme + 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1, y=1, label= "B", size =8)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(colour='white'),
        axis.title.y=element_blank(),
        axis.text.y=element_text(colour='white'))
#First year resident + transients
SGc<-ggplot(SGrt.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme + 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1, y=1, label= "C", size =8)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
#first year juvenile survival
SGd<-ggplot(SGjuv.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme + 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1, y=1, label= "D", size =8)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(colour='white'))

grid.arrange(SGa,SGb,SGc,SGd, nrow = 2, 
             left=textGrob("Estimate of apparent survival", gp=gpar(fontsize=15), rot=90, vjust=0.5),
             bottom=textGrob("Year", gp=gpar(fontsize=15), vjust=0))


##AB survival
#marked as adults (just resident survival)
ABa<-ggplot(ABadult.phi, aes(year, estimate, color=index, group=index)) +
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme +
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))+ 
  annotate("text", x=1, y=1, label= "A", size =8)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(colour='white'),
        axis.title.y=element_blank())
#marked as juveniles
ABb<-ggplot(ABmy.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme + 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1, y=1, label= "B", size =8)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(colour='white'),
        axis.title.y=element_blank(),
        axis.text.y=element_text(colour='white'))
#First year resident + transients
ABc<-ggplot(ABrt.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme + 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1, y=1, label= "C", size =8)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
#first year juvenile survival
ABd<-ggplot(ABjuv.phi, aes(year, estimate, color=index, group=index)) + 
  geom_point(size=5, position = position_dodge(0.25))+
  geom_path(size=0.5,position = position_dodge(0.25))+
  geom_errorbar(aes(ymin=LSE, ymax=USE), width=.25,position = position_dodge(0.25), size=1)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme + 
  scale_color_manual(values=c("red1", "blue1")) + scale_y_continuous(limits = c(0, 1))+
  annotate("text", x=1, y=1, label= "D", size =8)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_text(colour='white'))

grid.arrange(ABa,ABb,ABc,ABd, nrow = 2, 
             left=textGrob("Estimate of apparent survival", gp=gpar(fontsize=15), rot=90, vjust=0.5),
             bottom=textGrob("Year", gp=gpar(fontsize=15), vjust=0))

#####Save the figures####
ppi=400 #specify ppi
png(file="SG estimates of survival.png",width=2400,height=2600,res=275) #and filetype, name, and size
grid.arrange(SGa,SGb,SGc,SGd, nrow = 2, 
             left=textGrob("Estimate of apparent survival", gp=gpar(fontsize=15), rot=90, vjust=0.4, hjust = 0.35),
             bottom=textGrob("Year", gp=gpar(fontsize=15), vjust=0))
dev.off() #finish 

ppi=400 #specify ppi
png(file="AB estimates of survival.png",width=2400,height=2600,res=275) #and filetype, name, and size
grid.arrange(ABa,ABb,ABc,ABd, nrow = 2, 
             left=textGrob("Estimate of apparent survival", gp=gpar(fontsize=15), rot=90, vjust=0.40, hjust = 0.35),
             bottom=textGrob("Year", gp=gpar(fontsize=15), vjust=0))
dev.off() #finish 
