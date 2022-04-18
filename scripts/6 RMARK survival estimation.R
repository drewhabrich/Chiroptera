library(RMark)

####Full procedure####
#############################################################SG
#import the data
sg.eh=read.table(file="SG_EH.txt",header=TRUE,
                 colClasses=c("character","factor","numeric","numeric","numeric","factor","factor"))
head(sg.eh)
str(sg.eh) #make sure variables are the correct classes (either factors or numeric)

#process data for CJS model with groups
sg=process.data(sg.eh,model="CJS",groups=c("sex","ageclass"),age.var=2,initial.age=c(1,0)) #designate the initial age at time=0 for A and J, NOTE; I want age 0 for adults to capture residents/transients
str(sg)
print(sg)

#create design data with age bins for Phi
sg.ddl=make.design.data(sg,parameters=list(Phi=list(age.bins=c(0,1,6))),right=FALSE) #bin age classes for juveniles
sg.ddl=add.design.data(sg,sg.ddl,parameter="Phi",type="age", #bin resident only vs mixed resident/transient
                 bins=c(0,1,6),name="transience", replace=TRUE)
levels(sg.ddl$Phi$age)=c("0","1Plus")
levels(sg.ddl$Phi$transience)=c("RT","R")
sg.ddl$Phi[,-c(1,2,6)] #check if its added
sg.ddl$p
#add environmental variables
SGenv.data=data.frame(time=1:6,
                      minWtemp=c(-16.643,-11.271,-17.058,-20.680,-15.635,-11.247),
                      precip=c(1.019,0.495,0.921,1.470,0.684,0.623),
                      maxStemp=c(23.22777778,	20.85292208,	22.04784993,	19.71453349,	21.4490767,	20.42938312),
                      rain=c(1.133694084,	2.800974026,	1.353362573,	2.630752754,	1.567676768,	2.624140049))
str(SGenv.data)
sg.ddl$Phi=merge_design.covariates(sg.ddl$Phi,SGenv.data)

sg.ddl$Phi
str(sg.ddl)
## create analysis function
do_analysis=function()
{
  # create formulas for Phi
  Phi.dot=list(formula=~1) #constant over time
  Phi.time=list(formula=~time)
  Phi.sex=list(formula=~sex) #vary by sex
  Phi.notime=list(formula=~sex+age+transience) #no time variation by time
  Phi.nosex=list(formula=~age*time+transience*time)
  Phi.c.emigra=list(formula=~sex*time+age*time+transience) #consistant emigration
  Phi.sex.age.tra=list(formula=~sex*time+age*time+transience*time)
  Phi.global=list(formula=~sex*time+age*time+transience*time+minWtemp+precip+maxStemp+rain) #saturated model with the most parameters
  Phi.summer=list(formula=~sex*time+age*time+transience*time+maxStemp+rain) #summer env
  Phi.winter=list(formula=~sex*time+age*time+transience*time+minWtemp+precip) #winter env
  Phi.sum=list(formula=~sex*time+age*time+transience+maxStemp+rain) #summer env
  Phi.win=list(formula=~sex*time+age*time+transience+minWtemp+precip) #winter env
  
  # create formulas for p
  p.dot=list(formula=~1) #constant over time
  p.time=list(formula=~time) #variable over time
  p.sex=list(formula=~sex) #vary with sex
  p.sextime=list(formula=~sex*time) #vary with sex and time
  p.ageclass=list(formula=~ageclass) #varies with ageclass
  p.ageclass=list(formula=~ageclass*time) #varies with ageclass and time
  
  # create all combinations
  cml=create.model.list("CJS")
  # run all all models and return as a list with class marklist
  results=mark.wrapper(cml,data=sg,ddl=sg.ddl,output=FALSE,silent=TRUE)
  return(results)
}
#Now call the function to run the models and return the results
a.selection=do_analysis()
#adjust chat (overdispersion) if overdispersed
a.selection=adjust.chat(1.60,a.selection)
#Show the model selection table
a.selection

#model averaging
a.mavg=model.average(a.selection,parameter="Phi")
a.mavg[,c(1:5,9)]
a.mavg=model.average(a.selection,parameter="p")
a.mavg[,c(1:5,9)]
b.mavg=model.average(a.selection)
b.mavg

#############################################################AB
ab.eh=read.table(file="AB_EH.txt",header=TRUE,
                 colClasses=c("character","factor","numeric","numeric","numeric","factor","factor"))
head(ab.eh) 
str(ab.eh) #make sure variables are the correct classes (either factors or numeric)

#process data for CJS model with groups
ab=process.data(ab.eh,model="CJS",groups=c("sex","ageclass"),age.var=2,initial.age=c(1,0)) #designate the initial age at time=0 for A and J, NOTE; I want age 0 for adults to capture residents/transients
str(ab)
print(ab)

#create design data with age bins for Phi
ab.ddl=make.design.data(ab,parameters=list(Phi=list(age.bins=c(0,1,6))),right=FALSE) #bin age classes for juveniles
ab.ddl=add.design.data(ab,ab.ddl,parameter="Phi",type="age", #bin resident only vs mixed resident/transient
                       bins=c(0,1,6),name="transience", replace=TRUE)
levels(ab.ddl$Phi$age)=c("0","1Plus")
levels(ab.ddl$Phi$transience)=c("RT","R")
ab.ddl$Phi[,-c(1,2,6)] #check if its added

#add environmental variables
ABenv.data=data.frame(time=1:6,minWtemp=c(-18.45939394,-13.82168675,-19.51454545,-21.32634731,-17.70778443,-13.16011905),
                    precip=c(1.081097561,0.839759036,1.190909091,0.841935484,0.860606061,0.861309524),
                    maxStemp=c(20.77617605,19.74222222,19.81880952,18.71493506,19.98498804,18.58480861),
                    rain=c(2.936522367,	1.263393393,	6.55,	2.043720096,	2.139952153,	4.067469697))
str(ABenv.data)
ab.ddl$Phi=merge_design.covariates(ab.ddl$Phi,ABenv.data)
ab.ddl$Phi
str(ab.ddl)
## create analysis function
do_analysis=function()
{
  # create formulas for Phi
  Phi.dot=list(formula=~1) #constant over time
  Phi.time=list(formula=~time)
  Phi.sex=list(formula=~sex) #vary by sex
  Phi.notime=list(formula=~sex+age+transience) #no time variation by time
  Phi.nosex=list(formula=~age*time+transience*time)
  Phi.c.emigra=list(formula=~sex*time+age*time+transience) #consistant emigration
  Phi.sex.age.tra=list(formula=~sex*time+age*time+transience*time)
  Phi.global=list(formula=~sex*time+age*time+transience*time+minWtemp+precip+maxStemp+rain) #saturated model with the most parameters
  Phi.summer=list(formula=~sex*time+age*time+transience*time+maxStemp+rain) #summer env
  Phi.winter=list(formula=~sex*time+age*time+transience*time+minWtemp+precip) #winter env
  Phi.sum=list(formula=~sex*time+age*time+transience+maxStemp+rain) #summer env
  Phi.win=list(formula=~sex*time+age*time+transience+minWtemp+precip) #winter env
  # create formulas for p
  p.dot=list(formula=~1) #constant over time
  p.time=list(formula=~time) #variable over time
  p.sex=list(formula=~sex) #vary with sex
  p.sextime=list(formula=~sex*time) #vary with sex and time
  p.ageclass=list(formula=~ageclass) #varies with ageclass
  p.ageclass=list(formula=~ageclass*time) #varies with ageclass and time
  
  # create all combinations
  cml=create.model.list("CJS")
  # run all all models and return as a list with class marklist
  results=mark.wrapper(cml,data=ab,ddl=ab.ddl,output=FALSE,silent=TRUE)
  return(results)
}
#Now call the function to run the models and return the results
b.selection=do_analysis()
#adjust chat (overdispersion) if overdispersed
b.selection=adjust.chat(1,b.selection)
#Show the model selection table
b.selection

#model averaging
c.mavg=model.average(b.selection,parameter="Phi")
c.mavg[,c(1:5,9)]
c.mavg=model.average(b.selection,parameter="p")
c.mavg[,c(1:5,9)]
d.mavg=model.average(b.selection)
d.mavg
