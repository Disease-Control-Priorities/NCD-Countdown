library(readstata13)
library(foreign)
library(dplyr)
library(readxl)

dat <- read.dta13("dths_20age.dta")
#the file above is not included in Github because it is too large

#only take NCD causes (+all cause) for years 2010-2019
causes<-read_excel("new_inputs/GBDtoGHE.xlsx", sheet=1)
dat<-dat%>%filter(causename %in% unique(causes$ghe))
unique(dat$causename)

#convert to GBD column names and sex/age IDs
dat$sex[dat$sex==1]<-"Male"
dat$sex[dat$sex==2]<-"Female"

names(dat)[7]<-"cause"
names(dat)[8]<-"val"
names(dat)[9]<-"lower"
names(dat)[10]<-"upper"

#compare to GBD all ages, all cause death rate for females
#should be around 670
dat%>%filter(sex=="Female", year==2019, ghecause==0)%>%summarise(rate=sum(val)/sum(pop))*100000

#convert to rates pe 100,000
dat$val<-(dat$val/dat$pop)*100000
dat$lower<-(dat$lower/dat$pop)*100000
dat$upper<-(dat$upper/dat$pop)*100000

dat$metric<-"Rate"
dat$measure<-"Deaths"

dat$age<-paste0(dat$age, " to ", dat$age+4)
dat$age[dat$age=="0.1 to 4.1"]<-"Neonatal"
dat$age[dat$age=="0.11 to 4.11"]<-"1-11 months"
dat$age[dat$age=="85 to 89"]<-"85 plus"

unique(dat$age)

#write to new data folder
for (i in 2010:2019){
write.csv(dat%>%filter(year==i), 
          paste0("new_inputs/ghedata",i,".csv"), row.names = F)
}
