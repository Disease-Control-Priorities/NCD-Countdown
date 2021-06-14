library(readstata13)

dat <- read.dta13("dths_20age.dta")

dat2019<-dat%>%filter(year==2019)

write.csv(dat2019, "GHE2019.csv", row.names = F)

causelist<-unique(dat2019$causename)


efficacy<-read_excel("EfficacyData6.xlsx", sheet=1, na="NA")

L4<-na.omit(unique(efficacy$Level4))
L3<-na.omit(unique(efficacy$Level3[is.na(efficacy$Level4)]))

tobandalc<-read.csv("tobaccoandalcohol_efficacy5.csv", stringsAsFactors = F)

tobcauses<-na.omit(unique(tobandalc$Outcome))

oldcausenames<-unique(c(L4, L3, tobcauses))

#should be same as "Cause groupings file"
cg<-read.csv("Cause_groupings.csv", stringsAsFactors = F)
cglist<-unique(cg$GBD.causes)

which(!oldcausenames%in%cglist)
oldcausenames[67]

oldcausenames<-unique(c(oldcausenames, cglist))

#maybe exclude LRIs and TB related impacts since this is NCD paper?
#also issue with assualt (GBD PAFs are for specific assualt mechanisms e.g. "assualt by firearm" vs. "assualt by sharp object")
#might need to make some simplifying assumptions about assualt impacts since GHE data only has



which(!oldcausenames%in%causelist)
#most of them :/

#write out as cvs and match manually

write.csv(oldcausenames, "gbdcauses.csv")
write.csv(causelist, "ghecauses.csv")

