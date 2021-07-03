library(readxl)
library(vroom)
library(dplyr)
library(pdftools)
library(tidyverse)
library(stringr)

#################
# SALT #
#################

int<-read_excel("new_inputs/source data/NCD ccs2019-sodium best buys.xlsx")
sodium.intake<-vroom("new_inputs/source data/Adults (age 25+ years)_ Estimated per capita sodium intake_4-3-2021 11.50.csv", 
                     col_select = c("AreaID", "AreaName", "DataValue"))%>%rename(iso = AreaID, NAintake = DataValue)

#correcting error in JHU data w/ GBD 2019 estimate (not public yet)
sodium.intake$NAintake[sodium.intake$AreaName=="China"]<-6.954027

int<-left_join(int, sodium.intake, by="iso")

#efficacy values from literature
reform_mand<-0.2
reform_vol<-0.15
#LA study, but also similar to 20% redux on meals outside home (assuming
#roughly 1/3 of meals are eaten outside the home...so 20%/3 ~= 7%)
sup_env<-0.07
FOPL<-0.1
media<-0.05

#change NAs to no
int$`salt policy: product reform`[is.na(int$`salt policy: product reform`) | 
                                    int$`salt policy: product reform`=="don't know"]<-"no"
int$`salt policy: FOPL`[is.na(int$`salt policy: FOPL`)| 
                        int$`salt policy: FOPL`=="don't know"]<-"no"
int$`salt policy: public awareness pgm`[is.na(int$`salt policy: public awareness pgm`)| 
                                        int$`salt policy: public awareness pgm`=="don't know"]<-"no"
int$`salt policy: reg salt content`[is.na(int$`salt policy: reg salt content`)| 
                                   int$`salt policy: reg salt content`=="don't know"]<-"no"

#start all no/no countries at voluntary?
int$`salt policy: enforcement`[is.na(int$`salt policy: enforcement`)]<-"voluntary"
int$reformRRR<-NA

for (i in 1:194){

  if(int$`salt policy: product reform`[i]=="no" & 
     int$`salt policy: enforcement`[i]=="mandatory"){
    int$reformRRR[i]<-reform_mand
  }
  
  #assume don't know ~ voluntary
  else if (int$`salt policy: product reform`[i]=="no" &
           int$`salt policy: enforcement`[i]=="voluntary" | 
           int$`salt policy: enforcement`[i]=="don't know"){
    int$reformRRR[i]<-reform_vol
  }
  
  #bump countries at voluntary up to mandatory? no.
  #else if (int$`salt policy: product reform`[i]=="yes" &
  #         int$`salt policy: enforcement`[i]=="voluntary"){
  #  int$reformRRR[i]<-(reform_mand-reform_vol)*pkg_food
  #}
  
  else{
    int$reformRRR[i]<-0
  }
    
}

int$envRRR<-NA
int$foplRRR<-NA
int$mediaRRR<-NA

for (i in 1:194){
  
  if(int$`salt policy: reg salt content`[i]=="no"){
    int$envRRR[i]<-sup_env
  }
  
  else{
    int$envRRR[i]<-0
  }
  
  if(int$`salt policy: FOPL`[i]=="no"){
    int$foplRRR[i]<-FOPL
  }
  
  else{
    int$foplRRR[i]<-0
  }
  
  if(int$`salt policy: public awareness pgm`[i]=="no"){
    int$mediaRRR[i]<-media
  }
  
  else{
    int$mediaRRR[i]<-0
  }
}

#calculate number of g sodium reduced
int$reduced<-int$NAintake*(int$reformRRR+int$envRRR+int$foplRRR+int$mediaRRR)
int$target<-int$NAintake-int$reduced

int$reduxadj<-NA
#if target below 3, adjusted sodium reducfion
for (i in 1:194){
  if (int$target[i]<3 & !is.na(int$target[i])){
    int$reduxadj[i]<-int$NAintake[i]-3
  }
  
  else if (is.na(int$target[i])){
    int$reduxadj[i]<-NA
  }
  else{
    int$reduxadj[i]<-int$reduced[i]
  }
}

int$reduxadj[int$reduxadj<0]<-0

#mortality impact based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7601012/
int$mort.redux<-(1-(1/1.06))*int$reduxadj

names(int)[1]<-"iso3"

#write.csv(int%>%select(c(iso3, country, mort.redux)), "new_inputs/salt_effects.csv", row.names = F)

################
# TRANS FATS #
################

#efficacy based on this paper: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4581646/
tfa_eff<-0.297

#scraping data from PDF pages 31 to 41 (Annex 1)
#https://rstudio-pubs-static.s3.amazonaws.com/415060_553527fd13ed4f30aae0f1e4483aa970.html
#but data is not in rows so this solution isn't ideal. whyyyyyy?!

a<-pdf_text("new_inputs/source data/WHO TFA report.pdf") %>%
  readr::read_lines()

a<-a[-c(1:1290, 1911:1962)]

all_stat_lines <- a[7:620] %>%
  str_squish() %>%
  strsplit(split = " ")# remove empty spaces


#ended up using this: https://www.adobe.com/acrobat/online/pdf-to-excel.html
tfa<-read_excel("new_inputs/source data/tfa_coverage.xlsx", skip=1 )
tfa$Score[is.na(tfa$Score)]<-0
tfa<-na.omit(tfa)

chd<-read.csv("new_inputs/source data/tfa_chd.csv", stringsAsFactors = F)

tfa<-bind_cols(tfa, chd)

tfa$mort.redux<-NA

tfa$mort.redux[tfa$Score==4]<-0
tfa$mort.redux[tfa$Score==3]<-tfa_eff*tfa$CHD[tfa$Score==3]/100
tfa$mort.redux[tfa$Score<3]<-tfa$CHD[tfa$Score<3]/100

#update country names
countries<-read.csv("new_inputs/Country_groupings_extended.csv", stringsAsFactors = F)
names(countries)
names(countries)[3]<-"gbd2017"
names(countries)[4]<-"WB2015"
names(countries)[5]<-"WHO"

full<-countries%>%gather("source", "country", -Super_region, -Region, -NCD_region, -SDI, -World_bank_2015, -iso3, -LocID)%>%
  select(c(source, country, iso3, LocID))
unique<-na.omit(unique(full%>%select(c(country, iso3, LocID))))

#write.csv(unique, "new_inputs/allcountryspellings.csv", row.names = F)

tfa<-left_join(tfa, unique%>%rename(Country=country), by="Country")

nas<-data.frame(
    country=tfa$Country[is.na(tfa$iso3)],
    iso3=c("SMR", "MCO", "VCT", 
           "KNA", "NRU", "MKD",
           "TUV", "PRK", "PLW",
           "NIU", "COK", "CPV"),
    LocID=NA
)

unique<-bind_rows(nas, unique)

tfa<-left_join(tfa%>%select(-iso3), unique%>%rename(Country=country)%>%select(c(Country, iso3)), by="Country")

any(is.na(tfa$iso3))


#write.csv(unique, "new_inputs/allcountryspellings.csv", row.names = F)
#write.csv(tfa%>%select(c(iso3, Country, mort.redux)), "new_inputs/tfa_effects.csv", row.names = F)


#################
#add to tobacco and alcohol data and just take NCD4 causes
################
tob<-read.csv("Input_Data/tobaccoandalcohol_efficacy5.csv", stringsAsFactors = F)
tob<-left_join(tob, unique%>%rename(Country=country)%>%select(Country, iso3), by="Country")

which(is.na(tob$iso3))

add<-data.frame(country="Taiwan", iso3="TWN", LocID=NA)
unique<-bind_rows(unique, add)
#write.csv(unique, "new_inputs/allcountryspellings.csv", row.names = F)

tob<-read.csv("Input_Data/tobaccoandalcohol_efficacy5.csv", stringsAsFactors = F)
tob<-left_join(tob, unique%>%rename(Country=country)%>%select(Country, iso3), by="Country")

any(is.na(tob$iso3))

#check that all countries have full data for risk factors?
tlocs<-data.frame(iso3=unique(tob$iso3), tanda="yes")
slocs<-data.frame(iso3=unique(int$iso3), salt="yes")
tfalocs<-data.frame(iso3=unique(tfa$iso3), tfa="yes")

check<-full_join(tlocs, slocs)
check<-full_join(check, tfalocs)

##default to tobacco and alcohol full list
names(int)[20]<-"Mortality.reduction"
int$Outcome<-"Hypertensive heart disease"
int$Risk<-"Salt"
int$NCD4<-"yes"
int$self_harm<-"no"
int$NCD.cause.grouping<-"All other cardiovascular diseases"

names(tfa)[4]<-"Mortality.reduction"
tfa$Outcome<-"Ischemic heart disease"
tfa$Risk<-"Trans fat"
tfa$NCD4<-"yes"
tfa$self_harm<-"no"
tfa$NCD.cause.grouping<-"Ischaemic heart disease"

salt<-left_join(tlocs, int%>%select(c(iso3, Mortality.reduction, Outcome, Risk, NCD4, self_harm, NCD.cause.grouping)), by="iso3")%>%select(-c(tanda))
tfa<-left_join(tlocs, tfa%>%select(c(iso3, Mortality.reduction, Outcome, Risk, NCD4, self_harm, NCD.cause.grouping)), by="iso3")%>%select(-c(tanda))

write.csv(salt, "new_inputs/salt_policy_effects.csv", row.names = F)
write.csv(tfa, "new_inputs/tfa_policy_effects.csv", row.names = F)


##check that excluding NCD4 causes and selfharm and adding CKD due to diabetes gives you the correct tobacco and alcohol Outcomes
causes<-read_excel("new_inputs/GBDtoGHE.xlsx")

which(!unique(tob$Outcome[tob$NCD4=="yes"])%in%unique(causes$gbd))
unique(tob$Outcome[tob$NCD4=="yes"])[8] #"Esophageal cancer"
unique(tob$Outcome[tob$NCD4=="yes"])[22] #PAD - which we removed 

tob<-read.csv("Input_Data/tobaccoandalcohol_efficacy5.csv", stringsAsFactors = F)
tob$Outcome[tob$Outcome=="Oesophageal cancer"]<-"Esophageal cancer"
tob$NCD4[tob$Outcome=="Peripheral artery disease"]<-"no"

write.csv(tob, "new_inputs/tobaccoandalcohol_efficacy6.csv", row.names = F)
