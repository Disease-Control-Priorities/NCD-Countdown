###############################################################################################################################
###############################################################################################################################
# Iterate through all interventions by icer order
###############################################################################################################################

rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws)

library(foreach)
library(snow)
library(parallel)
library(iterators)
library(doParallel)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../new_inputs/PreppedData0819.Rda")
source("utils/demmod_icer_rank.R")

# The file for the ICER ranks, ideally stored here as an output of Yoshis code
icer_in  <- fread("output/All_ICER_2030_0830.csv")
icer_in<- icer_in%>%filter(Code<5)
###############################################################################################################################

# Data for all with the NCD4 related 40q30 in 2010 and the target for 2030 (1/3 reduction)

all.locs       <- unique(icer_in$Country)[67]
interventions  <- icer_in %>% pull(Code) %>% unique() %>% sort() 
total          <- length(all.locs)*length(interventions)
sel.cse        <- cse_g %>% pull(cause_name) %>% unique() # using all NCD causes

#df.targetq30.l <- list(length(all.locs))
all.pin.l <- all.dalys.l <- all.q30.l <- dadt.all.l<-df.targetq30.l <- list(total)

parallelCluster <- makeCluster(1,type = "SOCK",methods = FALSE, outfile="log3.txt") #first number is number of cores

setDefaultCluster(parallelCluster)
registerDoParallel(parallelCluster)

# set MRO's MKL threads to 1 and set data.table threads to 1
clusterEvalQ(cl = parallelCluster, {
  #setMKLthreads(1)    # MRO
  pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws)  # make sure we load the package on the cluster
  #setDTthreads(1)     # data.table
})


optim_parallel <- function(is,interventions,all.pin.l, all.dalys.l,all.q30.l,dadt.all.l, df.targetq30.l) {
  k = 1; u = 1
  
  # 40q30 calculations
  # Population numbers for 2010 and 2015, by single age and sex
  pop1015   <- wpp.in %>% filter(location_name == is & year %in% c(2010,2015)) %>% 
    select(year, sex_name, age_name, Nx) %>% spread(year, Nx) %>% 
    arrange(sex_name, age_name) %>% select("2010","2015") %>% as.matrix()
  
  # NCD death rates for 2010 and 2015, by single age and sex
  rncd1015  <- cause.mx %>% filter(location_name == is & year %in% c(2010,2015)) %>% 
    group_by(year, sex_name, age_name) %>% summarise(mxs = sum(mx, na.rm = T), .groups = "drop") %>% 
    spread(year, mxs) %>% 
    arrange(sex_name, age_name) %>% select("2010","2015") %>% as.matrix()
  
  # Calculate deaths by scaling population with rates
  dncd1015  <- rncd1015*pop1015 
  
  # Aggregating to person and estimating 40q30s
  p1015     <- apply(apply(pop1015, 2, combine.ages5), 2, get.pers) # person pops in 5 years ages
  d1015     <- apply(apply(dncd1015, 2, combine.ages5), 2, get.pers)# person deaths in 5 years ages
  r1015     <- d1015/p1015                                          # person rates in 5 years ages
  vq30      <- apply(r1015, 2, get.q30) # These are the 40q30 estimates for 2010 and 2015, respectively
  tq30      <- 2/3*vq30                 # The target is 2/3 reduction - baseline either 2010 or 2015
  
  
  j = 1
  
  inters <- icer_in %>% filter(Country == is & Code %in% interventions) %>% arrange(ICER_rank2) %>% pull(Code)
  
  for (inter in inters){
    
    #setWinProgressBar(pb, k, title=paste0(round(k/total*100,3),"% done"))
    
    if (j == 1){
      inter0 = inter
    } else {
      inter0 = c(inter0, inter)
    }  
    
    projection = project_pop(is, inter0, 0, "yes", sel.cse, "varying", "yes", "no")
    
    all.pin.l[[k]]    = data.table(projection$pin.est)
    all.dalys.l[[k]]  = data.table(projection$dalys)
    all.q30.l[[k]]    = data.table(projection$q30df)
    dadt.all.l[[k]]   = data.table(projection$DAdt)
    
    df.targetq30.l[[k]] <- data.table(location_name = is, 
                                      `40q30_10` = vq30[1], `40q30_15` = vq30[2], 
                                      Target_10 = tq30[1], Target_15 = tq30[2])
    
    # Calculate the NCD4 related 40q30 for 2010 and the 1/3rd reduction by 2030 
    
    j = j + 1; k = k + 1
  }
  return(list(all.pin.l,all.dalys.l,all.q30.l, dadt.all.l, df.targetq30.l))
}


#close(pb)

Sys.time()
everything <- foreach (is = all.locs, .combine ='rbind') %dopar% {
  optim_parallel(is,interventions,all.pin.l, all.dalys.l,all.q30.l, dadt.all.l, df.targetq30.l)
}

Sys.time()

pin.0 <- rbindlist(everything[[1]])
dalys.0 <- rbindlist(everything[[2]])
q30.0 <- rbindlist(everything[[3]])
dadt.0 <- rbindlist(everything[[4]])
target <- unique(rbindlist(everything[[5]]))


## run again for no intersectoral ##

optim_parallel2 <- function(is,interventions,all.pin.l, all.dalys.l,all.q30.l,dadt.all.l, df.targetq30.l) {
  k = 1; u = 1
  
  # 40q30 calculations
  # Population numbers for 2010 and 2015, by single age and sex
  pop1015   <- wpp.in %>% filter(location_name == is & year %in% c(2010,2015)) %>% 
    select(year, sex_name, age_name, Nx) %>% spread(year, Nx) %>% 
    arrange(sex_name, age_name) %>% select("2010","2015") %>% as.matrix()
  
  # NCD death rates for 2010 and 2015, by single age and sex
  rncd1015  <- cause.mx %>% filter(location_name == is & year %in% c(2010,2015)) %>% 
    group_by(year, sex_name, age_name) %>% summarise(mxs = sum(mx, na.rm = T), .groups = "drop") %>% 
    spread(year, mxs) %>% 
    arrange(sex_name, age_name) %>% select("2010","2015") %>% as.matrix()
  
  # Calculate deaths by scaling population with rates
  dncd1015  <- rncd1015*pop1015 
  
  # Aggregating to person and estimating 40q30s
  p1015     <- apply(apply(pop1015, 2, combine.ages5), 2, get.pers) # person pops in 5 years ages
  d1015     <- apply(apply(dncd1015, 2, combine.ages5), 2, get.pers)# person deaths in 5 years ages
  r1015     <- d1015/p1015                                          # person rates in 5 years ages
  vq30      <- apply(r1015, 2, get.q30) # These are the 40q30 estimates for 2010 and 2015, respectively
  tq30      <- 2/3*vq30                 # The target is 2/3 reduction - baseline either 2010 or 2015
  
  
  j = 1
  
  inters <- icer_in %>% filter(Country == is & Code %in% interventions) %>% arrange(ICER_rank2) %>% pull(Code)
  
  for (inter in inters){
  
  #setWinProgressBar(pb, k, title=paste0(round(k/total*100,3),"% done"))
  
  if (j == 1){
    inter0 = inter
  } else {
    inter0 = c(inter0, inter)
  }  
  
  projection = project_pop(is, inter0, 0.025, "no", sel.cse, "varying", "yes", "no")
  
  all.pin.l[[k]]    = data.table(projection$pin.est)
  all.dalys.l[[k]]  = data.table(projection$dalys)
  all.q30.l[[k]]    = data.table(projection$q30df)
  dadt.all.l[[k]]   = data.table(projection$DAdt)
  
  df.targetq30.l[[k]] <- data.table(location_name = is, 
                                    `40q30_10` = vq30[1], `40q30_15` = vq30[2], 
                                    Target_10 = tq30[1], Target_15 = tq30[2])
  
  # Calculate the NCD4 related 40q30 for 2010 and the 1/3rd reduction by 2030 
  
  j = j + 1; k = k + 1
}
return(list(all.pin.l,all.dalys.l,all.q30.l, dadt.all.l, df.targetq30.l))
}


#close(pb)

Sys.time()
everything <- foreach (is = all.locs, .combine ='rbind') %dopar% {
  optim_parallel2(is,interventions,all.pin.l, all.dalys.l,all.q30.l, dadt.all.l, df.targetq30.l)
}

Sys.time()

pin.c <- rbindlist(everything[[1]])
dalys.c <- rbindlist(everything[[2]])
q30.c <- rbindlist(everything[[3]])
dadt.c <- rbindlist(everything[[4]])
target <- unique(rbindlist(everything[[5]]))


stopCluster(parallelCluster)
#end of threadingstopCluster(parallelCluster)
#end of threading
q30.code     <- q30.c %>% filter(year_id == 2030) %>% 
  select(location_name, n.inters, Adjusted, Baseline) %>%
  rename(`40q30_2030_base`=Baseline, `40q30_2030_adj`= Adjusted) %>%
  left_join(target, by = "location_name") %>% distinct()

all.pin <- pin.c %>% 
  select(group, location_name, n.inters, Code, sub_id, cause_name, year_id, Coverage, pin) %>%
  arrange(group, location_name, n.inters, Code, year_id) 

###############################################################################################################################
###############################################################################################################################
#  Reaching or not reaching target by scenario
###############################################################################################################################

q30.sel <- q30.code %>% 
  mutate(Change10 = Target_10 -`40q30_2030_adj`,
         Change15 = Target_15 -`40q30_2030_adj`) %>%
  group_by(location_name) %>% 
  mutate(mChange10 = max(Change10), mChange15 = max(Change15)) %>%
  ungroup() %>% 
  mutate(Reaches10 = ifelse(mChange10 < 0, "No","Yes"),
         Reaches15 = ifelse(mChange15 < 0, "No","Yes"))

q30.sel.no.10 <- suppressWarnings(q30.sel %>% 
                                    filter(Reaches10 == "No") %>% group_by(location_name) %>% 
                                    mutate(mc = max(n.inters)) %>% ungroup() %>% 
                                    filter(n.inters == mc) %>% select(location_name, n.inters) %>%
                                    mutate(Reaches = "no", byear = 2010))

q30.sel.no.15 <- suppressWarnings(q30.sel %>% 
                                    filter(Reaches15 == "No") %>% group_by(location_name) %>% 
                                    mutate(mc = max(n.inters)) %>% ungroup() %>% 
                                    filter(n.inters == mc) %>% select(location_name, n.inters)  %>%
                                    mutate(Reaches = "no", byear = 2015))

q30.sel.yes.10 <- suppressWarnings(q30.sel %>% 
                                     filter(Reaches10 == "Yes" & Change10 >= 0) %>% 
                                     group_by(location_name) %>% mutate(mc = min(n.inters)) %>% ungroup() %>% 
                                     filter(n.inters == mc) %>% select(location_name, n.inters) %>%
                                     mutate(Reaches = "yes", byear = 2010))

q30.sel.yes.15 <- suppressWarnings(q30.sel %>% 
                                     filter(Reaches15 == "Yes" & Change15 >= 0) %>% 
                                     group_by(location_name) %>% mutate(mc = min(n.inters)) %>% ungroup() %>% 
                                     filter(n.inters == mc) %>% select(location_name, n.inters) %>%
                                     mutate(Reaches = "yes", byear = 2015))

q30.CodeDelta <- rbind(q30.sel.no.10, q30.sel.no.15, q30.sel.yes.10, q30.sel.yes.15)  %>%
  arrange(location_name, byear, Reaches)

###############################################################################################################################

all.pin.opt <- pin.c %>% 
  filter(group %in% c("Baseline","Adjusted")) %>%
  right_join(q30.CodeDelta, by = c("location_name","n.inters"))

dalys.opt    <- dalys.c %>%
  right_join(q30.CodeDelta, by = c("location_name","n.inters")) %>% 
  filter(year_id > 2019)

dadt.all.opt <- dadt.c %>%
  right_join(q30.CodeDelta, by = c("location_name","n.inters"))

save(all.pin.opt, dalys.opt, dadt.all.opt, q30.code, q30.sel, q30.CodeDelta, country.lab,
     file = "output/Malawi_all_clinical_0830.Rda")


##############################################################################################
rm(list=ls()) #Remove all
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(kableExtra)
library(reshape2)
library(stringr)

'%!in%' <- function(x,y)!('%in%'(x,y))
tradable_conversion <- function(unit_cost, tradable_ratio, currency, global_inflation, exchange){
  if(currency == "USD") {
    unit_cost*tradable_ratio*global_inflation} else {
      ((unit_cost*tradable_ratio)/exchange)*global_inflation
    }
}

nontradable_conversion <- function(unit_cost,tradable_ratio, currency, cpi_adjust, exchange,country, exchange_end, gni, gni_selected_country){
  if(currency == "USD" & (country == "LIC" | country == "LMIC" | country == "LIC+LMIC+UMIC")) {
    unit_cost*(1-tradable_ratio)*cpi_adjust*(gni_selected_country/gni)
  } else if (currency == "USD" & (country != "LIC" & country != "LMIC" & country != "LIC+LMIC+UMIC")){
    ((unit_cost*exchange)*(1-tradable_ratio))*cpi_adjust/exchange_end*(gni_selected_country/gni)} 
  else {
    ((unit_cost*(1-tradable_ratio)*cpi_adjust)/exchange_end)*(gni_selected_country/gni)
  }
}

load("output/Malawi_all_clinical_0830.Rda")
all.pin.opt<-all.pin.opt%>%filter(byear==2015)
all.pin.opt$unique_id<-paste0("C", all.pin.opt$Code, "_", all.pin.opt$sub_id)
dadt.all.opt<-dadt.all.opt%>%filter(byear==2015)
dalys.opt<-dalys.opt%>%filter(byear==2015)
q30.CodeDelta<-q30.CodeDelta%>%filter(byear==2015)

# Unit cost adjustment
uc <- read.csv("../new_inputs/PINandCosts0625.csv", stringsAsFactors=F)
uc$unique_id <- paste0("C",uc$NCD,"_",uc$sub_id)
uc <- uc %>%filter(Original.Currency != "NA")
# Necessary files
# Exchange rate
exchange <- read_xlsx("../new_inputs/Exchange_rate.xlsx", na = "..")
exchange<-exchange[,c(1:60)]
ex2<-read_xlsx("../new_inputs/Exchange_rate_2020.xlsx", skip=3)
ex2<-ex2[,c(1,61,62)]

exchange<-left_join(exchange, ex2, by="Country")
exchange <- exchange%>%gather(year, value, -Country)%>%group_by(Country)%>%fill(value)%>%spread(year, value)


# Regional inflation
#regional_inflation <- read_xls("../new_inputs/Regional_inflation_20200212.xls", sheet = "Data")
regional_inflation <- read_xls("../new_inputs/Regional_inflation_2020.xls", sheet = "Data", skip=3)
#regional_inflation[,c(5:64)] <- (regional_inflation[,c(5:64)]/100)+1
regional_inflation[,c(5:65)] <- (regional_inflation[,c(5:65)]/100)+1

# COUntry grouping
class <- read.csv("../new_inputs/Country_groupings.csv")

#Country list: only eligible countries
table(class$NCD_region)
country_list <- class %>% 
  filter(World_bank == "LIC" | World_bank == "LMIC"| World_bank == "UMIC") %>% 
  filter(NCD_region %in% c("Latin America and Caribbean",
                           "Central and Eastern Europe",
                           "Central Asia, Middle East and North Africa",
                           "Sub-Saharan Africa",
                           "South Asia",
                           "East and South East Asia",
                           "Oceania")) %>% 
  select(World_bank, NCD_region ,Country, iso3)


# CPI adjusted
cpi <- read.csv("../new_inputs/CPI_WB_2018_added2.csv", stringsAsFactors=F)
cpi<-cpi[,-c(66:71)]
#add 2020 data
cpi2<-read.csv("../new_inputs/CPI_2020.csv", stringsAsFactors=F)
names(cpi2)[1]<-"Country"
cpi2<-cpi2[,c(1,65)]
cpi<-left_join(cpi, cpi2, by="Country")

cpi <- cpi[,c(3,6:66)]%>%gather(year, value, -Country)%>%group_by(Country)%>%fill(value)%>%spread(year, value)

#also add France
#france<-read.csv("../new_inputs/CPI_2020.csv", stringsAsFactors=F)%>%filter(Country.Code=="FRA")
#names(france)[1]<-"Country"
#cpi<-bind_rows(cpi, france[,-c(2,3,4)])

# GNI
GNI <- read.csv("../new_inputs/GNI_WB_USD_2017_re2.csv", stringsAsFactors=F)
GNI<-GNI[,-c(1,4,5,6,66,67,68)]
GNI2 <- read.csv("../new_inputs/GNI_2020.csv", stringsAsFactors=F)
GNI2<-GNI2[,c(1,64,65)]
names(GNI2)[1]<-"Country"

GNI<-left_join(GNI, GNI2, by="Country")
GNI <- GNI%>%gather(year, value, -Country, -Country.Code)%>%group_by(Country.Code)%>%fill(value)%>%spread(year, value)

benefits.opt <- merge(y = dalys.opt, x = class, by.x = "Country", by.y = "location_name")
final_all <- data.frame(Code = character() ,
                        adjusted_uc = numeric(),
                        pin_sum= numeric(),
                        year_id =  numeric(),
                        total_cost = numeric(),
                        senario = character() ,
                        Country = character(),
                        Class = character(),
                        Region = character(),
                        stringsAsFactors=FALSE)


final_all_intervention <- data.frame(Code = character(),
                                     Intervention = character(),
                                     senario = character() ,
                                     Country = character(),
                                     adjusted_uc = numeric(),
                                     pin_sum = numeric(),
                                     total_cost = numeric(),
                                     Region = character(),
                                     stringsAsFactors=FALSE) 




final_all_ICER <- data.frame(Country = character(),
                             year_id = numeric(),
                             Region = character(),
                             sum_increment = numeric(),
                             life_years_gained = numeric(),
                             ICER = numeric()
)

increment_all_year      <- data.frame(
  unique_id = character(),
  year_id = numeric(),
  Class = character(),
  Region = character(),
  adjusted_uc = numeric(),
  base_pin = numeric(),
  base_total = numeric(),
  adjust_pin = numeric(),
  adjust_total = numeric(),
  increment = numeric()
  )

UC_Years<-2020

for (c in 67){
  
  selected_country <- as.character(country_list$Country[c])
  selected_country_code <- as.character(country_list$iso3[c])
  selected_country_category <- as.character(country_list$World_bank[c])
  selected_country_region <- as.character(country_list$NCD_region[c])
  
  country_class <- as.character(country_list[country_list$Country == selected_country,]$World_bank) 
  # Adjusted UC install
  # Adjustment database
  no <- nrow(uc)
  
  file.name <- paste0("../new_demography/output/costs/bycountry/",selected_country,"_adjusted_uc_",UC_Years,".csv")
  adjusted_uc <- read.csv(file.name)
  
  # Adjusted UC end 
  
  result.df <- adjusted_uc %>% select(unique_id)
  result.df2 <- adjusted_uc %>% select(unique_id)
  
  
  # full data -----------------------
  country.pop <- all.pin.opt %>% 
    filter(location_name == selected_country)
  
  full.data <- merge.data.frame(x = country.pop, y = adjusted_uc[,c("unique_id", "Original.Unit.Cost", "Cost.adjustment", "Traded", "tradable_uc", "nontradable_uc", "adjusted_uc" )], by = "unique_id", all.x = T)
  
  full.data$total_cost <- full.data$pin * full.data$adjusted_uc
  
  #file.name <- paste0("Result_0706/Optimal/All_pop_cost/",selected_country,"_all_by_intervention_year",".csv")
  #write.csv(full.data, file.name)
  
  
  # Baseline population in need------------------
  for (y in 3:10){
    
    ##PIN by intervention
    # pin_group = all code
    pin_group <- all.pin.opt %>% 
      filter(location_name == selected_country,
             year_id == 2020 + y, 
             group == "Baseline") %>% 
      group_by(unique_id) %>% 
      summarize(pin_sum = sum(pin))
    
    pin_group$year_id <- 2020 + y
    
    uc_pin <- merge.data.frame(adjusted_uc, pin_group, by = "unique_id")
    
    uc_pin <- uc_pin %>% 
      mutate(total_cost = adjusted_uc * pin_sum)
    
    uc_pin_all <- if(y == 3) {
      uc_pin
    } else {
      rbind(uc_pin_all, uc_pin)
    }
    
    result.df <- merge(result.df, uc_pin[,c("unique_id","total_cost")], by = "unique_id")
    
    label <- paste0("cost_",2020+y)
    names(result.df)[ which( names(result.df)=="total_cost" ) ] <- label
  }
  
  
  # By Interventions
  final.df1_int <- result.df
  final.df1_int$senario <- "Baseline"
  final.df1_int$Country <- selected_country
  final.df1_int$Class <- country_class
  final.df1_int$Region <- selected_country_region
  
  final.df1_int$total_cost <- apply(final.df1_int[,2:9],1,sum)
  
  # Add unit cost
  final.df1_int <- merge(final.df1_int, uc_pin[,c("unique_id", "adjusted_uc")] , by = "unique_id")
  
  # Add pin
  pin_code <- all.pin.opt %>% 
    filter(location_name == selected_country,
           group == "Baseline") %>% 
    group_by(unique_id) %>% 
    summarize(pin_sum = sum(pin))
  
  final.df1_int <- merge(final.df1_int, pin_code[,c("unique_id", "pin_sum")], by = "unique_id")
  
  # final.df1_int <- final.df1_int %>%
  #   select(unique_id, senario, Country, adjusted_uc, pin_sum ,total_cost, Region)
  # 
  
  # years
  result.df1_long <- uc_pin_all
  
  result.df1_long$senario <- "Baseline"
  result.df1_long$Country <- selected_country
  result.df1_long$Class <- country_class
  result.df1_long$Region <- selected_country_region
  
  
  
  
  
  #Adjustment-------------
  
  for (y in 3:10){
    #PIN by intervention
    
    # pin_group = all code
    pin_group <- all.pin.opt %>% 
      filter(location_name == selected_country,
             year_id == 2020 + y, 
             group == "Adjusted") %>% 
      group_by(unique_id) %>% 
      summarize(pin_sum = sum(pin))
    
    pin_group$year_id <- 2020 + y
    
    uc_pin <- merge.data.frame(adjusted_uc, pin_group, by = "unique_id")
    
    uc_pin <- uc_pin %>% 
      mutate(total_cost = adjusted_uc * pin_sum)
    
    uc_pin_all <- if(y == 3) {
      uc_pin
    } else {
      rbind(uc_pin_all, uc_pin)
    }
    
    result.df2 <- merge(result.df2, uc_pin[,c("unique_id","total_cost")], by = "unique_id")
    
    label <- paste0("cost_",2020+y)
    names(result.df2)[ which( names(result.df2)=="total_cost" ) ] <- label
    
  }
  
  # By intervention & save
  
  final.df2_int <- result.df2
  final.df2_int$senario <- "Adjusted"
  final.df2_int$Country <- selected_country
  final.df2_int$Class <- country_class
  final.df2_int$Region <- selected_country_region
  
  final.df2_int$total_cost <- apply(final.df2_int[,2:9],1,sum)
  
  # Add unit cost
  final.df2_int <- merge(final.df2_int, uc_pin[,c("unique_id", "adjusted_uc")])
  
  # Add pin
  pin_code <- all.pin.opt %>% 
    filter(location_name == selected_country, 
           group == "Adjusted") %>% 
    group_by(unique_id) %>% 
    summarize(pin_sum = sum(pin))
  
  final.df2_int <- merge(final.df2_int, pin_code[,c("unique_id", "pin_sum")])
  
  # 
  #  final.df2_int <- final.df2_int %>% 
  #    select(Code, senario, Country, adjusted_uc, pin_sum ,total_cost, Region)
  
  final_country_tc_int <- rbind(final.df1_int,final.df2_int)
  
  
  #Final dataset by interventions-------------------
  file.name <- paste0("Result_0706/Optimal/Total_cost_intervention/",selected_country,"_total_cost_2030_intervention",".csv")
  #write.csv(final_country_tc_int, file.name)
  
  final_all_intervention <- rbind(final_all_intervention, final_country_tc_int)
  #--------------------------------------------------
  
  # By year
  result.df2_long <- uc_pin_all
  
  result.df2_long$senario <- "Adjusted"
  result.df2_long$Country <- selected_country
  result.df2_long$Class <- country_class
  result.df2_long$Region <- selected_country_region
  
  # Save
  final_country_tc3 <- rbind(result.df1_long,result.df2_long)
  file.name <- paste0("Result_0706/Optimal/Total_cost_year/",selected_country,"_total_cost_2030",".csv")
  #write.csv(final_country_tc3, file.name)
  
  #Final dataset-------------------
  final_all <- rbind(final_all, final_country_tc3)
  
  # ----------------------
  # incremental cost by year and intervention
  base_result <- result.df1_long %>% 
    filter(senario == "Baseline")%>% 
    select(unique_id, Country, Class, Region, year_id, adjusted_uc, pin_sum, total_cost) %>% 
    rename(base_pin = pin_sum,
           base_total = total_cost) 
  
  adjust_result <- result.df2_long %>%
    filter(senario == "Adjusted") %>% 
    select(unique_id, year_id, pin_sum, total_cost) %>% 
    rename(adjust_pin = pin_sum,
           adjust_total = total_cost) 
  
  
  increment_by_year <- merge(base_result, adjust_result , by = c("unique_id", "year_id"))
  
  increment_by_year$incremental <- increment_by_year$adjust_total - increment_by_year$base_total
  
  file.name <- paste0("Result_0706/Optimal/incremental_cost_year/",selected_country,"_increment_cost_2020_2030",".csv")
  #write.csv(increment_by_year, file.name)
  
  increment_all_year <- rbind(increment_all_year, increment_by_year)
  
  country.all.id <- colsplit(increment_by_year$unique_id,"_",names=c("Code","sub_id"))
  # From unique_id to Code
  increment_by_year$Code <- country.all.id$Code
  increment_by_year$sub_id <- country.all.id$sub_id
  
  
  country.all.sum <- increment_by_year %>% 
    group_by(Country, Region, year_id) %>% 
    summarize(sum_increment = sum(incremental))
  
  
  # Benefit
  benefits.country <- benefits.opt %>% 
    filter(Country == selected_country) 
  
  
  country.all2 <- merge(x = country.all.sum, y = benefits.country[,c("Country", "year_id", "DALY.ave")], by = c("Country", "year_id"),all.x = T)
  
  
  country.all2 <- country.all2 %>% 
    mutate(ICER = sum_increment / DALY.ave
    )
  
  file.name <- paste0("output/",selected_country,"_ICER_2020_2030",".csv")
  #write.csv(country.all2, file.name)
  
  final_all_ICER <- rbind(final_all_ICER, country.all2)
  
}

totalcost<-unique(final_all_intervention)%>%group_by(senario)%>%
  summarise(sum_cost=sum(total_cost))

dadt.all.opt%>%filter(byear==2015)%>%summarise(DA=sum(Deaths.Avert))%>%pull(DA)
dalys.opt%>%filter(byear==2015)%>%summarise(DA=sum(DALY.ave))%>%pull(DA)

deaths_opt<-dadt.all.opt%>%filter(location_name=="Malawi", byear==2015)%>%group_by(year_id)%>%summarise(DA = sum(Deaths.Avert))
dalys_opt<-dalys.opt%>%filter(location_name=="Malawi", byear==2015)%>%group_by(year_id)%>%summarise(DALY.ave = sum(DALY.ave))
dalys_opt$year_id<-as.numeric(dalys_opt$year_id)

write.csv(left_join(dalys_opt, deaths_opt), "Figures/Malawi_onlyclinical_impact_0830.csv")

names<-read.csv("../new_inputs/names.csv", stringsAsFactors = F)
costs<-final_all_intervention%>%mutate(Code = substr(unique_id,1,4))%>%
  select(-c(unique_id,Country, Class, Region, total_cost, adjusted_uc, pin_sum))%>%
  gather(year, cost, -senario, -Code)%>%group_by(year, Code, senario)%>%summarise(cost=sum(cost))%>%
  spread(year, cost)

costs<-left_join(costs, names)

write.csv(costs, "Figures/Malawi_onlyclinical_costs_0830.csv")

q30_opt<-q30.sel%>%filter(location_name=="Malawi")


#
