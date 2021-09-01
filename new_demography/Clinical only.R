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

all.locs       <- unique(icer_in$Country)
interventions  <- icer_in %>% pull(Code) %>% unique() %>% sort() 
total          <- length(all.locs)*length(interventions)
sel.cse        <- cse_g %>% pull(cause_name) %>% unique() # using all NCD causes

#df.targetq30.l <- list(length(all.locs))
all.pin.l <- all.dalys.l <- all.q30.l <- dadt.all.l<-df.targetq30.l <- list(total)

parallelCluster <- makeCluster(32,type = "SOCK",methods = FALSE, outfile="log3.txt") #first number is number of cores

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
    
    projection = project_pop(is, inter0, 0.025, "no", sel.cse, "varying", "yes", "yes", 1)
    
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


all.pin <- rbindlist(everything[[1]])

for (i in 2:123){
  temp<-rbindlist(everything[[i]])
  all.pin<-rbind(all.pin, temp)
}

all.dalys <- rbindlist(everything[[124]])

for (i in 125:246){
  temp<-rbindlist(everything[[i]])
  all.dalys<-rbind(all.dalys, temp)
}

all.q30 <- rbindlist(everything[[247]])

for (i in 248:369){
  temp<-rbindlist(everything[[i]])
  all.q30<-rbind(all.q30, temp)
}

dadt.all <- rbindlist(everything[[370]])

for (i in 371:492){
  temp<-rbindlist(everything[[i]])
  dadt.all<-rbind(dadt.all, temp)
}

df.targetq30 <- unique(rbindlist(everything[[493]]))

for (i in 494:615){
  temp<-unique(rbindlist(everything[[i]]))
  df.targetq30<-rbind(df.targetq30, temp)
}

q30.code     <- all.q30 %>% filter(year_id == 2030) %>% 
  select(location_name, n.inters, Adjusted, Baseline) %>%
  rename(`40q30_2030_base`=Baseline, `40q30_2030_adj`= Adjusted) %>%
  left_join(df.targetq30, by = "location_name") %>% distinct()

all.pin <- all.pin %>% 
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

all.pin.opt <- all.pin %>% 
  filter(group %in% c("Baseline","Adjusted")) %>%
  right_join(q30.CodeDelta, by = c("location_name","n.inters"))

dalys.opt    <- all.dalys %>%
  right_join(q30.CodeDelta, by = c("location_name","n.inters")) %>% 
  filter(year_id > 2019)

dadt.all.opt <- dadt.all %>%
  right_join(q30.CodeDelta, by = c("location_name","n.inters"))

save(all.pin.opt, dalys.opt, dadt.all.opt, q30.code, q30.sel, q30.CodeDelta, country.lab,
     file = "output/clinical_output_0830.Rda")


stopCluster(parallelCluster)
#end of threadingstopCluster(parallelCluster)
#end of threading

###############################################################################################################################
rm(list=ls()) 

load("../new_inputs/PreppedData0819.Rda")
pop<-wpp.in%>%filter(year==2019)%>%group_by(location_name)%>%summarise(pop=sum(Nx))
###### results for paper ############
load("output/Optim.Out0830.Rda")
inters<-q30.CodeDelta%>%filter(byear==2015)%>%rename(int.req = n.inters)
ref_q30<-left_join(q30.code, inters, by="location_name")%>%mutate(check=int.req-n.inters)%>%
  filter(check==0)

load("output/clinical_output_0830.Rda")
inters2<-q30.CodeDelta%>%filter(byear==2015)%>%rename(int.req = n.inters)
intersectoral_q30<-left_join(q30.code, inters2, by="location_name")%>%mutate(check=int.req-n.inters)%>%
  filter(check==0)

#proportion of countries achieving?

#reference
length(inters%>%filter(Reaches=="yes")%>%pull(Reaches))/123
#intersectoral only
length(inters2%>%filter(Reaches=="yes")%>%pull(Reaches))/123

#change in 40q30
ref_q30<-left_join(ref_q30, pop, by="location_name")
intersectoral_q30<-left_join(intersectoral_q30, pop, by="location_name")

weighted.mean(ref_q30$`40q30_15`, ref_q30$pop)
weighted.mean(ref_q30$`40q30_2030_base`, ref_q30$pop)
weighted.mean(ref_q30$`40q30_2030_adj`, ref_q30$pop)
weighted.mean(intersectoral_q30$`40q30_2030_adj`, intersectoral_q30$pop)

## calculate costs ##
