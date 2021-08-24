###############################################################################################################################
###############################################################################################################################
# Iterate through all interventions by icer order
###############################################################################################################################

rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../new_inputs/PreppedData.Rda")
source("utils/demmod_icer_rank.R")

# The file for the ICER ranks, ideally stored here as an output of Yoshis code
icer_in  <- fread("output/All_ICER_2030.csv")

###############################################################################################################################

# Data for all with the NCD4 related 40q30 in 2010 and the target for 2030 (1/3 reduction)

all.locs       <- unique(icer_in$Country)
interventions  <- icer_in %>% pull(Code) %>% unique() %>% sort() 
total          <- length(all.locs)*length(interventions)
sel.cse        <- cse_g %>% pull(cause_name) %>% unique() # using all NCD causes

df.targetq30.l <- list(length(all.locs))
all.pin.l      <- all.dalys.l <- all.q30.l <- dadt.all.l <- list(total)

pb             <- winProgressBar(title = "progress bar", min = 0,max = total, width = 300)
k = 1; u = 1
for (is in all.locs){

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
  
  df.targetq30.l[[u]] <- data.table(location_name = is, 
                                    `40q30_10` = vq30[1], `40q30_15` = vq30[2], 
                                    Target_10 = tq30[1], Target_15 = tq30[2])
  u = u + 1
  
  j = 1
  inters <- icer_in %>% filter(Country == is & Code %in% interventions) %>% arrange(ICER_rank) %>% pull(Code)

  for (inter in inters){
    
    setWinProgressBar(pb, k, title=paste0(round(k/total*100,3),"% done"))
    
    if (j == 1){
      inter0 = inter
    } else {
      inter0 = c(inter0, inter)
    }  
    
    projection = project_pop(is, inter0, 0.02, "yes", sel.cse, "varying", "yes", "no")
    
    all.pin.l[[k]]    = data.table(projection$pin.est)
    all.dalys.l[[k]]  = data.table(projection$dalys)
    all.q30.l[[k]]    = data.table(projection$q30df)
    dadt.all.l[[k]]   = data.table(projection$DAdt)
    
    # Calculate the NCD4 related 40q30 for 2010 and the 1/3rd reduction by 2030 
    
    j = j + 1; k = k + 1
  }
}
close(pb)

df.targetq30 <- rbindlist(df.targetq30.l)
all.pin      <- rbindlist(all.pin.l)
all.dalys    <- rbindlist(all.dalys.l)
all.q30      <- rbindlist(all.q30.l)
dadt.all     <- rbindlist(dadt.all.l) 

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
     file = "output/Optim.Out.Rda")

###############################################################################################################################
###############################################################################################################################
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 
# Appendix Table

load("output/Optim.Out.Rda")
regs = c("Latin America and Caribbean","Central and Eastern Europe",
         "Central Asia, Middle East and North Africa","Sub-Saharan Africa",
         "South Asia","East and South East Asia","Oceania","All low- and middle-income countries")

count_g <- fread("../new_inputs/Country_groupings.csv") %>%
  right_join(country.lab, by = "iso3")

q30.reaches <- q30.CodeDelta %>% 
  left_join(count_g %>% select(location_name, NCD_region), by = "location_name")

q30.reaches <- q30.reaches %>% mutate(NCD_region = "All low- and middle-income countries") %>%
  rbind(q30.reaches) %>% mutate(NCD_region = factor(NCD_region, levels = regs)) %>%
  arrange(NCD_region)

q30.summary1 <- q30.reaches %>% mutate(CodeDelta = as.numeric(n.inters)) %>%  
  group_by(NCD_region, byear) %>% 
  summarise(MinInts = min(CodeDelta), MedInt = median(CodeDelta), .groups = "drop") %>% 
  ungroup()

q30.summary1 <- q30.reaches %>% 
  mutate(Count = 1) %>%
  group_by(NCD_region, Reaches, byear) %>% summarise(Count = sum(Count), .groups = "drop") %>% ungroup() %>% 
  spread(Reaches, Count) %>% 
  mutate(no = ifelse(is.na(no), 0, no), yes = ifelse(is.na(yes), 0, yes)) %>% 
  right_join(q30.summary1, by = c("NCD_region","byear")) %>% 
  arrange(NCD_region, byear) 

da <- dadt.all.opt %>% mutate(Deaths.Avert = ifelse(Deaths.Avert < 0, 0, Deaths.Avert)) %>%
  left_join(count_g %>% select(Country, NCD_region) %>% rename(location_name = Country), by = "location_name") %>% 
  group_by(age_group, NCD_region, byear) %>% summarise(Deaths.Avert = sum(Deaths.Avert), .groups = "drop") %>% ungroup()

da <- da %>% mutate(NCD_region = "All low- and middle-income countries") %>%
  group_by(age_group, NCD_region, byear) %>% summarise(Deaths.Avert = sum(Deaths.Avert), .groups = "drop") %>% ungroup() %>%
  rbind(da) %>% mutate(NCD_region = factor(NCD_region, levels = regs)) %>% arrange(NCD_region) %>% 
  mutate(Deaths.Avert = 10*round(ceiling(Deaths.Avert/10000))) %>%
  spread(age_group, Deaths.Avert)

save(q30.summary1, da, file = "output/Optim.Out.Tables.Rda")

