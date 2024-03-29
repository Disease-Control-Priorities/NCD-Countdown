###############################################################################################################################
# AROC analysis
###############################################################################################################################

rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../new_inputs/PreppedData0803.Rda")
source("utils/demmod_icer_rank.R")

######################################################################

all.locs      <- countries[c(1:85,87:124)]
causes        <- cse_g %>% 
   filter(ncd_cause %!in% c("All Causes")) %>% 
   pull(ncd_cause) %>% unique()
total         <- length(all.locs)*length(causes)

#####################################################################################################

ts_40q30 <- data.table(location_name = character(), sex_name = character(), 
                       ncd_cause = character(), year_id =integer(), 
                       Baseline=integer(), Adjusted=integer())

ts_pc    <- data.table(location_name = character(), sex_name = character(), byear = integer(),
                       ncd_cause = character(), Baseline=integer(), Adjusted=integer())

Da <- Db <- Pa <- Pb <- array(dim = c(length(all.locs), length(causes), 172, 12))

##############################################################################################################
pb <- winProgressBar(title = "progress bar", min = 0,  max = total, width = 300)

k = 1
r = 1
for (is in all.locs){
   c = 1
   
   for (cause in causes){
   setWinProgressBar(pb, k, title=paste0(round(k/total*100,3),"% done"))
   
   cause2  <-   cse_g  %>% filter(ncd_cause == cause) %>% pull(cause_name) %>% unique()
   
   ###########################################################################################################
   # 40q30 calculations
   # Population numbers for 2010 and 2015, by single age and sex
   pop1015   <- wpp.in %>% filter(location_name == is & year %in% c(2010,2015)) %>% 
      select(year, sex_name, age_name, Nx) %>% spread(year, Nx) %>% 
      arrange(sex_name, age_name) %>% select("2010","2015") %>% as.matrix()
   
   # NCD death rates for 2010 and 2015, by single age and sex
   rncd1015  <- cause.mx %>% filter(location_name == is & year %in% c(2010,2015) & cause_name %in% cause2) %>% 
      group_by(year, sex_name, age_name) %>% summarise(mxs = sum(mx, na.rm = T), .groups = "drop") %>% 
      spread(year, mxs) %>% 
      arrange(sex_name, age_name) %>% select("2010","2015") %>% as.matrix()
   
   # Calculate deaths by scaling population with rates
   dncd1015  <- rncd1015*pop1015 
   
   # Aggregating to person and estimating 40q30s
   p1015     <- apply(pop1015, 2, combine.ages5) # sex specific pops in 5 years ages
   d1015     <- apply(dncd1015, 2, combine.ages5)# sex specific deaths in 5 years ages
   r1015     <- d1015/p1015                      # sex specific rates in 5 years ages
   
   # reorder, F2010, F2015, M2010, M2015
   r1015_2   <- cbind(r1015[1:19,1], r1015[1:19,2], r1015[20:38,1],r1015[20:38,2])
   vq30      <- .01*apply(r1015_2, 2, get.q30) # 40q30 estimates Female 2010 and 2015, Male 2010 and 2015, respectively

   q30.start <- data.table(location_name = is, 
                           sex_name = c("Female","Female", "Male", "Male"),
                           year_id  = c(2010, 2015, 2010, 2015),
                           Adjusted = vq30,
                           Baseline = vq30,
                           ncd_cause= cause)
   ###########################################################################################################
   
   inter   <-   int.df %>% filter(location_name == is & cause_name %in% cause2) %>% 
      pull(Code) %>% unique() %>% sort()
      
      proj        <- project_pop(is, inter, 0.90, "yes", cause2, "fixed", "no", "no")
      ltcause_out <- proj$ltcause %>% mutate(ncd_cause = cause)
      Pb[r,c,,]   <- proj$P0
      Pa[r,c,,]   <- proj$P1
      Db[r,c,,]   <- proj$D0cse
      Da[r,c,,]   <- proj$D1cse
      ts_40q30    <- rbind(ts_40q30, q30.start, ltcause_out)
      
         dfreg       <- ltcause_out %>% rbind(q30.start) %>%
            filter(year_id %in% c(2010,2015,2030)) %>% 
            select(location_name, sex_name, year_id, Baseline, Adjusted) %>% 
            gather(assumption, val, -year_id, -sex_name, -location_name) %>%
            spread(year_id, val) %>% 
            mutate(lpc10 = 100/20*log(`2030`/`2010`),
                   lpc15 = 100/15*log(`2030`/`2015`)) %>% 
            select(location_name, sex_name, assumption, lpc10, lpc15) %>% rename("2010"=lpc10, "2015"=lpc15) %>%
            gather(byear, lpc, -assumption, -sex_name, -location_name) %>% mutate(byear = as.numeric(byear)) %>%
            spread(assumption, lpc) %>% data.table() %>% mutate(ncd_cause = cause) 

         ts_pc <- rbind(ts_pc, dfreg)
         
      k = k + 1
      c = c + 1
   }
   r = r + 1
}

close(pb)

################################################################################################
# All NCDs
################################################################################################

P0_all    = apply(Pb, c(1,3,4), mean)
P1_all    = apply(Pa, c(1,3,4), mean)

D0cse_all = apply(Db, c(1,3,4), sum)
D1cse_all = apply(Da, c(1,3,4), sum)

sexes  <- c("Female","Male")
n      <- 12
r      <- 1

for (is in all.locs){
   q300a <- q30.est.sex(P0_all[r,,], D0cse_all[r,,], n)
   q301a <- q30.est.sex(P1_all[r,,], D1cse_all[r,,], n)
   
   ltncd <- q300a %>% mutate(assumption = "Baseline")  
   ltncd <- q301a %>% mutate(assumption = "Adjusted") %>%
      rbind(ltncd) %>% mutate(ncd_cause = "Non-communicable diseases", location_name = is) %>% 
      spread(assumption, val) %>% arrange(sex_name, year_id)
   
   pop1015a    <- wpp.in %>% filter(location_name == is & year %in% c(2010,2015)) %>% 
      select(year, sex_name, age_name, Nx) %>% spread(year, Nx) %>% 
      arrange(sex_name, age_name) %>% select("2010","2015") %>% as.matrix()
   
   # NCD death rates for 2010 and 2015, by single age and sex
   rncd1015a   <- cause.mx %>% filter(location_name == is & year %in% c(2010,2015)) %>% 
      group_by(year, sex_name, age_name) %>% summarise(mxs = sum(mx, na.rm = T), .groups = "drop") %>% 
      spread(year, mxs) %>% 
      arrange(sex_name, age_name) %>% select("2010","2015") %>% as.matrix()
   
   # Calculate deaths by scaling population with rates
   dncd1015a  <- rncd1015a*pop1015a 
   
   # Aggregating to person and estimating 40q30s
   p1015a     <- apply(pop1015a, 2, combine.ages5) # sex specific pops in 5 years ages
   d1015a     <- apply(dncd1015a, 2, combine.ages5)# sex specific deaths in 5 years ages
   r1015a     <- d1015a/p1015a                      # sex specific rates in 5 years ages
   
   # reorder, F2010, F2015, M2010, M2015
   r1015_2a   <- cbind(r1015a[1:19,1], r1015a[1:19,2], r1015a[20:38,1],r1015a[20:38,2])
   vq30a      <- .01*apply(r1015_2a, 2, get.q30) # 40q30 estimates Female 2010 and 2015, Male 2010 and 2015, respectively
   
   q30.start.a <- data.table(location_name = is, 
                           sex_name = c("Female","Female", "Male", "Male"),
                           year_id  = c(2010, 2015, 2010, 2015),
                           Adjusted = vq30a,
                           Baseline = vq30a,
                           ncd_cause= "Non-communicable diseases")
   ts_40q30    <- rbind(ts_40q30, q30.start.a, ltncd)
   
      dfreg.a       <- ltncd %>% rbind(q30.start.a) %>%
         filter(year_id %in% c(2010,2015,2030)) %>% 
         select(location_name, sex_name, year_id, Baseline, Adjusted) %>% 
         gather(assumption, val, -year_id, -sex_name, -location_name) %>%
         spread(year_id, val) %>% 
         mutate(lpc10 = 100/20*log(`2030`/`2010`),
                lpc15 = 100/15*log(`2030`/`2015`)) %>% 
         select(location_name, sex_name, assumption, lpc10, lpc15) %>% rename("2010"=lpc10, "2015"=lpc15) %>%
         gather(byear, lpc, -assumption, -sex_name, -location_name) %>% mutate(byear = as.numeric(byear)) %>%
         spread(assumption, lpc) %>% data.table() %>% mutate(ncd_cause = "Non-communicable diseases")
      
      ts_pc <- rbind(ts_pc, dfreg.a)

   r = r + 1
}

################################################################################################
################################################################################################

cseorder <- c(fread("../new_inputs/Cause_groupings2.csv") %>% pull(ncdorder), "Non-communicable diseases")
iso3reg  <- fread("../new_inputs/iso3region.csv") %>% select(iso3, region) %>% right_join(country.lab, by = "iso3")

q302015      <- ts_40q30 %>% 
   filter(year_id == 2015) %>% select(location_name, sex_name, ncd_cause, Baseline) %>%
   mutate(Baseline = Baseline) %>% rename(q30in2015 = Baseline)

out_lt_cause <- left_join(ts_pc %>% filter(byear == 2015) %>% mutate(byear = NULL), 
                          q302015, by = c("location_name","sex_name","ncd_cause")) %>% 
   mutate(ncd_cause = factor(ncd_cause, levels = cseorder)) %>% 
   left_join(iso3reg, by = "location_name") %>% 
   gather(metric, val, -location_name, -sex_name, -ncd_cause, -iso3, -region) %>% 
   spread(sex_name, val) %>% mutate(Female = ifelse(is.na(Female), 0, Female),
                                    Male   = ifelse(is.na(Male), 0, Male)) %>% 
   gather(sex_name, val, -location_name, -metric, -ncd_cause, -iso3, -region) %>% 
   spread(metric, val)

save(ts_40q30, out_lt_cause, countries, cseorder, file = "output/NCDcountdown_bycause.df.rda")

################################################################
#-----------------------
# Plot AROC 
#----------------------
rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dplyr, tidyr, data.table, ggplot2, ggpubr, gridExtra, RColorBrewer, ggrepel)

'%!in%'   <- function(x,y)!('%in%'(x,y))
col1      <- brewer.pal(8, "Dark2")[3:7]
load("output/NCDcountdown_bycause.df.rda")

cseorder2 <- c("Non-communicable diseases", fread("../new_inputs/Cause_groupings2.csv")  %>% 
                  filter(ncdorder != "All other noncommunicable diseases") %>% pull(ncdorder) %>% rev())

###########################################################################################################

lpc.hist <- ts_40q30 %>% 
   filter(ncd_cause %!in% c("All causes", "All other noncommunicable diseases")) %>% 
   select(-c(Adjusted)) %>%
   spread(year_id, Baseline) %>% mutate(lpc = 100/5*log(`2015`/`2010`), Source = "Historical") %>% 
   select(location_name, sex_name, ncd_cause, Source, lpc) %>%
   mutate(lpc = ifelse(sex_name == "Male" & ncd_cause %in% c("Breast cancer","Cervix uteri cancer"), NA, lpc)) %>%
   mutate(lpc = ifelse(sex_name == "Female" & ncd_cause %in% c("Prostate cancer"), NA, lpc)) %>%
   mutate(lpc = ifelse(is.infinite(lpc), NA, lpc))

############################################################################################################

lpc.proj <- out_lt_cause %>% filter(ncd_cause != "All other noncommunicable diseases") %>%
   select(location_name, sex_name, ncd_cause, Adjusted, Baseline) %>%
   gather(Source, lpc, -location_name, -sex_name, -ncd_cause) %>%
   mutate(lpc = ifelse(sex_name == "Male" & ncd_cause %in% c("Breast cancer","Cervix uteri cancer"), NA, lpc)) %>%
   mutate(lpc = ifelse(sex_name == "Female" & ncd_cause %in% c("Prostate cancer"), NA, lpc))  %>%
   mutate(ncd_cause = as.character(ncd_cause), location_name = as.character(location_name)) %>%
   mutate(lpc = ifelse(is.infinite(lpc), NA, lpc))

############################################################################################################

lpc.plot <- rbind(lpc.hist, lpc.proj) %>%
   spread(Source, lpc) %>%
   mutate(Adjusted = ifelse(is.na(Adjusted), Historical, Adjusted),
          Baseline = ifelse(is.na(Baseline), Historical, Baseline)) %>%
   gather(Source, lpc, -location_name, -sex_name, -ncd_cause) %>% 
   mutate(ncd_cause  = factor(ncd_cause, levels = cseorder2)) %>%
   group_by(ncd_cause, sex_name, Source) 


   library(viridis)
   library(hrbrthemes)
   library(forcats)
   library(ggridges)
   #library(Ipaper)
   
      #ridge
      inner = geom_density_ridges(alpha=0.4, aes(fill=text, y=ncd_cause, x=value))
      #box
      #inner = geom_boxplot(alpha=0.4, aes(fill=text, y=ncd_cause, x=value), outlier.shape = NA)
    
   levs = c("Historical", "Adjust to 90%")
   
   plot<-na.omit(lpc.plot %>%
      mutate(Source = ifelse(Source == "Adjusted",  "Adjust to 90%", Source),
             text = factor(Source, levels = levs), value = lpc) )
      
     ggplot(plot%>%filter(Source!="Baseline")) +  
      geom_vline(xintercept = 0, colour = "grey") + 
      geom_vline(xintercept =  -2.703101, colour = "grey", lty = "dashed") +
      inner + scale_fill_viridis(discrete=TRUE) +
      scale_color_viridis(discrete=TRUE) + theme_bw() +
      theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
            title=element_text(face='bold'), legend.position = "bottom", 
            text=element_text(face='bold', size = 15), 
            legend.title = element_blank()) + facet_wrap(~sex_name) +
      xlab("Yearly percentage change in cause-specific probability of dying") +
      ylab("")+ 
      xlim(-15,5)
   
   
ggsave("Figures/Figure2.png")


