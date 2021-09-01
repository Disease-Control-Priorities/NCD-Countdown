###############################################################################################################################
###############################################################################################################################
#-----------------------
# Housekeeping
#-----------------------
###############################################################################################################################

rm(list = ls(all = TRUE)) # Clear all memory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

library(foreach)
library(snow)
library(parallel)
library(iterators)

library(doParallel)

load("../new_inputs/PreppedData0819.Rda")
source("utils/demmod_icer_rank.R")

###############################################################################################################################
###############################################################################################################################

all.locs      <- countries[c(1:85,87:124)]
increases     <- c(0,0.005,0.015,0.025,0.035,0.045)#seq(0, 0.09, 0.01)
covidscens    <- seq(0, 2, 0.5)
total         <- length(increases)*length(covidscens)*length(all.locs)

inter         <-  pin.groups %>% filter(calc_ICER=="yes") %>% pull(Code) %>% unique() %>% sort() 
sel.cse       <- cse_g %>% pull(cause_name) %>% unique() # using all NCD causes


adj.D.P       <- array(dim = c(2,
                               length(covidscens),
                               length(increases),
                               length(all.locs), 
                               172, 12))
q30.df        <- ncd.deaths <- goal.q30 <- list(); 
scen.list     <- data.table(k.index = integer(), )


parallelCluster <- makeCluster(32,type = "SOCK",methods = FALSE, outfile="log3.txt") #first number is number of cores

setDefaultCluster(parallelCluster)
registerDoParallel(parallelCluster)

# set MRO's MKL threads to 1 and set data.table threads to 1
clusterEvalQ(cl = parallelCluster, {
  #setMKLthreads(1)    # MRO
  pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws)  # make sure we load the package on the cluster
  #setDTthreads(1)     # data.table
})




covidscenarios <- function(is,interventions,increases,covidscens, q30.df, ncd.deaths, goal.q30, adj.D.P) {

k = 1

#pb            <- winProgressBar(title = "progress bar", min = 0,max = total, width = 300)

i.count = 1
  
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
  
  df.targetq30 <- data.table(Baseline_2015 = vq30[2], Target_40q30 = tq30[2])
  
  j.count = 1
  for (sinc in increases){

    c.count = 1
    for (sc.covid in covidscens){
    #setWinProgressBar(pb, k, title=paste0(round(k/total*100,3),"% done"))
      if(sinc==0){
        tas<-"no"
      }
      
      else{
        
        tas<-"yes"
      }
      proj  <- project_pop(is, inter, sinc, tas, sel.cse, "varying", "no", "yes", sc.covid)
    
  goal.q30[[k]]   <- proj$q30df %>%  filter(year_id == 2030) %>% 
    select(location_name, Adjusted) %>% rename(Adjusted_2030 = Adjusted) %>%
    cbind(df.targetq30) %>%
      mutate(s_covid = sc.covid, s_inc = sinc,
             Change = Target_40q30 - Adjusted_2030, Reaches = ifelse(Change < 0, "No","Yes"))  %>% 
    select(location_name, Baseline_2015, Target_40q30, Adjusted_2030, s_covid, s_inc, Change, Reaches) %>%
    data.table()

  ncd.deaths[[k]] <- data.table(year_id = 2019:2030, group = "Deaths", 
                                base.ncd = apply(proj$D0cse,2,sum), 
                                adj.ncd = apply(proj$D1cse,2,sum), 
                                location = is, s_covid = sc.covid, s_inc = sinc)
  
  q30.df[[k]]     <- data.table(proj$q30df %>% select(-c(n.inters, q30.ave)), 
                                s_covid = sc.covid, s_inc = sinc)
  
  adj.D.P[1, c.count, j.count, i.count,,]  <- proj$D1
  adj.D.P[2, c.count, j.count, i.count,,]  <- proj$P1
  
        c.count = c.count + 1; k = k + 1
    }
    j.count = j.count + 1
  }
  i.count = i.count + 1
  
  return(list(q30.df, ncd.deaths, goal.q30))
}

Sys.time()
everything <- foreach (is = all.locs, .combine ='rbind') %dopar% {
  covidscenarios(is,interventions,increases,covidscens, q30.df, ncd.deaths, goal.q30, adj.D.P)
}
Sys.time()


q30.df <- rbindlist(everything[[1]])

for (i in 2:123){
  temp<-rbindlist(everything[[i]])
  q30.df<-rbind(q30.df, temp)
}

ncd.deaths.out <- rbindlist(everything[[124]])

for (i in 125:246){
  temp<-rbindlist(everything[[i]])
  ncd.deaths.out<-rbind(ncd.deaths.out, temp)
}

goal.q30.out <- rbindlist(everything[[247]])

for (i in 248:369){
  temp<-rbindlist(everything[[i]])
  goal.q30.out<-rbind(goal.q30.out, temp)
}


all.q30.out <- q30.df %>%
  mutate(measure = "40q30") %>%  mutate(Baseline = round(Baseline, 2),
                                        Adjusted = round(Adjusted, 2)) %>% 
  arrange(location_name, s_covid, s_inc, year_id)

###############################################################################################################################
###############################################################################################################################
# summary
###############################################################################################################################

summary <- goal.q30.out %>% mutate(Reaches = as.numeric(factor(Reaches)) - 1, All = 1) %>% 
  group_by(s_covid, s_inc) %>% summarise(n.reaches = sum(Reaches), n.all = sum(All), .groups = "drop")

summary <- ncd.deaths.out %>% group_by(s_covid, s_inc) %>% 
  summarise(base.ncd = sum(base.ncd), 
            adj.ncd  = sum(adj.ncd),.groups = "drop") %>%
  mutate(base.diff = base.ncd - min(base.ncd),
         adj.diff  = adj.ncd - min(adj.ncd)) %>% 
  left_join(summary, by = c("s_covid", "s_inc"))

# Calculate 40q30 by scenario

deaths.sum <- apply(adj.D.P[1,,,,,], c(1,2,4,5), sum)
pop.sum    <- apply(adj.D.P[2,,,,,], c(1,2,4,5), sum)

pop.sum2   <- apply(apply(pop.sum, c(1,2,4), combine.ages5), c(2:4), get.pers) # person pops in 5 years ages
deaths.sum2<- apply(apply(deaths.sum, c(1,2,4), combine.ages5), c(2:4), get.pers)# person deaths in 5 years ages
r.sum      <- deaths.sum2/pop.sum2                                          # person rates in 5 years ages
q30.sum    <- apply(r.sum, 2:4, get.q30) # Array col 1 is covid scale, col 2 is coverage scale, col 3 is year (2019 -2030)


###############################################################################################################################

save(goal.q30.out, ncd.deaths.out, summary, all.q30.out, adj.D.P, q30.sum, 
     file = "output/goal.q30.covid_0830.Rda")

###############################################################################################################################


