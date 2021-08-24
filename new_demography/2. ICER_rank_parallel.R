###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs
# https://towardsdatascience.com/parallelization-caveats-in-r-1-the-basics-multiprocessing-and-multithreading-performance-eb584b7e850ehttps://towardsdatascience.com/parallelization-caveats-in-r-1-the-basics-multiprocessing-and-multithreading-performance-eb584b7e850e


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

###############################################################################################################################
# Loop getting pin and DALY estimates

all.locs      <- countries[c(1:85,87:124)]
interventions <-  pin.groups %>% filter(calc_ICER=="yes") %>% pull(Code) %>% unique() %>% sort()
total         <- length(all.locs)*length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

all.pin.l     <- all.dalys.l <- all.q30.l <-dadt.all.l<- list(total)

parallelCluster <- makeCluster(32,type = "SOCK",methods = FALSE,outfile="log.txt")

setDefaultCluster(parallelCluster)
registerDoParallel(parallelCluster)

# set MRO's MKL threads to 1 and set data.table threads to 1
clusterEvalQ(cl = parallelCluster, {
  #setMKLthreads(1)    # MRO
  pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws)  # make sure we load the package on the cluster
  #setDTthreads(1)     # data.table
})


#pb  <- winProgressBar(title ="progress bar", min = 0, max = total, width = 300)
#pb  <- winProgressBar(title = paste("progress bar", is), min = 0, max = total, width = 300)

do_the_thing <- function(is,interventions,all.pin.l,all.dalys.l,all.q30.l, dadt.all.l) {
  k = 1
  for (inter in interventions){
    #setWinProgressBar(pb, k, title=paste0(round(k/total*100,3),"% done"))
    
    # Increasing by 1p.p for ICER calculation
    projection <- project_pop(is, inter, 0.10, "no", sel.cse, "fixed", "yes", "no")
    
    all.pin.l[[k]]    = data.table(projection$pin.est)
    all.dalys.l[[k]]  = data.table(projection$dalys) %>% mutate(Code = inter)
    all.q30.l[[k]]    = data.table(projection$q30df) %>% mutate(Code = inter)
    dadt.all.l[[k]]   = data.table(projection$DAdt) %>% mutate(Code = inter)
    
    
    k = k + 1
  }
  return(list(all.pin.l,all.dalys.l,all.q30.l, dadt.all.l))
}

Sys.time()
everything <- foreach (is = all.locs, .combine ='rbind') %dopar% {
  do_the_thing(is,interventions,all.pin.l,all.dalys.l,all.q30.l, dadt.all.l)
}

Sys.time()
#close(pb)
  

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

stopCluster(parallelCluster)

#########################################################################################

save(all.pin, all.dalys, all.q30, dadt.all, file = "output/icer_rank_output_0819.Rda")

on.exit({
  try({
    cat("Attempting to stop cluster\n")
    stopImplicitCluster()        # package: `doParallel`
    stopCluster(parallelCluster) # package: `parallel`
  })
})

