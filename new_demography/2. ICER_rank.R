###############################################################################################################################
###############################################################################################################################
# Generate output files that can be used to rank the interventions according to ICERs

rm(list=ls()) 
pacman::p_load(data.table, dplyr, tidyr, progress, pspline, MortalityLaws) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("../new_inputs/PreppedData0819.Rda")
source("utils/demmod_icer_rank.R")

###############################################################################################################################
# Loop getting pin and DALY estimates

all.locs      <- countries
interventions <-  pin.groups %>% filter(calc_ICER=="yes") %>% pull(Code) %>% unique() %>% sort() 
total         <- length(all.locs)*length(interventions)
sel.cse       <- cse_g %>% pull(cause_name) %>% unique()

all.pin.l     <- all.dalys.l <- all.q30.l <- list(total)

pb            <- winProgressBar(title = "progress bar", min = 0, max = total, width = 300)
k = 1
for (is in all.locs){
  j = 1
  for (inter in interventions){
    setWinProgressBar(pb, k, title=paste0(round(k/total*100,3),"% done"))
    
    # Increasing by 1p.p for ICER calculation
    projection = project_pop(is, inter, 0.10, "no", sel.cse, "fixed", "yes", "no")      
    
    all.pin.l[[k]]    = data.table(projection$pin.est)
    all.dalys.l[[k]]  = data.table(projection$dalys) %>% mutate(Code = inter)
    all.q30.l[[k]]    = data.table(projection$q30df) %>% mutate(Code = inter)
    
    j = j + 1; k = k + 1
  }
}
close(pb)

all.pin      <- rbindlist(all.pin.l)
all.dalys    <- rbindlist(all.dalys.l)
all.q30      <- rbindlist(all.q30.l)

#########################################################################################
save(all.pin, all.dalys, all.q30, file = "output/icer_rank_output.Rda")

