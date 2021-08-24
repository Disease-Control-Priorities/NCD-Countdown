##########################
## load packages
##########################

library(shiny)
library(shinythemes)
library(shinydashboard)
library(data.table)
library(dplyr)
library(tidyr)
library(highcharter)
library(MortalityLaws)

##########################
## get required functions
##########################

load("../../new_inputs/PreppedData.Rda")
source("../utils/demmod_icer_rank.R")
load("../utils/obs.wpp.Rda")

cols     <- c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666",
              "#66C2A5","#FC8D62", "#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3")