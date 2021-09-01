rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(dplyr, tidyr, data.table, ggplot2, ggpubr, gridExtra, RColorBrewer, ggrepel)
library(stringr)
library(ggforce)
library(dplyr)
library(readxl)
library(tinytex)
library(tidyr)

##################### AROC ################################################

#-----------------------
# Plot AROC 
#----------------------


'%!in%'   <- function(x,y)!('%in%'(x,y))
col1      <- brewer.pal(8, "Dark2")[3:7]

load("output/NCDcountdown_bycause.df0830.rda")

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

inner = geom_density_ridges(alpha=0.4, aes(fill=text, y=ncd_cause, x=value))
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
  xlim(-20,5)

ggsave("Figures/Figure1_0830.png")

##diff##
inner = geom_density_ridges(alpha=0.4, aes(y=ncd_cause, x=value))
#levs = c("Difference")

plot2<-na.omit(lpc.plot %>% spread(Source, lpc))%>%mutate(value=Adjusted-Historical)

ggplot(plot2)+  
  #geom_vline(xintercept = 0, colour = "grey") + 
  #geom_vline(xintercept =  -2.703101, colour = "grey", lty = "dashed") +
  inner + #scale_fill_viridis(discrete=TRUE) +
  #scale_color_viridis(discrete=TRUE) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),
        title=element_text(face='bold'), legend.position = "bottom", 
        text=element_text(face='bold', size = 15), 
        legend.title = element_blank()) + facet_wrap(~sex_name) +
  xlab("Difference in yearly cause-specific probability of dying (%)") +
  ylab("")+ 
  xlim(-20,5)

ggsave("Figures/Appendix-densityplot_0830.png")


#number for paper
achieved<-plot%>%filter(Source=="Adjust to 90%",
                        ncd_cause=="Non-communicable diseases")
achieved$reached<-0
achieved$reached[achieved$lpc<= -2.703101]<-1
sum(achieved$reached)/246

################################################################################################
#Target figure

load("output/goal.q30.covid_0830.Rda")

#colors from https://spectrum.adobe.com/page/color-for-data-visualization/
ryb<-c("#d73027","#fdae61", "#ffffbf","#abd9ea", "#4575b4")
ryb9<-c("#d73027","#EA6F44", "#f5a055","#fccd76", "#ffffbf", "#D5ECD5","#abd9ea", "#78A7CF" ,"#4575b4")
ryb2<-c("#c65154","#f0a882", "#ffffe0","#89c0c4", "#208288")
ryb2_reverse<-c("#208288", "#89c0c4", "#ffffe0", "#f0a882", "#c65154")
pwb<-c("#cc415a","#ed9ab0", "#faf0ff","#92b2de", "#2f74b3")
ybp<-c("#3c1f5c","#33628e", "#2d7b8d","#20a386", "#fde725")

#base<-all.q30.out%>%filter(s_covid==0 & s_inc==0)%>%mutate(scenario="Baseline")%>%
#  select(Adjusted, year_id, location_name, scenario)
best<-all.q30.out%>%filter(s_covid==0 & s_inc==0.045 & year_id>=2019)%>%mutate(scenario="Most optimistic")%>%
  select(Adjusted, year_id, location_name, scenario)
middle<-all.q30.out%>%filter(s_covid==1 & s_inc==0.025 & year_id>=2019)%>%mutate(scenario="Moderate")%>%
  select(Adjusted, year_id, location_name, scenario)
worst<-all.q30.out%>%filter(s_covid==2 & s_inc==0 & year_id>=2019)%>%mutate(scenario="Most pessimistic")%>%
  select(Adjusted, year_id, location_name, scenario)

plot3<-bind_rows(best, middle, worst)
plot3$scenario<-factor(plot3$scenario, levels=c("Most pessimistic", "Moderate", "Most optimistic"))

historic<-unique(goal.q30.out%>%select(c(location_name, Baseline_2015)))

## Population-weighted average for global plot
load("../new_inputs/PreppedData0819.Rda")
pop<-wpp.in%>%filter(year==2019)%>%group_by(location_name)%>%summarise(pop=sum(Nx))

historic<-historic%>%rename(Adjusted = Baseline_2015)%>%mutate(year_id=2015, scenario = "Historic")
add2019<-plot3%>%filter(year_id==2019, scenario=="Moderate")%>%mutate(scenario = "Historic")
historic<-rbind(historic, add2019)

plot3<-rbind(plot3, historic)
plot3<-left_join(plot3, pop, by="location_name")
global<-plot3%>%group_by(scenario,year_id)%>%summarise(q30=weighted.mean(Adjusted, pop))

goal.q30.out<-left_join(goal.q30.out, pop, by='location_name')
global_target<-weighted.mean(goal.q30.out$Target_40q30, goal.q30.out$pop)

global$year<-as.numeric(global$year_id)

ggplot(global, aes(x=year, y=q30, color=scenario, group=scenario))+
  geom_point()+
  geom_line()+
  geom_line(y=global_target, color="darkgrey", lty=2)+
  ylab("40q30 (%)")+
  xlab("Year")+
  ggtitle("Projected 40q30 over time, Global")+
  labs(color="Scenario")+
  coord_cartesian(ylim=c(0,30))+
  scale_color_manual(values=c("#cc415a", "#057fee", "#22a884", "black"))+
  theme_bw()
#annotate(geom="text", x=2027, y=global_target-2, label="Target: 1/3 reduction in 40q30", color="darkgrey")

ggsave("Figures/Appendix-figureA3_0830.png", height = 7, width = 9, units = "in")


### heat map ###

base<-summary%>%filter(s_inc==0)%>%select(c(s_covid,adj.ncd))%>%rename(baseline = adj.ncd)
heatmap<-left_join(summary, base, by="s_covid")%>%mutate(diff=baseline-adj.ncd)

ggplot(heatmap%>%filter(s_inc!=0), 
       aes(y=s_covid, x=s_inc*100, fill=adj.diff/1000000))+
  geom_tile()+
  geom_text(aes(label=paste0(signif(100*n.reaches/124, digits=2), "%")), color="black")+
  scale_fill_gradientn(colors=ryb[c(5,3,1)], values=c(0,0.45,1))+
  labs(fill="NCD deaths (millions) \nin excess of the most \noptimistic covid scenario")+
  ylab("Covid scaling factor for underreporting")+
  xlab("Maximum annual increase (%) in intervention coverage")+
  ggtitle("Global excess NCD deaths and the number of countries \nachieving a one-third reduction in 40q30 \nby each covid and intervention scale-up scenario")+
  labs(caption = "\n*Text numbers represent the percent of LMICs achieving a 1/3 reduction in 40q30 by 2030")


#ggsave("Figures/Figure4_0819_2.png", height = 7, width = 9, units = "in")


##binned colors##
heatmap$cat[heatmap$adj.diff/1000000>=35]<-"35+"
heatmap$cat[heatmap$adj.diff/1000000<35&heatmap$adj.diff/1000000>=30]<-"30-34"
heatmap$cat[heatmap$adj.diff/1000000<30&heatmap$adj.diff/1000000>=25]<-"25-29"
heatmap$cat[heatmap$adj.diff/1000000<25&heatmap$adj.diff/1000000>=20]<-"20-24"
heatmap$cat[heatmap$adj.diff/1000000<20&heatmap$adj.diff/1000000>=15]<-"15-19"
heatmap$cat[heatmap$adj.diff/1000000<15&heatmap$adj.diff/1000000>=10]<-"10-14"
heatmap$cat[heatmap$adj.diff/1000000<10&heatmap$adj.diff/1000000>=5]<-"5-9"
heatmap$cat[heatmap$adj.diff/1000000<5&heatmap$adj.diff/1000000>0]<-"0-4"
heatmap$cat[heatmap$adj.diff/1000000==0]<-"Most optimistic"

heatmap$cat<-factor(heatmap$cat, levels=c("35+","30-34", "25-29", "20-24","15-19",
                                          "10-14", "5-9", "0-4", "Most optimistic"))

ggplot(heatmap%>%filter(s_inc!=0), 
       aes(y=s_covid, x=s_inc*100, fill=cat))+
  geom_tile()+
  geom_text(aes(label=paste0(signif(100*n.reaches/124, digits=2), "%")), color="black")+
  scale_fill_manual(values=ryb9)+
  labs(fill="NCD deaths (millions) \nin excess of the most \noptimistic scenario")+
  ylab("Adjustment factor for under-reporting of NCD deaths during pandemic")+
  xlab("Target annual percent increase in intervention coverage")+
  theme_minimal()

ggsave("Figures/Figure3_0830.png", height = 7, width = 9, units = "in")

#ggsave("Figures/Figure4_0815_difference.png", height = 7, width = 9, units = "in")


## Spaghetti plot

load("output/goal.q30.covid_0830.Rda")
regions<-read.csv("../new_inputs/Country_groupings.csv")%>%select(c(Country, NCD_region))%>%rename(location_name = Country)

##UPDATE 2015-2030
##Difference from 2015 (1 at year==2015, 0.67 ==target)
##highlight net increase from 2020 to 2030
q2015<-unique(goal.q30.out%>%select(c(location_name, Baseline_2015)))%>%
  mutate(year_id=2015, s_covid=1, s_inc=0.025)%>%rename(Adjusted = Baseline_2015)

all.q30.out<-rbind(all.q30.out, q2015, fill=TRUE)

plot<-left_join(all.q30.out, regions, by='location_name')
reg<-unique(plot$NCD_region)

base<-unique(goal.q30.out%>%select(c(location_name, Baseline_2015)))
plot<-left_join(plot, base, by='location_name')

plot$diff<-plot$Adjusted/plot$Baseline_2015

#outliers
plot$label[plot$location_name=="Syria" & plot$year_id==2029]<-"Syria"
plot$label[plot$location_name=="Haiti"& plot$year_id==2029]<-"Haiti"
#plot$label[plot$location_name=="Mauritius"& plot$year_id==2030]<-"Mauritius"
plot$label[plot$location_name=="Malawi"& plot$year_id==2030]<-"Malawi"
#plot$label[plot$location_name=="South Africa"& plot$year_id==2030]<-"South Africa"


p<-ggplot(plot%>%filter(s_covid==1, s_inc==0.025, NCD_region!="Sub-Saharan Africa"), 
          aes(x=year_id, y=diff, group=location_name))+
  geom_line()+
  ylab("Change in 40q30")+
  facet_wrap(~NCD_region)+
  geom_text(aes(label=label), vjust=-1)+
  geom_line(y=1, color="red", lty=2)+
  geom_line(y=0.67, color="#009FD7", lty=2)
  

p

#ggsave("Figures/adjusted40q30.png")

plot2<-left_join(plot, pop, by="location_name")
SSA<-plot2%>%filter(NCD_region=="Sub-Saharan Africa",s_covid==1, s_inc==0.025)%>%group_by(year_id)%>%summarise(diff = weighted.mean(diff, pop))
SSA$location_name<-"Regional average"
plot2<-plot2%>%filter(NCD_region=="Sub-Saharan Africa",s_covid==1, s_inc==0.025)%>%select(c(year_id, location_name, diff))

plot2<-rbind(plot2, SSA)

plot2$label<-"Other countries"
plot2$label[plot2$location_name=="Malawi"]<-"Malawi"
plot2$label[plot2$location_name=="Regional average"]<-"Regional average"

plot2$text[plot2$location_name=="Malawi"& plot2$year_id==2030]<-"Malawi"
plot2$text[plot2$location_name=="Regional average" & plot2$year_id==2030]<-"Regional average"

plot2$year<-as.numeric(plot2$year_id)

ggplot(plot2, 
          aes(x=year, y=diff, group=location_name, color=label, size=label))+
  geom_line()+
  ylab("Change in 40q30 relative to 2015")+
  geom_line(y=1, color="black", lty=2, size=1)+
  geom_line(y=0.67, color="black", lty=2, size=1)+
  scale_color_manual(values=c("#009E73","grey", "#CC79A7"), guide='none')+
  scale_size_manual(values=c(1.5,0.5,1.5), guide='none')+
  geom_text(aes(label=text), hjust=-0.1, size=4)+
  theme_bw()+
  xlab("Year")+
  labs(color = "Location")+
  xlim(2015,2032)

ggsave("Figures/Appendix-figureA6_0830.png")


##################### global ##########################

global<-left_join(plot, pop, by="location_name")%>%filter(s_covid==1, s_inc==0.025)
global<-global%>%group_by(year_id)%>%
  summarise(adjusted=weighted.mean(Adjusted, pop), baseline=weighted.mean(Baseline_2015, pop))
global$diff<-global$adjusted/global$baseline
global$NCD_region<-"Global"

byregion<-left_join(plot, pop, by="location_name")%>%filter(s_covid==1, s_inc==0.025)
byregion<-byregion%>%group_by(year_id, NCD_region)%>%
  summarise(adjusted=weighted.mean(Adjusted, pop), baseline=weighted.mean(Baseline_2015, pop))
byregion$diff<-byregion$adjusted/byregion$baseline

plot2<-bind_rows(global, byregion)
plot2$year_id<-as.numeric(plot2$year_id)
regions<-unique(plot2$NCD_region[plot2$NCD_region!="Global"])
plot2$groups<-factor(plot2$NCD_region, levels = c("Global", regions))
plot2$type[plot2$NCD_region=="Global"]<-"lt2"
plot2$type[plot2$NCD_region!="Global"]<-"lt1"



cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "grey", "#0072B2", "#D55E00", "#CC79A7")
  
ggplot(plot2, 
          aes(x=year_id, y=diff, group=groups, color=groups))+
  geom_line(aes(size=type))+
  scale_size_manual(values=c(0.8, 1.2), guide=FALSE)+
  scale_color_manual(values=c('black', cbPalette ))+
  ylab("Change in 40q30 from 2015")+
  geom_line(y=1, color="grey", lty=2)+
  geom_line(y=0.67, color="grey", lty=2)+
  theme_bw()+
  xlab("Year")+
  labs(color="Region")+
  guides(color = guide_legend(override.aes = list(size = 2)))

ggsave("Figures/Appendix-figureA5_0830.png", height = 7, width = 9, units = "in")



### MALAWI numbers ###
uc<-read.csv("output/costs/bycountry/Malawi_adjusted_uc_2017.csv")
cov<-read.csv("../new_inputs/Coverage0621.csv")

cost<-uc%>%group_by(NCD, Intervention)%>%summarize(Adjusted_UC = sum(adjusted_uc))
coverage<-cov%>%filter(Country=="Malawi")%>%select(-c(WB_region15, GBD_region, Country, iso3, LocID))%>%
  gather(Code, Baseline_coverage)%>%mutate(NCD=as.numeric(sub('X', '', Code)))

malawi<-left_join(cost, coverage, by="NCD")
load("output/icer_rank_output_0830.Rda")

deaths<-dadt.all%>%filter(location_name == "Malawi")%>%group_by(Code)%>%
  summarise(Deaths.Averted = sum(Deaths.Avert))%>%mutate(Code = paste0("X",Code))

dalys<-all.dalys%>%filter(location_name =="Malawi")%>%group_by(Code)%>%
  summarise(DALYs.Averted = sum(DALY.ave))%>%mutate(Code = paste0("X",Code))

changeq30<-all.q30%>%filter(location_name =="Malawi")%>%group_by(Code)%>%
  summarise(Change.40q30 = sum(q30.ave))%>%mutate(Code = paste0("X",Code))

malawi<-left_join(malawi, deaths, by="Code")
malawi<-left_join(malawi, dalys, by="Code")
malawi<-left_join(malawi, changeq30, by="Code")

m_icer<-read.csv("output/All_ICER_2030_0830.csv", stringsAsFactors = F)%>%
  filter(Country == "Malawi")%>%select(-c(X, Country, World_bank, NCD_region, iso3))%>%mutate(Code = paste0("X",Code))

malawi<-left_join(malawi, m_icer, by="Code")

write.csv(malawi[,-c(1,4)], "Figures/Malawi_icer_results_0830.csv", row.names = F)

#optimization results
load("output/Optim.Out0830.Rda")

n.inters<-q30.CodeDelta%>%filter(location_name=="Malawi", byear==2015)%>%pull(n.inters)
inters.total<-15

inc.annual<-read.csv("output/Optimal/All_Intervention_2030_0830.csv")%>%filter(Country=="Malawi")

##add intersectoral
int_cost<-read.csv("output/costs/All_total_cost_2030_intervention_0830.csv", stringsAsFactors = F)%>%
  mutate(unique_id = paste0("C",unique_id))%>%filter(substr(unique_id,1,2)=="C5")%>%filter(Country=="Malawi")

cost<-bind_rows(inc.annual, int_cost)%>%
  select(-c(X, adjusted_uc, total_cost, pin_sum, Country,Class, Region))%>%
  gather(year, cost, -senario, -unique_id)%>%
  spread(senario, cost)

cost$Incremental<-cost$Adjusted-cost$Baseline

adj<-cost%>%select(c(year, Adjusted, unique_id))%>%spread(year, Adjusted)%>%mutate(Scenario = "Adjusted")
base<-cost%>%select(c(year, Baseline, unique_id))%>%spread(year, Baseline) %>%mutate(Scenario = "Baseline")
incr<-cost%>%select(c(year, Incremental, unique_id))%>%spread(year, Incremental) %>%mutate(Scenario = "Incremental")

malawi_costs<-rbind(base, adj,incr)

write.csv(malawi_costs, "Figures/Malawi_reference_costs_0830.csv")

deaths_opt<-dadt.all.opt%>%filter(location_name=="Malawi", byear==2015)%>%group_by(year_id)%>%summarise(DA = sum(Deaths.Avert))
#deaths averted
sum(deaths_opt$DA)
#cost per death averted
malawi_costs$Incremental[malawi_costs$unique_id=="total" & malawi_costs$year=="2023-2030"]/sum(deaths_opt$DA)

dalys_opt<-dalys.opt%>%filter(location_name=="Malawi", byear==2015)%>%group_by(year_id)%>%summarise(DALY.ave = sum(DALY.ave))
dalys_opt$year_id<-as.numeric(dalys_opt$year_id)

write.csv(left_join(dalys_opt, deaths_opt), "Figures/Malawi_reference_impact_0830.csv")

#40q30
q30_opt<-q30.sel%>%filter(location_name=="Malawi")


############### Numbers for paper ############################
load("output/Optim.Out0830.Rda")
cost<-read.csv("output/Optimal/All_ICER_2030_0830.csv", stringsAsFactors = F)

sum(cost$sum_increment)/8/1e9

##reaches goal w/ 4.5% scale up
nrow(goal.q30.out%>%filter(s_inc==0.045, s_covid==0, Reaches=="Yes"))
nrow(goal.q30.out%>%filter(s_inc==0.045, s_covid==0.5, Reaches=="Yes"))
nrow(goal.q30.out%>%filter(s_inc==0.045, s_covid==1, Reaches=="Yes"))
round(100*nrow(goal.q30.out%>%filter(s_inc==0.045, s_covid==2, Reaches=="Yes"))/123)


#################################################################
#intersectoral only
#################################################################
load("output/intersectoral_output.Rda")
load("../new_inputs/PreppedData0819.Rda")
pop<-wpp.in%>%filter(year==2019)%>%group_by(location_name)%>%summarise(pop=sum(Nx))
regions<-read.csv("../new_inputs/Country_groupings.csv")%>%select(c(Country, NCD_region))%>%rename(location_name = Country)


tableA8<-left_join(q30.code%>%filter(n.inters==1), pop, by="location_name")
tableA8<-left_join(tableA8, regions, by="location_name")

reg<-tableA8%>%group_by(NCD_region)%>%summarise(q2015=weighted.mean(`40q30_15`, pop), q2030 = weighted.mean(`40q30_2030_adj`, pop))
global<-tableA8%>%summarise(q2015=weighted.mean(`40q30_15`, pop), q2030 = weighted.mean(`40q30_2030_adj`, pop))%>%
  mutate(NCD_region = "All low- and middle-income countries")

tableA8<-rbind(reg, global)
tableA8<-tableA8[c(4,1,2,7,6,3,5,8),]
tableA8$percent_Change<-100*(tableA8$q2030-tableA8$q2015)/tableA8$q2015
write.csv(tableA8, "Figures/Appendix-tableA8.csv")

#extra tableA3
dadt.all.opt%>%filter(byear==2015)%>%summarise(DA=sum(Deaths.Avert))%>%pull(DA)/1000
dalys.opt%>%filter(byear==2015)%>%summarise(DA=sum(DALY.ave))%>%pull(DA)/1000000
left_join(q30.sel%>%filter(n.inters==1), pop, by="location_name")%>%summarise(change=weighted.mean(mChange15, pop))%>%pull(change)

load("output/intersectoral_output_salt.Rda")
dadt.all.opt%>%filter(byear==2015)%>%summarise(DA=sum(Deaths.Avert))%>%pull(DA)/1000
dalys.opt%>%filter(byear==2015)%>%summarise(DA=sum(DALY.ave))%>%pull(DA)/1000000
left_join(q30.sel%>%filter(n.inters==1), pop, by="location_name")%>%summarise(change=weighted.mean((`40q30_2030_adj` - `40q30_2030_base`), pop))%>%pull(change)

load("output/intersectoral_output_transfat.Rda")
dadt.all.opt%>%filter(byear==2015)%>%summarise(DA=sum(Deaths.Avert))%>%pull(DA)/1000
dalys.opt%>%filter(byear==2015)%>%summarise(DA=sum(DALY.ave))%>%pull(DA)/1000000
left_join(q30.sel%>%filter(n.inters==1), pop, by="location_name")%>%summarise(change=weighted.mean((`40q30_2030_adj` - `40q30_2030_base`), pop))%>%pull(change)

load("output/intersectoral_output_alcpolicy.Rda")
dadt.all.opt%>%filter(byear==2015)%>%summarise(DA=sum(Deaths.Avert))%>%pull(DA)/1000
dalys.opt%>%filter(byear==2015)%>%summarise(DA=sum(DALY.ave))%>%pull(DA)/1000000
left_join(q30.sel%>%filter(n.inters==1), pop, by="location_name")%>%summarise(change=weighted.mean((`40q30_2030_adj` - `40q30_2030_base`), pop))%>%pull(change)

load("output/intersectoral_output_alctax.Rda")
dadt.all.opt%>%filter(byear==2015)%>%summarise(DA=sum(Deaths.Avert))%>%pull(DA)/1000
dalys.opt%>%filter(byear==2015)%>%summarise(DA=sum(DALY.ave))%>%pull(DA)/1000000
left_join(q30.sel%>%filter(n.inters==1), pop, by="location_name")%>%summarise(change=weighted.mean((`40q30_2030_adj` - `40q30_2030_base`), pop))%>%pull(change)

load("output/intersectoral_output_smokingtax.Rda")
dadt.all.opt%>%filter(byear==2015)%>%summarise(DA=sum(Deaths.Avert))%>%pull(DA)/1000
dalys.opt%>%filter(byear==2015)%>%summarise(DA=sum(DALY.ave))%>%pull(DA)/1000000
left_join(q30.sel%>%filter(n.inters==1), pop, by="location_name")%>%summarise(change=weighted.mean((`40q30_2030_adj` - `40q30_2030_base`), pop))%>%pull(change)
