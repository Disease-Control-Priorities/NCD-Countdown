
library(stringr)
library(ggforce)
library(dplyr)
library(readxl)
library(tinytex)
library(tidyr)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

load("output/goal.q30.covid_0819.Rda")

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

#add splines
#p1<-plot3%>%filter(scenario=="Most pessimistic", location_name=="Brazil")
#spline1 <- as.data.frame(spline(p1$year_id, p1$Adjusted))
#spline1$scenario<-"Most pessimistic"
brazil_target<-goal.q30.out%>%filter(location_name=="Brazil")%>%pull(Target_40q30)
library(ggplot2)
library(ggforce)

ggplot(plot3%>%filter(location_name=="Brazil"), aes(x=year_id, y=Adjusted, color=scenario, group=scenario))+
  geom_point()+
  geom_line()+
  #stat_smooth(aes(x=year_id, y=Adjusted), formula =y~poly(x,6), method="lm", se=F)
  #stat_smooth(aes(x=year_id, y=Adjusted), formula=y~s(x,k=10), method="gam", se=F)+
  geom_line(y=brazil_target[1], color="darkgrey", lty=2)+
  ylab("40q30 (%)")+
  xlab("Year")+
  ggtitle("Projected 40q30 over time, Brazil")+
  labs(color="Scenario")+
  coord_cartesian(ylim=c(0,40))+
  scale_color_manual(values=c("#cc415a",   "#057fee", "#22a884"))
#annotate(geom="text", x=2027, y=brazil_target-2, label="Target: 1/3 reduction in 40q30", color="darkgrey")

## Population-weighted average for global plot
load("../new_inputs/PreppedData0803.Rda")
pop<-wpp.in%>%filter(year==2019)%>%group_by(location_name)%>%summarise(pop=sum(Nx))

plot3<-left_join(plot3, pop, by="location_name")
global<-plot3%>%group_by(scenario,year_id)%>%summarise(q30=weighted.mean(Adjusted, pop))

goal.q30.out<-left_join(goal.q30.out, pop, by='location_name')
global_target<-weighted.mean(goal.q30.out$Target_40q30, goal.q30.out$pop)

ggplot(global, aes(x=year_id, y=q30, color=scenario, group=scenario))+
  geom_point()+
  geom_line()+
  geom_line(y=global_target, color="darkgrey", lty=2)+
  ylab("40q30 (%)")+
  xlab("Year")+
  ggtitle("Projected 40q30 over time, Global")+
  labs(color="Scenario")+
  coord_cartesian(ylim=c(0,40))+
  scale_color_manual(values=c("#cc415a",   "#057fee", "#22a884"))
#annotate(geom="text", x=2027, y=global_target-2, label="Target: 1/3 reduction in 40q30", color="darkgrey")

ggsave("Figures/Target_figure0819.png", height = 7, width = 9, units = "in")


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


ggsave("Figures/Figure4_0819_2.png", height = 7, width = 9, units = "in")


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

ggsave("Figures/Figure4_0819_3.png", height = 7, width = 9, units = "in")

#ggsave("Figures/Figure4_0815_difference.png", height = 7, width = 9, units = "in")



##countries with increasing 40q30?

  ggplot(all.q30.out%>%filter(s_covid==1, s_inc==0.025),aes(x=year_id, y=Adjusted, group=location_name))+
  geom_line()+
  ylab("Adjusted 40q30")

library(plotly)
regions<-read.csv("../new_inputs/Country_groupings.csv")%>%select(c(Country, NCD_region))%>%rename(location_name = Country)
  

plot_ly(plot%>%filter(s_covid==1, s_inc==0.025), 
        x=~year_id, 
        y=~Adjusted, 
        mode='lines',
        type='scatter',
        color=~location_name,
        line=list(width=1)
        )


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
plot$label[plot$location_name=="Mauritius"& plot$year_id==2030]<-"Mauritius"

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

p<-ggplot(plot%>%filter(s_covid==1, s_inc==0.025, NCD_region=="Sub-Saharan Africa"), 
          aes(x=year_id, y=diff, group=location_name))+
  geom_line()+
  ylab("Change in 40q30")+
  facet_wrap(~NCD_region)+
  geom_text(aes(label=label), vjust=-1)+
  geom_line(y=1, color="red", lty=2)+
  geom_line(y=0.67, color="#009FD7", lty=2)


p

#ggsave("Figures/adjusted40q30_SSA.png")


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

ggplot(plot2, 
          aes(x=year_id, y=diff, group=NCD_region, color=NCD_region))+
  geom_line()+
  ylab("Change in 40q30 from 2015")+
  geom_line(y=1, color="grey", lty=2)+
  geom_line(y=0.67, color="grey", lty=2)+
  theme_minimal()+
  xlab("Year")

ggsave("Figures/Figure_spaghetti.png", height = 7, width = 9, units = "in")


###MALAWI numbers###
uc<-read.csv("output/costs/bycountry/Malawi_adjusted_uc_2017.csv")
cov<-read.csv("../new_inputs/Coverage0621.csv")

cost<-uc%>%group_by(NCD, Intervention)%>%summarize(Adjusted_UC = sum(adjusted_uc))
coverage<-cov%>%filter(Country=="Malawi")%>%select(-c(WB_region15, GBD_region, Country, iso3, LocID))%>%
  gather(Code, Baseline_coverage)%>%mutate(NCD=as.numeric(sub('X', '', Code)))

malawi<-left_join(cost, coverage, by="NCD")

write.csv(malawi[,c(2,3,5)], "Figures/Malawi_adjusted_unit_costs.csv", row.names = F)

load("output/Optim.Out0803.Rda")

n.inters<-q30.CodeDelta%>%filter(location_name=="Malawi", byear==2015)%>%pull(n.inters)
inters.total<-15
inc.annual<-read.csv("output/Optimal/All_Intervention_2030_0803.csv")%>%filter(Country=="Malawi")
