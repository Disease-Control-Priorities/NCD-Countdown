
library(ggplot2)

load("goal.q30.Rda")

g<-ggplot(summary, aes(x=s_covid, y=s_inc*100, fill=adj.diff))+
  geom_tile()+
  geom_text(aes(label=n.reaches), color="black")+
  scale_fill_gradientn(colors=c("#3c1f5c","#33628e", "#2d7b8d","#20a386", "#fde725"), values=c(0,0.1,1))+
  labs(fill="Excess NCD deaths")+
  ylab("Annual absolute increase in coverage (%)")+
  xlab("Covid adjustment factor")+
  ggtitle("Global excess NCD deaths and the number of countries \nachieving a one-third reduction in 40q30 \nby each covid and intervention scale-up scenario")+
  labs(caption = "*Text numbers represent number of countries achieving 40q30 target")

ggsave("testplot.png", g)
