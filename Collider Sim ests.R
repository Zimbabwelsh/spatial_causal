library(tidyverse)
library(ggplot2)


set.seed(1337)
pop <- 5000
greenspace <- rnorm(pop)
mh <- rnorm(pop)
studied <- as.numeric(greenspace+(1.5*mh)>2.5)
sel=rbinom(pop, 1, (0.6*studied))
sel1 <- sel==1

table(studied, sel1)

info <- ifelse(sel==1, "Study Participants", "Population" )

data <- data.frame(greenspace, mh, studied, sel, sel1,  info)

model1 <- lm(mh~greenspace)
model2 <- lm(mh[sel1]~greenspace[sel1])
model3 <- lm(mh[studied==1]~greenspace[studied==1])


colors <- c("#BDBDBD", "#424242")

studyplot <- ggplot(data %>% arrange(info))+
  geom_point(aes(x=greenspace, y=mh, color=info))+ 
  geom_abline(slope=summary(model1)$coef[2,1], intercept = summary(model1)$coef[1,1],  size=1.1, linetype="dashed")+
  geom_abline(slope=(summary(model3)$coef[2,1]), intercept = (summary(model3)$coef[1,1]),  size=1.1, linetype="dashed")+
  theme_bw()+
  theme(legend.title=element_blank(),
        legend.text=element_text(size=14),
        axis.title = element_text(size=14),
        legend.key.size = unit(3, "line")) +
  ylim(-4,4)+
  xlim(-4,4)+
  # scale_fill_manual(values=colors, name="info",labels=c("Population", "Study Participants"))+
  scale_colour_manual(values=colors)+
  xlab("Neighbourhood Green Space")+
  ylab("Individual Mental Health")+
  guides(color=guide_legend(override.aes=list(size=4)))
  
studyplot

ggsave("studyplot.png", dpi=360)
