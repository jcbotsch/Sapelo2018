#Load Packages
library(tidyverse)
library(readxl)

#looking at snails
PrelimSnails=read_csv("./Data/NDVi_Snails22Oct18.csv")
SnailMorph=read_xlsx("./Data/Botsch_Sapelo_Snail_MASTER.xlsx", sheet = 2)

PrelimSnails=PrelimSnails%>%
  mutate(Snail=str_split_fixed(file, "_",2)[,1],
         Snail=str_replace(Snail, ".jpg",""),
         position=str_split_fixed(file, "_",2)[,2],
         position=str_replace(position, ".jpg", ""))
  
  
Snailmean=PrelimSnails%>%
  group_by(Snail)%>%
  summarise(mean.NDVI=mean(NDVI))%>%
  left_join(SnailMorph%>%mutate(Snail=paste0(ifelse(nchar(Snail)==2,"0","00")), by=c("Snail")))
  
Snail=left_join(PrelimSnails, SnailMorph%>%mutate(Snail=paste0(ifelse(nchar(Snail)==2,"0","00"), Snail)))


PrelimSnails%>%
  ggplot(aes(x=Location, y=NDVI, col=Snail))+
  geom_jitter(alpha=0.4, size=3.5)+
  geom_errorbar(aes(ymax=NDVI_max, ymin=NDVI_min), width=0)+
  theme_bw()


Snail%>%
  ggplot(aes(x=Location, y=NDVI, col=Snail))+
  geom_jitter(alpha=0.4, size=3.5)+
  theme_bw()

Snail%>%
  ggplot(aes(x=Location, y=NDVI, col=position))+
  geom_point(position= position_dodge(width=0.5), alpha=0.4, size=3.5)+
  # geom_errorbar(aes(ymax=NDVI_max, ymin=NDVI_min), position= position_dodge(width=0.5), width=0)+
  theme_bw()

summary(lm(Snail$NDVI~Snail$Location))

m=lmer(NDVI~Location+(1|Snail), data=Snail)
summary(m)

Anova(m, test="F", type="3")
