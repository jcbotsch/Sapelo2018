#Load Packages
library(tidyverse)
library(readxl)
library(car)
library(lme4)
library(stringr)

#looking at snails
PrelimSnails=rbind(read_csv("./Data/NDVi_25Oct18.csv"),read_csv("./Data/NDVi_2324Oct18.csv"), read_csv("./Data/NDVi_Snails22Oct18.csv"), read_csv("./Data/NDVi_26Oct18.csv"))
SnailMorph=read_xlsx("./Data/Botsch_Sapelo_Snail_MASTER.xlsx", sheet = 2)

PrelimSnails=PrelimSnails%>%
  mutate(Snail=str_split_fixed(file, "_",2)[,1],
         Snail=as.numeric(str_replace(Snail, ".jpg","")),
         position=str_split_fixed(file, "_",2)[,2],
         position=str_replace(position, ".jpg", ""))
  
PrelimSnails=PrelimSnails%>%
  filter(Snail!="MSnail",
          !grepl("wet", position))


SnailMorph=SnailMorph%>%
  mutate_at(vars(starts_with("Shell")),funs(as.numeric))%>%
  mutate(radius=(Shell.Width+Shell.Length)/2,
          area= radius*pi*(radius+sqrt(Shell.Height^2+radius^2)))
  
Snailmean=PrelimSnails%>%
  group_by(Snail)%>%
  summarise(mean.NDVI=mean(NDVI))%>%
  left_join(SnailMorph, by=c("Snail"))
  
Snail=left_join(PrelimSnails, SnailMorph, by=c("Snail"))%>%
  filter(!is.na(Location))


Snail%>%
  ggplot(aes(x=Location, y=NDVI, col=factor(Site)))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3.5)+
  theme_bw()


Snail%>%
  ggplot(aes(x=Location, y=NDVI, col=factor(position)))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3.5)+
  theme_bw()

Snailmean%>%
  ggplot(aes(x=Location, y=mean.NDVI, col=factor(Site)))+
  geom_jitter(position=position_jitterdodge(), alpha=0.4, size=3.5)+
  theme_bw()

Snail%>%
  ggplot(aes(x=Location, y=NDVI, col=position))+
  geom_point(position= position_jitterdodge(), alpha=0.4, size=3.5)+
  # geom_errorbar(aes(ymax=NDVI_max, ymin=NDVI_min), position= position_dodge(width=0.5), width=0)+
  theme_bw()

Snail%>%
  ggplot(aes(x=area, y=NDVI, col=Location))+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth(method="lm", se = FALSE)+
  theme_bw()

Snail%>%
  ggplot(aes(x=area, y=NDVI, col=Location, shape=position))+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth(aes(lty=position), method="lm", se = FALSE)+
  xlab(bquote('Area (' ~cm^2*')'))+
  theme_bw()

Snailmean%>%
  ggplot(aes(x=area, y=mean.NDVI, col=Location))+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth( method="lm", se = FALSE)+
  xlab(expression('Area ('* "cm"^2*')'))+
  theme_bw()

summary(lm(Snailmean$mean.NDVI~Snailmean$area*Snailmean$Location))

summary(lm(Snail$NDVI~Snail$Location))

m=lmer(NDVI~Location*position+(1|Snail), data=Snail)
summary(m)

Anova(m, test="F", type="2")
