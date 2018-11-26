#Load Packages
library(tidyverse)
library(readxl)
library(car)
library(lme4)
library(stringr)
library(MuMIn)

#looking at snails
PrelimSnails=rbind(read_csv("./Data/NDVi_25Oct18.csv"),read_csv("./Data/NDVi_2324Oct18.csv"), read_csv("./Data/NDVi_Snails22Oct18.csv"), read_csv("./Data/NDVi_26Oct18.csv"))
SnailMorph=read_xlsx("./Data/Botsch_Sapelo_Snail_MASTER.xlsx", sheet = 2)
SiteInfo=read_xlsx("./Data/Botsch_Sapelo_Snail_MASTER.xlsx", sheet = 3)
SpartinaInfo=read_xlsx("./Data/Botsch_Sapelo_Snail_MASTER.xlsx", sheet = 4)

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
  mutate(radius=(Shell.Width+Shell.Length)/4,
          area= radius*pi*(radius+sqrt(Shell.Height^2+radius^2)))
  
Snailmean=PrelimSnails%>%
  group_by(Snail)%>%
  summarise(mean.NDVI=mean(NDVI))%>%
  left_join(SnailMorph, by=c("Snail"))
  
Snail=left_join(PrelimSnails, SnailMorph, by=c("Snail"))%>%
  filter(!is.na(Location))


Snail%>%
  ggplot(aes(x=Location, y=NDVI, col=factor(Site), shape=position))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3.5)+
  scale_shape_manual(values = c(16,1))+
  xlab("Proximity to Creek")+
  theme_bw()


Snail%>%
  select(Snail, Location, Site, position, NDVI)%>%
  mutate(Site=as.factor(Site))%>%
  spread(key = position, value = NDVI)%>%
  ggplot(aes(x=bottom, y=top, col=Site))+
  facet_grid(~Location)+
  geom_point(alpha=0.4, size=3.5)+
  scale_shape_manual(values = c(16,1))+
  geom_abline(slope=1)+
  xlab("ENDVI of Underside of Snail Shell")+
  ylab("ENDVI of Top of Snail Shell")+
  xlim(-0.15,0.5)+
  ylim(-0.15,0.5)+
  theme_bw()

Snail%>%
  ggplot(aes(x=Location, y=NDVI, col=factor(position)))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3.5)+
  theme_bw()

Snailmean%>%
  filter(!is.na(Location))%>%
  ggplot(aes(x=Location, y=mean.NDVI, col=factor(Site)))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3.5)+
  geom_boxplot(position = position_jitterdodge(), alpha=0)+
  theme_bw()

Snail%>%
  ggplot(aes(x=Location, y=NDVI, col=position))+
  geom_point(position= position_jitterdodge(), alpha=0.4, size=3.5)+
  # geom_boxplot(position=position_jitterdodge(), alpha=1)+
  geom_errorbar(aes(ymax=NDVI_max, ymin=NDVI_min, group=Snail), position= position_dodge(width=0.5), width=0, alpha=0.2)+
  theme_bw()

Snail%>%
  ggplot(aes(x=area, y=NDVI, col=Location))+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth(method="lm", se = FALSE)+
  xlab(bquote('Shell Area (' ~mm^2*')'))+
  theme_bw()

Snail%>%
  ggplot(aes(x=area, y=NDVI, col=Location))+
  facet_wrap(~position, nrow=2)+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth(method="lm", se = FALSE)+
  xlab(bquote('Shell Area (' ~mm^2*')'))+
  theme_bw()

Snailmean%>%
  ggplot(aes(x=area, y=mean.NDVI, col=Location))+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth( method="lm", se = FALSE)+
  ylab("Mean ENDVI of Snail Shell")+
  xlab(expression('Shell Area ('* "mm"^2*')'))+
  theme_bw()





Anova(lm(log1p(Snailmean$mean.NDVI)~factor(Snailmean$Site)*Snailmean$Location), type="3")


m1=lmer(mean.NDVI~Location*Avg.Stalks +(1|Site),
   weights=var,
   data=SiteSnail)

summary(m1)

Anova(m1)
#weighted lm
#calculate variance (sd) weights =1-sd
summary(aov(NDVI~Location*position, data=Snail))

TukeyHSD(aov(NDVI~Location*position, data=Snail))

summary(lm(Snail$NDVI~Snail$Location))

m=lmer(NDVI~Location*position+(1|Site), data=Snail) #maybe wrong
summary(m)

Anova(m, test="F", type="3")

#With Site Info
Sites=SiteInfo%>%
  mutate(
    Site=factor(Site)
  )%>%
  group_by(Site, Location)%>%
  summarise(
    Avg.Stalks=mean(No.Stalks),
    Avg.Snails=mean(No.Snails)
  )%>%
  ungroup


variance=Snailmean%>%
  group_by(Site, Location)%>%
  summarise(var=1/var(mean.NDVI))%>%
  ungroup%>%
  mutate(Site=factor(Site))

SiteSnail=Snailmean%>%
  mutate(Site=factor(Site))%>%
  left_join(variance,  by=c("Site", "Location"))%>%
  left_join(Sites,  by=c("Site", "Location"))%>%
  group_by(Site)



SiteSnail%>%
  ggplot(aes(y=mean.NDVI, x=Avg.Snails))+
  geom_point(aes(col=Site), size=3, alpha=0.3)+
  theme_bw()

SiteSnail%>%
  ggplot(aes(y=mean.NDVI, x=Avg.Stalks/0.09))+
  geom_point(aes(col=Site), size=3, alpha=0.8)+
  theme_bw()

#modified ricker to include an intercept.
n1=nls(mean.NDVI~c+a*Avg.Snails*exp(b*Avg.Snails),
    start=list(a=1,b=0.05, c=0.05),
    weights=var,
    data=SiteSnail)
summary(n1)

prediction=data.frame(Avg.Snails=seq(0,32), mean.NDVI=NA)
prediction$mean.NDVI=predict(n1, newdata = prediction)

SiteSnail%>%
  ggplot(aes(y=mean.NDVI, x=Avg.Snails/0.09))+
  geom_point(aes(col=Site), size=3, alpha=0.8)+
  geom_line(data=prediction)+
  xlab(expression("Average number of snails in 1 m"^{2}))+
  ylab("Mean NDVI of Snail Shell")+
  theme_bw()


SiteSnail%>%
  ggplot(aes(y=Avg.Snails, x=Location))+
  geom_point(aes(col=Site), size=3, alpha=0.8)+
  theme_bw()

SpartinaInfo%>%
  ggplot(aes(x=Height.Stalk))+
  facet_wrap(~interaction(Site, Location))+
  geom_histogram(stat="count")


st=SpartinaInfo%>%
  group_by(Site, Location, Subplot)%>%
  summarise(sh=mean(Height.Stalk))%>%
  spread(Location, sh)

t.test(st$Near,st$Far, alternative = "two.sided", paired=FALSE)


SpartinaInfo%>%
  ggplot(aes(x=Location, y=Height.Stalk))+
  geom_jitter(aes(col=factor(Site)), size=3, alpha=0.8)+
  geom_boxplot(fill=NA)+
  theme_bw()

SiteInfo%>%
  ggplot(aes(x=Location, y=No.Snails))+
  geom_jitter(aes(col=factor(Site)), size=3, alpha=0.8)+
  geom_boxplot(fill=NA)+
  theme_bw()

SiteInfo%>%
  ggplot(aes(x=Location, y=No.Stalks))+
  geom_jitter(aes(col=factor(Site)), size=3, alpha=0.8)+
  geom_boxplot(fill=NA, outlier.alpha = 0)+
  theme_bw()


stalk.t=SiteInfo%>%
  group_by(Site)%>%
  select(Site, Location, Subplot, No.Stalks)%>%
  spread(Location, No.Stalks)

t.test(stalk.t$Far, stalk.t$Near)

SiteSnail2=SpartinaInfo%>%
  mutate(Site=as.character(Site))%>%
  group_by(Site, Location)%>%
  summarise(height=mean(Height.Stalk))%>%
  ungroup%>%
  left_join(SiteSnail%>% select(Site, Location, Snail, mean.NDVI, Avg.Stalks)%>%ungroup%>%mutate(Site=as.character(Site)), by=c("Site", "Location"))


SiteSnail2%>%
  group_by(Site, Location, Avg.Stalks, height)%>%
  summarise(mean=mean(mean.NDVI))%>%
  ggplot(aes(x=Avg.Stalks, y=height, col=interaction(Site,Location)))+
  geom_point(aes(size=1.5*mean))

s2=Snail%>%
  select(Snail, Location, Site, position, NDVI)%>%
  mutate(Site=as.factor(Site))%>%
  spread(key = position, value = NDVI)

t.test(s2$bottom, s2$top)

wilcox.test(s2$bottom, s2$top, paired = TRUE)
