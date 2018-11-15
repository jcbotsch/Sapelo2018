#====General information====
#Questions:
#1. How does the algal biomass of communities differ between snails near the creek and far from the creek?
#2. Does the location of algae differ between snails near the creek and far from the creek?
#3. Does snail morphology drive epizoic algal biomass?

#====Load Packages====
library(tidyverse)
library(readxl)
library(car)
library(lme4)
library(stringr)
library(MuMIn)

#====Read in Data====
PrelimSnails=rbind(read_csv("./Data/NDVi_25Oct18.csv"), #combine all sheets with NDVI values
                   read_csv("./Data/NDVi_2324Oct18.csv"), 
                   read_csv("./Data/NDVi_Snails22Oct18.csv"), 
                   read_csv("./Data/NDVi_26Oct18.csv")) 
#The these files were generated using the "IRProcessing.R" script
SnailMorph=read_xlsx("./Data/Botsch_Sapelo_Snail_MASTER.xlsx", 
                  sheet = 2) #Info about individual snails
SiteInfo=read_xlsx("./Data/Botsch_Sapelo_Snail_MASTER.xlsx", 
                  sheet = 3) #Info about sites
SpartinaInfo=read_xlsx("./Data/Botsch_Sapelo_Snail_MASTER.xlsx", 
                  sheet = 4) #I took a couple measurements of the spartina at some of the sites. 

#Set Site Colors for plots with fewer than 5 Sites
Sitecol=c(
  "1"="#F8766D",
  "2"="#A3A500",
  "3"="#00BF7D",
  "4"="#00B0F6",
  "5"="#E76BF3"
)

#====Prep Data for Analyses====

#====Prep: Snail Data====
#Extract unique snail id from filenames
PrelimSnails=PrelimSnails%>%
  mutate(Snail=str_split_fixed(file, "_",2)[,1], #split file to return the thing before _
         position=str_split_fixed(file, "_",2)[,2], #return the second column in the character
         position=str_replace(position, ".jpg", "")) #remove .jpg at end

#remove snails that are not part of the study 
PrelimSnails=PrelimSnails%>%
  filter(Snail!="MSnail", #a very green snail Marrissa found
         !grepl("wet", position),
         !grepl("Tile", Snail)) #I did a test to see if the wetness affected the measured NDVI It did.

#Average Snail NDVI
Snailmean=PrelimSnails%>%
  group_by(Snail)%>% 
  summarise(mean.NDVI=mean(NDVI))%>% #average NDVI from top and bottom
  ungroup

#====Prep: Snail Morphology====
SnailMorph=SnailMorph%>%
  mutate_at(vars(starts_with("Shell")),funs(as.numeric))%>% #make all columns beginning with shell numeric. (It read in as a character b/c NAs)
  mutate(Snail=as.character(Snail), #change Snail from numeric to character
         Site=as.character(Site),
         radius=(Shell.Width+Shell.Length)/4, #average the two measurements of snail base and divided by two to get radius
         area= radius*pi*(radius+sqrt(Shell.Height^2+radius^2)))%>% #calculate the area of circular cone with those dimensions
  select(Snail, Location, Site, area)


#Join with Prelim Snails
Snail=PrelimSnails%>%
  select(Snail, position, NDVI)%>% 
  mutate(Snail=as.character(as.numeric(Snail)))%>%
  left_join(SnailMorph)%>%
  filter(!is.na(Location))%>%
  arrange(as.numeric(Snail))


#Join with Averaged Snail data
Snailmean=Snailmean%>%
  mutate(Snail=as.character(as.numeric(Snail)))%>%
  left_join(SnailMorph, by=c("Snail"))%>%
  filter(!is.na(Location))

#====Prep: inverse variance to weight models====

#Top and bottom
Snailvariance=Snail%>%
  group_by(Site, Location)%>%
  summarise(var=1/var(NDVI))%>%
  ungroup%>%
  filter(!is.na(Site))

#Averaged data
Smvariance=Snailmean%>%
  group_by(Site, Location)%>%
  summarise(var=1/var(mean.NDVI))%>%
  ungroup%>%
  filter(!is.na(Site))

#====Prep: Site Information====
Sites=SiteInfo%>%
  mutate(
    Site=as.character(Site)
  )%>%
  group_by(Site, Location)%>%
  summarise(
    Avg.Stalks=mean(No.Stalks)/0.09, #Average number of stalks in 1m^2
    Avg.Snails=mean(No.Snails)/0.09 #Average number of Snails in 1m^2
  )%>%
  ungroup

#====Prep: Join Site info with Snail Info, stalk height====
Snail = Snail%>%
  left_join(Sites%>%mutate(Site=as.character(Site)), by=c("Site", "Location"))

Snailmean = Snailmean%>%
  left_join(Sites%>%mutate(Site=as.character(Site)), by=c("Site", "Location"))%>%
  left_join(Smvariance, by=c("Site","Location"))

#====Site Info====

#===SI1: Do sites differ near the creek to far from the creek?====

#plot average number of stalks and snails in 1m2
Sites%>%
  gather(key=var, value=value, Avg.Stalks, Avg.Snails)%>%
  mutate(var1=factor(var, labels = c(expression(italic("Littorina irrorata")),
                                     expression(italic("Spartina alterniflora")))))%>%
  ggplot(aes(x=Location, y=value, col=Site))+
  facet_wrap(~var1, labeller = label_parsed, scales="free_y")+
  geom_point(size=3, alpha=0.8)+
  geom_line(aes(group=Site), alpha=0.8)+
  ylab(expression("Number of Individuals in 1m"^2))+
  xlab("Proximity to Creek")+
  scale_color_manual(values=Sitecol)+
  theme_bw()+
  theme(legend.position = "bottom")

#Spartina Height
SpartinaInfo%>%
  mutate(Site=as.character(Site))%>%
  ggplot(aes(x=Location, y=Height.Stalk, col=Site))+
  geom_point(position=position_jitterdodge(), size=3, alpha=0.8)+
  geom_boxplot(alpha=0)+
  ylab(expression(paste(italic("Spartina"),"  Height (cm)", sep=" ")))+
  xlab("Proximity to Creek")+
  scale_color_manual(values=Sitecol)+
  theme_bw()+
  theme(legend.position = "bottom")


#convert to wide format for t tests
WideSite=SiteInfo%>%
  select(Site, Subplot, Location, No.Stalks, No.Snails)%>%
  gather(key=var, value=value, No.Stalks, No.Snails)%>%
  spread(key=Location, value = value)

t.SiteSnail=WideSite%>%filter(var=="No.Snails")
t.SiteStalks=WideSite%>%filter(var=="No.Stalks")

st=SpartinaInfo%>%
  group_by(Site, Location, Subplot)%>%
  summarise(sh=mean(Height.Stalk))%>%
  spread(Location, sh)

#Wilcoxon signed rank test
ttest=function(a, b){
  wilcox.test(a$Far,a$Near, paired = b, exact = FALSE)
}
#FALSE indicates not paired Wilcoxon rank sum test with continuity correction
ttest(t.SiteSnail, FALSE) #W=217.5, p=1.371e-5
ttest(t.SiteStalks, FALSE) #W=70.5, p=0.08279
#TRUE indicates paired Wilcoxon ranked sum test with continuity correction
ttest(st, TRUE) #V=2, p=0.01782 

#distances differ significantly in number of snails and stalk height 

#====SI2: Do sites differ in these characters?====
#ANOVA to compare whether there are site differences in these patterns

saov=function(a,b){
  summary(aov(a~b))
}

saov(SiteInfo$No.Snails, factor(SiteInfo$Site)) #F=1.228, p=0.324
saov(SiteInfo$No.Stalks, factor(SiteInfo$Site)) #F=1.226, p=0.31
saov(SpartinaInfo$Height.Stalk, SpartinaInfo$Site) #F=2.619, p=0.109

#====Question 1: Does algal biomass differ between snails found near the creek and far from the creek?====
#====Q1: Plots====
Snailmean%>%
  ggplot(aes(x=Location, y=mean.NDVI, col=factor(Site)))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3)+
  stat_summary(aes(group=Site), geom = "line", fun.y =mean, position=position_jitterdodge(), size=1, alpha=0.8)+
  xlab("Proximity to Creek")+
  ylab("Average ENDVI of Snail Shell")+
  labs(col="Site")+
  theme_bw()+
  theme(legend.position = "bottom")

#====Q1: Analyses====
#confirm normality 
qqPlot(Snailmean$mean.NDVI)#looks roughly along the line
shapiro.test(Snailmean$mean.NDVI) #p=0.3465 cannot reject H0 that data are non-normal.

#Mixed effects model- is ndvi driven by location, allow slope and intercept to vary by site
m1=lmer(mean.NDVI~Location+(Location|Site),
        data=Snailmean)

summary(m1) #slope estimate: far--> near sites -0.02430 
plot(m1) #residulas look fairly good.
Anova(m1, type="3") #chisq=3.8594, p=0.04947
r.squaredGLMM(m1) #pseudo R2: marginal R2= variance explained by fixed effects= 0.026 Conditional R2= variance explained by model=0.461

m1.red=lmer(mean.NDVI~Location+(1|Site),
            data=Snailmean)

r.squaredGLMM(m1.red) #same r2m r2c=0.4383. slope does explain some of variation, but not crazy amounts.

#fixed efffects model- looking for interaction between location and snail
m1.fix=lm(mean.NDVI~Location*Site,
          data=Snailmean)

summary(m1.fix) #when combined with Site 1 (i.e. LocationNear slope is based on LocationNear slope for site 1) all negative, but 3 just barely
Anova(m1.fix, type="3") #Location no longer significant, Site highly sig p=2.71e-13, Location:Site p=0.006246


#====Question 2: Does the location of algal biomass differ between the snails found near and far from the creek?==== 
#====Q2: Plots====
#Plot paired snail top and bottom of snails
Snail%>%
  select(Snail, Location, Site, position, NDVI)%>%
  mutate(Site=as.factor(Site))%>%
  spread(key = position, value = NDVI)%>% #convert to wide format
  ggplot(aes(x=bottom, y=top, col=Site))+
  facet_grid(~Location)+
  geom_point(alpha=0.4, size=3.5)+
  geom_abline(slope=1)+
  xlab("ENDVI of Underside of Snail Shell")+
  ylab("ENDVI of Top of Snail Shell")+
  xlim(-0.15,0.5)+ #set limits as equal
  ylim(-0.15,0.5)+
  coord_equal()+ #make plots square
  theme_bw()+
  theme(legend.position="bottom")

Snail%>%
  ggplot(aes(y=NDVI, x=position, col=Location))+
  facet_wrap(~Site)+
  geom_point(position=position_jitterdodge())+
  stat_summary(aes(group=interaction(Location, Site)), geom = "line", fun.y =mean, size=1, position=position_jitterdodge(), alpha=0.8)
  

#====Q2: Analyses====

#confirm normality
hist(Snail$NDVI) #fairly good. There's a slight skew.
qqPlot(Snail$NDVI)#looks pretty good.
shapiro.test(Snail$NDVI) #p=0.0006 reject null hypothesis. Data are non-normal

#Log +1 transform and try again.
hist(log1p(Snail$NDVI)) #looks better.
qqPlot(log1p(Snail$NDVI)) #looks alright still.
shapiro.test(log1p(Snail$NDVI)) #p=0.1304. Data are not significantly non-normal. 

m2 = lmer(log1p(NDVI)~Location*position+Avg.Stalks*position+
          (Location|Site)+(1|Snail),
     data=Snail)
summary(m2)
Anova(m2)
r.squaredGLMM(m2)

#====Question 3: Does the algal biomass differ on snails of different sizes?====
#====Q3:Plot====
Snail%>%
  ggplot(aes(x=area, y=NDVI, col=Location))+
  facet_wrap(~position, nrow=2)+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth(method="lm", se = FALSE)+
  xlab(bquote('Shell Area (' ~mm^2*')'))+
  ylab("ENDVI")+
  theme_bw()

Snail%>%
  select(Snail, Location, Site, position, NDVI, area)%>%
  mutate(Site=as.factor(Site))%>%
  spread(key = position, value = NDVI)%>% #convert to wide format
  mutate(NDVI.diff=bottom-top)%>%
  ggplot(aes(x=area, y=NDVI.diff, col=Location))+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth(method="lm")+
  theme_bw()+
  theme(legend.position="bottom")



#====Q3:Analyses====
Anova(lmer(log1p(NDVI)~Location*position*area+(1|Snail),
   data=Snail), type="3")
