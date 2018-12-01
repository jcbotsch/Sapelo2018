#====General information====
#Questions:
#1. How does the algal biomass of communities differ between snails near the creek and far from the creek?
#2. Does the location of algae differ between snails near the creek and far from the creek?
#3. Does snail morphology drive epizoic algal biomass?

#====Load Packages====
library(tidyverse) #data prep, plotting, etc.
library(readxl) #read excel files
library(car) #Anova function for type 3 anovas.
library(stringr) #modify strings for data prep


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

#Set Site Colors for figures that contain fewer than 5 Sites
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


#Difference in ENDVI
SnailDiff=Snail%>%
  select(Snail, Location, Site, position, NDVI, area)%>%
  mutate(Site=as.factor(Site))%>%
  spread(key = position, value = NDVI)%>% #convert to wide format
  mutate(NDVI.diff=bottom-top)


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
  left_join(Sites%>%mutate(Site=as.character(Site)), by=c("Site", "Location"))

SnailDiff=SnailDiff%>%
  left_join(Sites%>%mutate(Site=as.character(Site)), by=c("Site", "Location"))

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

#select variables of interest
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
ttest(st, FALSE) #W=7.5, p=0.004069

#distances differ significantly in number of snails and stalk height 

#clean up
rm(t.SiteSnail, t.SiteStalks)

#====SI2: Do sites differ in these characters?====
#ANOVA to compare whether there are site differences in these patterns

saov=function(a,b){
  summary(aov(a~b))
}

saov(SiteInfo$No.Snails, factor(SiteInfo$Site)) #F=1.228, p=0.324
saov(SiteInfo$No.Stalks, factor(SiteInfo$Site)) #F=1.226, p=0.31
saov(SpartinaInfo$Height.Stalk, SpartinaInfo$Site) #F=2.619, p=0.109
saov(SpartinaInfo$Height.Stalk, SpartinaInfo$Location) #F=48.649, p=5.45e-10



#====Question 1: Does algal biomass differ between snails found near the creek and far from the creek?====
#====Q1: Plots====
Snailmean%>%
  ggplot(aes(x=Location, y=mean.NDVI, col=factor(Site)))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3)+
  stat_summary(aes(group=Site), geom = "line", fun.y =median, position=position_jitterdodge(), size=1, alpha=0.8)+
  geom_boxplot(alpha=0)+
  xlab("Proximity to Creek")+
  ylab("Average ENDVI of Snail Shell")+
  labs(col="Site")+
  theme_bw()+
  theme(legend.position = "bottom")

Snailmean%>%
  ggplot(aes(x=Avg.Stalks, y=mean.NDVI, col=factor(Site)))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3)+
  geom_hline(yintercept=0)+
  xlab(expression(paste("Average Number of", italic("Spartina")," Stalks in 1", m^2, sep=" ")))+
  ylab("AVerage ENDVI of Snail Shell")+
  labs(col="Site")+
  theme_bw()+
  theme(legend.position = "bottom")

#====Q1: Analyses====
#confirm normality 
qqPlot(Snailmean$mean.NDVI)#looks roughly along the line
shapiro.test(Snailmean$mean.NDVI) #p=0.3465 cannot reject H0 that data are non-normal.

#====Q1: Analyses: Location====
#fixed efffects model- looking for interaction between location and snail
m1.fix1=lm(mean.NDVI~Location*Site,
           data=Snailmean)

summary(m1.fix1)
Anova(m1.fix1, type="3") 
#                 Sum Sq  Df  F value    Pr(>F)    
#   Location      0.00140   1   0.4461  0.504727    
#   Site          0.22677   4  18.1134  2.71e-13
#   Location:Site 0.04590   4   3.6660  0.006246


#====Q1: Analyses: Number of Stalks====
m1.stalk=lmer(mean.NDVI~log(Avg.Stalks)+(log(Avg.Stalks)|Site),
        data=Snailmean,
        REML = FALSE)

summary(m1.stalk)
Anova(m1.stalk, type="3")
#                   Chisq Df Pr(>Chisq)
# (Intercept)     2.0800  1     0.1492
# log(Avg.Stalks) 0.1858  1     0.6664

r.squaredGLMM(m1.stalk) #R2m = 0.0005607244  R2c= 0.3574799


m1.stalk.red=lmer(mean.NDVI~log(Avg.Stalks)+(1|Site), #drop random slope
                  data=Snailmean,
                  REML = FALSE)

summary(m1.stalk.red)
r.squaredGLMM(m1.stalk.red) #r2c slightly better 0.357889



#fixed effects:Average stalks
m1.fix2=lm(mean.NDVI~Site*log(Avg.Stalks),
          data=Snailmean)

summary(m1.fix2)
Anova(m1.fix2, type="3") 
#                     Sum Sq  Df F value    Pr(>F)    
# Site                 0.09347   4  7.4660 9.778e-06
# log(Avg.Stalks)      0.00140   1  0.4461   0.50473    
# Site:log(Avg.Stalks) 0.09017   4  7.2023 1.531e-05


#Interaction Fixed effects
m1.fix3=lm(mean.NDVI~Location*Avg.Stalks,
           data=Snailmean)

summary(m1.fix3)
Anova(m1.fix3, type="3")
#                       Sum Sq  Df F value Pr(>F)
# (Intercept)         0.01079   1  2.2071 0.1384
# Location            0.00372   1  0.7602 0.3840
# Avg.Stalks          0.00000   1  0.0001 0.9944
# Location:Avg.Stalks 0.00097   1  0.1975 0.6571

anova(m1.fix, m1.fix2, m1.fix3)
#models including site  better AIC change=-128

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

SnailDiff%>%
  ggplot(aes(x=Location, y=NDVI.diff, col=factor(Site)))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3)+
  geom_boxplot(alpha=0)+
  stat_summary(aes(group=Site), geom = "line", fun.y =median, position=position_jitterdodge(), size=1, alpha=0.8)+
  xlab("Proximity to Creek")+
  ylab("Difference in ENDVI (bottom-top)")+
  labs(col="Site")+
  theme_bw()+
  theme(legend.position = "bottom")
  
SnailDiff%>%
  ggplot(aes(x=Avg.Stalks, y=NDVI.diff, col=factor(Site)))+
  geom_point(position=position_jitterdodge(), alpha=0.4, size=3)+
  geom_hline(yintercept=0)+
  xlab(expression(paste("Average Number of", italic("Spartina")," Stalks in 1", m^2, sep=" ")))+
  ylab("Difference in ENDVI (bottom-top)")+
  labs(col="Site")+
  scale_x_continuous(trans="log1p")+
  theme_bw()+
  theme(legend.position = "bottom")

#====Q2: Analyses====

#confirm normality
hist(SnailDiff$NDVI.diff) #fairly good. There's a slight skew.
qqPlot(SnailDiff$NDVI.diff)#looks pretty good.
shapiro.test(SnailDiff$NDVI.diff) #p<0.05 reject null hypothesis. Data are non-normal

#Log +1 transform and try again.
hist(log1p(SnailDiff$NDVI.diff)) #looks better.
qqPlot(log1p(SnailDiff$NDVI.diff)) #looks alright still.

m2.diff=lm(log1p(NDVI.diff)~Location*Site, data=SnailDiff)
summary(m2.diff)
Anova(m2.diff,type = "3")
#                 Sum Sq  Df F value   Pr(>F)    
# Location      0.02884   1  6.5123  0.01123 *  
# Site          0.15250   4  8.6080 1.41e-06 ***
# Location:Site 0.14186   4  8.0072 3.90e-06 ***


m2.diff2 = lm(log1p(NDVI.diff)~log1p(Avg.Stalks)*Site,
               data=SnailDiff)
summary(m2.diff2)
Anova(m2.diff2,type = "3")
#                           Sum Sq  Df F value   Pr(>F)    
#   log1p(Avg.Stalks)      0.02884   1  6.5123  0.01123 *  
#   Site                   0.13766   4  7.7704 5.83e-06 ***
#   log1p(Avg.Stalks):Site 0.13965   4  7.8825 4.82e-06 ***


#====Question 3: Does the algal biomass differ on snails of different sizes?====
#====Q3:Plot====
Snail%>%
  filter(!is.na(area))%>%
  ggplot(aes(x=Location, y=area, col=factor(Site)))+
  geom_point(alpha=0.4, position = position_jitterdodge(), size=3)+
  geom_boxplot(alpha=0)+
  scale_color_manual(values = Sitecol)+
  theme_bw()

Snailmean%>%
  ggplot(aes(x=area, y=mean.NDVI, col=Location))+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth(method="lm", se = FALSE)+
  xlab(bquote('Shell Area (' ~mm^2*')'))+
  ylab("Average ENDVI")+
  theme_bw()

Snail%>%
  filter(!is.na(area))%>%
  select(Snail, Location, Site, position, NDVI, area)%>%
  mutate(Site=as.factor(Site))%>%
  spread(key = position, value = NDVI)%>% #convert to wide format
  mutate(NDVI.diff=bottom-top)%>%
  ggplot(aes(x=area, y=NDVI.diff, col=Location))+
  geom_point(alpha=0.4, size=3.5)+
  geom_smooth(method="lm", se=FALSE)+
  xlab(bquote('Shell Area (' ~mm^2*')'))+
  ylab("Difference in ENDVI (bottom-top)")+
  theme_bw()+
  theme(legend.position="bottom")



#====Q3:Analyses====
#Are snails the same size near and far?
size=lm(area~Location*Site, data=Snail)
summary(size) #yes location is significantly different p<0.0001, site differs significantly, p=0.028, interaction very significant p<0.0001
Anova(size)
#               Sum Sq  Df F value    Pr(>F)    
# Location       53950   1 10.2971   0.00159 ** 
# Site            5917   1  1.1294   0.28940    
# Location:Site  88095   1 16.8141 6.348e-05 ***

m31= lmer(log1p(mean.NDVI)~Location*area +(1|Site),
         data=Snailmean)

m31= lm(log1p(mean.NDVI)~Location*area +Site,
          data=Snailmean)

Anova(m31, type="3")
#                 Chisq Df Pr(>Chisq)    
#   Location       6.3761  1  0.0115670 *  
#   area           0.7584  1  0.3838333    
# Location:area  2.6987  1  0.1004299  

m32= lmer(log1p(NDVI.diff)~Location*area +(1|Site),
   data=SnailDiff)

Anova(m32, type="3")
#                 Chisq Df Pr(>Chisq)    
#   Location       4.1112  1  0.0426009 *  
#   area          13.0386  1  0.0003051 ***
#   Location:area  9.7209  1  0.0018218 ** 

r.squaredGLMM(m31) #site substantial variation. 
r.squaredGLMM(m32) #site explains no variation. 
