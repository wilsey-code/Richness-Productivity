#save the data sets to the C drive, and 
#then import data sets (MENDB, BioGen, Latitude)

MENDB <- read.csv("C:/SNPP/MENDB.csv")
View(MENDB)
BioGen <- read.csv("C:/SNPP/BioGen.csv")
View(BioGen)
Latitude <- read.csv("C:/SNPP/Latitude.csv")
View(Latitude)

#load packages
library(dbplyr)
library(ggfortify)
library(dplyr)
library(car)

#plot belowground productivity x richness, MEND, Fig. 3a

ggplot(MENDB, aes(x = lRichness, y = BNPP, colour = Origin)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') theme(axis.ticks = element_line(size=8)) +
  ylab ("BNPP g/m2") + library(ggplot2)

#plot total productivity (NPP), MEND, Fig. 3b
ggplot(MENDB, aes(x = lRichness, y = NPP, colour = Origin)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') + theme(axis.ticks = element_line(size=8)) + 
  ylab ("ANPP g/m2") + 
  theme_classic() + scale_y_continuous(trans='log10') 

#linear model NPP for MEND
MEND.mod <-lm(lNPP ~ Irrigation + lRichness * Origin, data=MENDB)
names(MEND.mod)
Anova(MEND.mod, type="III")
summary(MEND.mod)
autoplot(MEND.mod)

#plot of aboveground net primary productivity (Figure 2a), MEND and linear model (ANCOVA)
ggplot(MENDB, aes(x = lRichness, y = ANPP, colour = Origin)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') + theme(axis.ticks = element_line(size=8)) +
  ylab ("ANPP g/m2") + 
  theme_classic() + scale_y_continuous(trans='log10') 
MEND.mod <-lm(lANPP ~ Irrigation + lRichness * Origin, data=MENDB)
names(MEND.mod)
Anova(MEND.mod, type="III")
summary(MEND.mod)
autoplot(MEND.mod)

#plot of belowground productivity, MEND
ggplot(MENDB, aes(x = lRichness, y = BNPP, colour = Origin)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') + theme(axis.ticks = element_line(size=8)) +
  ylab ("BNPP g/m2") + 
  theme_classic() + scale_y_continuous(trans='log10')

#linear model (ANCOVA) for belowground productivity  
MEND.mod <-lm(lBNPP ~ Irrigation + lRichness * Origin, data=MENDB)
names(MEND.mod)
Anova(MEND.mod, type="III")
summary(MEND.mod)
autoplot(MEND.mod)

#plot and linear model (ANCOVA) for ANPP x richness for BioGen
#Figure 2c
ggplot(BioGen, aes(x = lrich, y = ANPP, colour = Origin)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') + theme(axis.ticks = element_line(size=8)) +
  ylab ("ANPP g/m2") + 
  theme_classic() + scale_y_continuous(trans='log10')

Biogen.mod <-lm(lNPP ~ lrich * Origin + Grazing, data=BioGen)
autoplot(Biogen.mod)
names(Biogen.mod)
Anova(Biogen.mod, type="III")
summary(Biogen.mod)
predict(Biogen.mod)

#Plot and linear model (ANCOVA) for Aboveground primary productivity
#for latitudinal gradient study, Figure 2a
ggplot(Latitude, aes(x = lS, y = mBioSDm2, colour = Native)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') + theme(axis.ticks = element_line(size=8)) +
  ylab ("ANPP g/m2") + 
  theme_classic() + scale_y_continuous(trans='log10')

Latitude.mod <-lm(lmBioSD ~ LatDecDegree + lS * Native, data=Latitude)
autoplot(Latitude.mod)
names(Latitude.mod)
Anova(Latitude.mod, type="III")
summary(Latitude.mod)
predict(Latitude.mod)




