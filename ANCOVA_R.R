ggplot(MENDB, aes(x = lRichness, y = BNPP, colour = Origin)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') theme(axis.ticks = element_line(size=8)) +
  ylab ("BNPP g/m2") + library(ggplot2)
library(dbplyr)
library(ggfortify)
library(dplyr)
library(car)

ggplot(MENDB, aes(x = lRichness, y = NPP, colour = Origin)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') + theme(axis.ticks = element_line(size=8)) + 
  ylab ("ANPP g/m2") + 
  theme_classic() + scale_y_continuous(trans='log10') 
MEND.mod <-lm(lNPP ~ Irrigation + lRichness * Origin, data=MENDB)
names(MEND.mod)
Anova(MEND.mod, type="III")
summary(MEND.mod)
autoplot(MEND.mod)

ggplot(MENDB, aes(x = lRichness, y = ANPP, colour = Origin)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') + theme(axis.ticks = element_line(size=8)) +
  ylab ("ANPP g/m2") + 
  theme_classic() + scale_y_continuous(trans='log10') 
MEND.mod <-lm(lANPP ~ Irrigation + lRichness * Origin, data=MENDB)
names(MEND.mod)
Anova(MEND.mod, type="III")
ummary(MEND.mod)
autoplot(MEND.mod)

ggplot(MENDB, aes(x = lRichness, y = BNPP, colour = Origin)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') + theme(axis.ticks = element_line(size=8)) +
  ylab ("BNPP g/m2") + 
  theme_classic() + scale_y_continuous(trans='log10')
  
MEND.mod <-lm(lBNPP ~ Irrigation + lRichness * Origin, data=MENDB)
names(MEND.mod)
Anova(MEND.mod, type="III")
summary(MEND.mod)
autoplot(MEND.mod)

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




