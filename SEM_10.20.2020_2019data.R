library(piecewiseSEM);library(lme4);library(nlme)

data = read.table("clipboard", header=T)
names(data)

# "Bee_Rich"       "log_Compaction" "Forb_Rich_2018" "Bare_Ground"   
# "Percent_Sand"   "Watershed"      "Transect"       "Bee_Abundance" 
# "Fire"           "Grazing" 

##Did scatterplots of compaction and bare ground in excel to see transformations
###Use Compaction and log_total_BG

modlist = psem(
  lm(log_Compaction~Grazing+Fire,data = data),
  lm(Percent_Sand~Grazing+Fire,data = data),
  lm(Forb_Rich_2018~Grazing+Fire,data = data),
  lm(Bare_Ground~Grazing+Fire,data = data),
  lm(Bee_Abundance~Bare_Ground+Grazing+Fire+Forb_Rich_2018+Percent_Sand+log_Compaction,data = data),
  lm(Bee_Rich~Bare_Ground+Grazing+Fire+Forb_Rich_2018+Percent_Sand+log_Compaction,data = data))

summary(modlist)

###Simple Boxplots for Konza

hist(data$Bee_Abundance)

bartlett.test(Bee_Abundance~Watershed,data)

test=aov(Bee_Abundance~Watershed,data = data);summary(test)
tuk=TukeyHSD(test,"Watershed");tuk
summary(tuk)

#Boxplot

boxplot(Bee_Abundance~Fire,data = data,col=c('cornflowerblue'))