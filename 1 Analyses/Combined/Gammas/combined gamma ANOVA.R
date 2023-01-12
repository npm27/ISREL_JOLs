##Read in the data
dat = read.csv("gammas_ex1.csv")
dat2 = read.csv("ex 2 gamma.csv")

dat2$i = dat2$i + 1000 #make unique sub id's

combined = rbind(dat, dat2)

combined$Experiment = as.character(combined$Experiment) #make sure Ex is treated as a label

options(scipen = 999)

#libraries
library(ez)
library(psychReport)

#run the ANOVA
model1 = ezANOVA(combined,
                 dv = g,
                 wid = i,
                 between = .(group, Experiment),
                 within = direction,
                 type = 3,
                 detailed = T)


model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")
