####Set up####
##read in data
dat = read.csv("Mixed Effects data 2.csv")

##libraries
library(lme4)
library(nlme)

##Build the model
#3(encoding) x 4(direction) x 11(bin) // this model is about to get ugly...

##set up the data
dat$Bin = as.character(dat$Bin)

dat$Recall = dat$Recall / 100

##Final model
model1 = glmer(Recall ~ Group * Direction * Bin
               + (1|Subject), data = dat, family = "binomial") 
