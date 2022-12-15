####Interaction between experiments?####
###General set up
##read in data
dat1 = read.csv("Ex 1 Cleaned.csv")
dat2 = read.csv("Ex 2 Cleaned.csv")

##load libraries
library(reshape)
library(ez)
library(psychReport)

##turn off scientific notation
options(scipen = 9)

###Start cleaning the data
##remove out of range values
dat1$JOL[dat1$JOL > 100] = NA
dat2$JOL[dat2$JOL > 100] = NA

#get recall on correct scale
dat2$Scored = dat2$Scored * 100

#get everything lined up correct for merge
colnames(dat2)[6] = "Recall"

dat1 = dat1[ , -c(3, 6)]

colnames(dat1)[6] = "Encoding"

dat1 = dat1[ , -1]

colnames(dat2)[1] = "Subject"

dat2 = dat2[ , -3]

dat1 = dat1[ , c(1, 5, 2, 3, 4, 6)]

dat1$Subject = dat1$Subject + 1000
dat2$Subject = dat2$Subject + 2000

##remove NAs
dat1 = na.omit(dat1)
dat2 = na.omit(dat2)

#now combine
combined = rbind(dat1, dat2)

#now get in long format for ANOVA
combined.long = melt(combined, 
                     measure.vars = c("JOL", "Recall"))

colnames(combined.long)[5:6] = c("Measure", "Score")

###Analysis time
model1 = ezANOVA(combined.long,
                 wid = Subject,
                 dv = Score,
                 between = .(Experiment, Encoding),
                 within = .(Direction, Measure),
                 type = 3,
                 detailed = T)
model1

####T-test###
##break down the three-way (ex x direction x measure)
tapply(combined.long$Score, list(combined.long$Measure, combined.long$Direction), mean)

ex1 = subset(combined.long,
             combined.long$Experiment == 1)
ex2 = subset(combined.long,
             combined.long$Experiment == 2)

tapply(ex1$Score, list(ex1$Measure,ex1$Direction), mean)
tapply(ex2$Score, list(ex2$Measure,ex2$Direction), mean)

###get the data in the right form for t-tests
ex1.jol = subset(ex1,
                 ex1$Measure == "JOL")
ex1.r = subset(ex1,
               ex1$Measure == "Recall")

ex2.jol = subset(ex2,
                 ex2$Measure == "JOL")
ex2.r = subset(ex2,
               ex2$Measure == "Recall")

ex1.jol2 = cast(ex1.jol, Subject ~ Direction, mean)
ex2.jol2 = cast(ex2.jol, Subject ~ Direction, mean)

ex1.r2 = cast(ex1.r, Subject ~ Direction, mean)
ex2.rl2 = cast(ex2.r, Subject ~ Direction, mean)

###JOLS
##f
t.test(ex1.jol2$F, ex2.jol2$F, var.equal = T, paired = F)
