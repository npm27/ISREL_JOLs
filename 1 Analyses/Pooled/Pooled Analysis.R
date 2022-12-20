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

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

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
ex2.r2 = cast(ex2.r, Subject ~ Direction, mean)

###JOLS
##f
temp = t.test(ex1.jol2$F, ex2.jol2$F, var.equal = T, paired = F)
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##b
temp = t.test(ex1.jol2$B, ex2.jol2$B, var.equal = T, paired = F)
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##S
temp = t.test(ex1.jol2$S, ex2.jol2$S, var.equal = T, paired = F)
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##U
temp = t.test(ex1.jol2$U, ex2.jol2$U, var.equal = T, paired = F)
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##get the pbics
#f
pbic1 = ex1.jol2[ , c(1, 3)]
pbic1$ex = rep("1")

pbic2 = ex2.jol2[ , c(1, 3)]
pbic2$ex = rep("2")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Subject,
        between = ex,
        dv = F,
        detailed = T,
        type = 3)
#b
pbic1 = ex1.jol2[ , c(1, 2)]
pbic1$ex = rep("1")

pbic2 = ex2.jol2[ , c(1, 2)]
pbic2$ex = rep("2")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Subject,
        between = ex,
        dv = B,
        detailed = T,
        type = 3)

#S
pbic1 = ex1.jol2[ , c(1, 4)]
pbic1$ex = rep("1")

pbic2 = ex2.jol2[ , c(1, 4)]
pbic2$ex = rep("2")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Subject,
        between = ex,
        dv = S,
        detailed = T,
        type = 3)

#U
pbic1 = ex1.jol2[ , c(1, 5)]
pbic1$ex = rep("1")

pbic2 = ex2.jol2[ , c(1, 5)]
pbic2$ex = rep("2")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Subject,
        between = ex,
        dv = U,
        detailed = T,
        type = 3)

###Recall
##f
temp = t.test(ex1.r2$F, ex2.r2$F, var.equal = T, paired = F)
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##b
temp = t.test(ex1.r2$B, ex2.r2$B, var.equal = T, paired = F)
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##S
temp = t.test(ex1.r2$S, ex2.r2$S, var.equal = T, paired = F)
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##U
temp = t.test(ex1.r2$U, ex2.r2$U, var.equal = T, paired = F)
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##get values for d
#b
mean(ex1.r2$B); mean(ex2.r2$B)
sd(ex1.r2$B); sd(ex2.r2$B)

#u
mean(ex1.r2$U); mean(ex2.r2$U)
sd(ex1.r2$U); sd(ex2.r2$U)

##get pbics
#f
pbic1 = ex1.r2[ , c(1, 3)]
pbic1$ex = rep("1")

pbic2 = ex2.r2[ , c(1, 3)]
pbic2$ex = rep("2")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Subject,
        between = ex,
        dv = F,
        detailed = T,
        type = 3)

#s
pbic1 = ex1.r2[ , c(1, 4)]
pbic1$ex = rep("1")

pbic2 = ex2.r2[ , c(1, 4)]
pbic2$ex = rep("2")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Subject,
        between = ex,
        dv = S,
        detailed = T,
        type = 3)