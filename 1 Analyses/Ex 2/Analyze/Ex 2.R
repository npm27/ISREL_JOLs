####Set up####
##read in data
dat = read.csv("Ex 2 Scored.csv")

##libraries
library(reshape)
library(ez)
library(psychReport)

##turn of scientific notation
options(scipen = 999)

####Cleaning####
#fix jols
dat$JOL = as.numeric(dat$JOL) #get rid of any weird characters
dat$JOL[dat$JOL > 100] = NA #remove out of range values

#fix recall
dat$Scored = dat$Scored * 100 #get jols and recall on same scale

#Okay, what are we working with now?
summary(dat)

#remove the NAs
dat = na.omit(dat)ga

#get n
length(unique(dat$Sub.ID)) #102

##check for outliers/general weirdness
#any participants who got nothing correct? (shouldn't have to worry about cheating since in person!)
check_dat = cast(dat, Sub.ID ~ Direction, mean)

colnames(dat)[6] = "Recall"

##get data in long format
long.dat = melt(dat, measure.vars = c("JOL", "Recall"))

colnames(long.dat)[5] = "measure"
colnames(long.dat)[6] = "Score"

##descriptives
tapply(long.dat$Score, list(long.dat$measure, long.dat$Direction), mean) ##overall

#make the subsets
READ = subset(long.dat,
              long.dat$Encoding == "READ")
IS = subset(long.dat,
              long.dat$Encoding == "IS")
RL = subset(long.dat,
              long.dat$Encoding == "RL")

##READ
tapply(READ$Score, list(READ$measure, READ$Direction), mean)

##IS
tapply(IS$Score, list(IS$measure, IS$Direction), mean)

##RL
tapply(RL$Score, list(RL$measure, RL$Direction), mean)

####ANOVA time####
model1 = ezANOVA(long.dat,
                 dv = Score,
                 between = Encoding,
                 within = .(measure, Direction),
                 wid = Sub.ID,
                 type = 3,
                 detailed = T)
model1

#main effect of "measure" (diff between JOLs and recall is NS)
tapply(long.dat$Score, long.dat$measure, mean)

##significant effects
tapply(long.dat$Score, long.dat$Direction, mean) #direction
tapply(long.dat$Score, long.dat$Encoding, mean) #encoding group

##2-way interactions
tapply(long.dat$Score, list(long.dat$measure, long.dat$Encoding), mean)
tapply(long.dat$Score, list(long.dat$measure, long.dat$Direction), mean)
tapply(long.dat$Score, list(long.dat$Encoding, long.dat$Direction), mean)

##3-way interaction
tapply(READ$Score, list(READ$measure, READ$Direction), mean) #read
tapply(IS$Score, list(IS$measure, IS$Direction), mean) #item-specific
tapply(RL$Score, list(RL$measure, RL$Direction), mean) #relational

####post-hocs####
post.IS = cast(IS, Sub.ID ~ Direction, mean)
post.READ = cast(READ, Sub.ID ~ Direction, mean)
post.RL = cast(RL, Sub.ID ~ Direction, mean)

####t-tests####
###Just do the three-way for now
##Control
#F

#B

#S

#U

##Item-specific
#F

#B

#S

#U

##Relational
#F

#B

#S

#U

####get values for cohen's d####