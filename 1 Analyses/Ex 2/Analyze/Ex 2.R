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
dat = na.omit(dat)

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
##split by jol and recall
#jol
is.jol = subset(IS,
                IS$measure == "JOL")
rl.jol = subset(RL,
                RL$measure == "JOL")
read.jol = subset(READ,
                  READ$measure == "JOL")

#recall
is.r = subset(IS,
                IS$measure == "Recall")
rl.r = subset(RL,
                RL$measure == "Recall")
read.r = subset(READ,
                  READ$measure == "Recall")

##Now make the post-hoc subsets
#jol
post.IS.jol = cast(is.jol, Sub.ID ~ Direction, mean)
post.READ.jol = cast(read.jol, Sub.ID ~ Direction, mean)
post.RL.jol = cast(rl.jol, Sub.ID ~ Direction, mean)

#recall
post.IS.r = cast(is.r, Sub.ID ~ Direction, mean)
post.READ.r = cast(read.r, Sub.ID ~ Direction, mean)
post.RL.r = cast(rl.r, Sub.ID ~ Direction, mean)

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

####Get 95% CIs####
###JOL
##IS
x = apply(post.IS.jol, 2, sd) 
x / sqrt(length(unique(post.IS.jol$Sub.ID))) * 1.96

##RL
x = apply(post.RL.jol, 2, sd) 
x / sqrt(length(unique(post.RL.jol$Sub.ID))) * 1.96

##READ
x = apply(post.READ.jol, 2, sd) 
x / sqrt(length(unique(post.READ.jol$Sub.ID))) * 1.96

###Recall
##IS
x = apply(post.IS.r, 2, sd) 
x / sqrt(length(unique(post.IS.r$Sub.ID))) * 1.96

##RL
x = apply(post.RL.r, 2, sd) 
x / sqrt(length(unique(post.RL.r$Sub.ID))) * 1.96

##READ
x = apply(post.READ.r, 2, sd) 
x / sqrt(length(unique(post.READ.r$Sub.ID))) * 1.96

####get values for cohen's d####
###JOL
##IS
apply(post.IS.jol, 2, mean) 
apply(post.IS.jol, 2, sd) 

##RL
apply(post.RL.jol, 2, mean) 
apply(post.RL.jol, 2, sd) 

##READ
apply(post.READ.jol, 2, mean) 
apply(post.READ.jol, 2, sd)

###RECALL
##IS
apply(post.IS.r, 2, mean) 
apply(post.IS.r, 2, sd) 

##RL
apply(post.RL.r, 2, mean) 
apply(post.RL.r, 2, sd) 

##READ
apply(post.READ.r, 2, mean) 
apply(post.READ.r, 2, sd) 