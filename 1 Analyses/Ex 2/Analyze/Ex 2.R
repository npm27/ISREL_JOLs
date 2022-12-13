####Set up####
##read in data
dat = read.csv("Ex 2 Scored.csv")

##libraries
library(reshape)
library(ez)
library(psychReport)
library(Hmisc)

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

####Gamma####
##get data in the right format
long.dat.r = subset(long.dat,
                    long.dat$measure == "Recall")
long.dat.j = subset(long.dat,
                    long.dat$measure == "JOL")

combined = cbind(long.dat.r, long.dat.j)

combined_gammas = combined[ , -c(5, 7:11)]

colnames(combined_gammas)[5] = "Recall"
colnames(combined_gammas)[6] = "JOL"
colnames(combined_gammas)[2] = "type"
colnames(combined_gammas)[1] = "Subject"

combined_gammas = combined_gammas[ , c(1:4, 6, 5)]

combined_gammas$Recall[combined_gammas$Recall == 100] = 1

##Subset by encoding task
gammas_IS = subset(combined_gammas,
                   combined_gammas$type == "IS")

gammas_RL = subset(combined_gammas,
                   combined_gammas$type == "RL")

gammas_READ = subset(combined_gammas,
                     combined_gammas$type == "READ")

##Okay, now subset everything by direction
##IS
gammas_IS_F = subset(gammas_IS,
                     gammas_IS$Direction == "F")
gammas_IS_B = subset(gammas_IS,
                     gammas_IS$Direction == "B")
gammas_IS_S = subset(gammas_IS,
                     gammas_IS$Direction == "S")
gammas_IS_U = subset(gammas_IS,
                     gammas_IS$Direction == "U")

##RL
gammas_RL_F = subset(gammas_RL,
                     gammas_RL$Direction == "F")
gammas_RL_B = subset(gammas_RL,
                     gammas_RL$Direction == "B")
gammas_RL_S = subset(gammas_RL,
                     gammas_RL$Direction == "S")
gammas_RL_U = subset(gammas_RL,
                     gammas_RL$Direction == "U")

##READ
gammas_READ_F = subset(gammas_READ,
                       gammas_READ$Direction == "F")
gammas_READ_B = subset(gammas_READ,
                       gammas_READ$Direction == "B")
gammas_READ_S = subset(gammas_READ,
                       gammas_READ$Direction == "S")
gammas_READ_U = subset(gammas_READ,
                       gammas_READ$Direction == "U")

###Now do a bunch of loops to get mean gammas (computes gamma for each participant then averages)
##Start with IS
#F
empty = data.frame()

for (i in unique(gammas_IS_F$Subject)){
  
  temp = subset(gammas_IS_F, gammas_IS_F$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

forward_IS = empty

#Backward IS
empty = data.frame()

for (i in unique(gammas_IS_B$Subject)){
  
  temp = subset(gammas_IS_B, gammas_IS_B$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

backward_IS = empty

#Symmetrical IS
empty = data.frame()

for (i in unique(gammas_IS_S$Subject)){
  
  temp = subset(gammas_IS_S, gammas_IS_B$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

symmetrical_IS = empty

#Unrelated IS
empty = data.frame()

for (i in unique(gammas_IS_U$Subject)){
  
  temp = subset(gammas_IS_U, gammas_IS_U$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Unrelated_IS = empty

##Now do relational
#F
empty = data.frame()

for (i in unique(gammas_RL_F$Subject)){
  
  temp = subset(gammas_RL_F, gammas_RL_F$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

forward_RL = empty

#Backward
empty = data.frame()

for (i in unique(gammas_RL_B$Subject)){
  
  temp = subset(gammas_RL_B, gammas_RL_B$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

backward_RL = empty

#Symmetrical
empty = data.frame()

for (i in unique(gammas_RL_S$Subject)){
  
  temp = subset(gammas_RL_S, gammas_RL_B$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

symmetrical_RL = empty

#Unrelated
empty = data.frame()

for (i in unique(gammas_RL_U$Subject)){
  
  temp = subset(gammas_RL_U, gammas_RL_U$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Unrelated_RL = empty

##Now do read
#F
empty = data.frame()

for (i in unique(gammas_READ_F$Subject)){
  
  temp = subset(gammas_READ_F, gammas_READ_F$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

forward_READ = empty

#Backward
empty = data.frame()

for (i in unique(gammas_READ_B$Subject)){
  
  temp = subset(gammas_READ_B, gammas_READ_B$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

backward_READ = empty

#Symmetrical
empty = data.frame()

for (i in unique(gammas_READ_S$Subject)){
  
  temp = subset(gammas_READ_S, gammas_READ_B$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

symmetrical_READ = empty

#Unrelated
empty = data.frame()

for (i in unique(gammas_READ_U$Subject)){
  
  temp = subset(gammas_READ_U, gammas_READ_U$Subject == i)
  
  g = rcorr.cens(temp$Recall, temp$JOL, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Unrelated_READ = empty

##Okay, get means!
#IS
mean(forward_IS$g, na.rm = T)
mean(backward_IS$g, na.rm = T)
mean(symmetrical_IS$g, na.rm = T)
mean(Unrelated_IS$g, na.rm = T)

#RL
mean(forward_RL$g, na.rm = T)
mean(backward_RL$g, na.rm = T)
mean(symmetrical_RL$g, na.rm = T)
mean(Unrelated_RL$g, na.rm = T)

#READ
mean(forward_READ$g, na.rm = T)
mean(backward_READ$g, na.rm = T)
mean(symmetrical_READ$g, na.rm = T)
mean(Unrelated_READ$g, na.rm = T)
