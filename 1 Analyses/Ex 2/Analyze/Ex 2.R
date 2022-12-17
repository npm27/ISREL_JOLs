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
model1


#Get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

#now get partial eta
aovEffectSize(model1, effectSize = "pes")

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
##main effect of encoding group
encoding_group = cast(long.dat, Sub.ID ~ Encoding, mean)

##IS vs Read
temp = t.test(encoding_group$IS, encoding_group$READ, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##GET VALUES FOR COMPUTING COHEN'S D
apply(encoding_group, 2, mean, na.rm = T)
apply(encoding_group, 2, sd, na.rm = T)

##main effect of pair direction
pair_type = cast(long.dat, Sub.ID ~ Direction, mean)

#F vs. S
temp = t.test(pair_type$F, pair_type$S, paired = F, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##GET VALUES FOR COMPUTING COHEN'S D
apply(pair_type, 2, mean, na.rm = T)
apply(pair_type, 2, sd, na.rm = T)

###Direction x Measure interaction
jol_dat = subset(long.dat,
                 long.dat$measure == "JOL")
recall_dat = subset(long.dat,
                    long.dat$measure == "Recall")

jol2 = cast(jol_dat, Sub.ID ~ Direction, mean)
recall2 = cast(recall_dat, Sub.ID ~ Direction, mean)

##F
temp = t.test(jol2$F, recall2$F, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##B
temp = t.test(jol2$B, recall2$B, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##S
temp = t.test(jol2$S, recall2$S, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##U
temp = t.test(jol2$U, recall2$U, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##get pbic for symmetrical
pbic1 = jol2[ , c(1, 4)]
pbic2 = recall2[ , c(1 ,4)]

pbic1$task = rep('jol')
pbic2$task = rep('recall')

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        wid = Sub.ID,
        dv = S,
        within = task,
        detailed = T,
        type = 3)

##Now get values for computing d
apply(jol2, 2, mean, na.rm = T)
apply(recall2, 2, mean, na.rm = T)

apply(jol2, 2, sd, na.rm = T)
apply(recall2, 2, sd, na.rm = T)

###Just do the three-way for now
##Backward
#Read
temp = t.test(post.READ.jol$B, post.READ.r$B, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#IS
temp = t.test(post.IS.jol$B, post.IS.r$B, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#RL
temp = t.test(post.RL.jol$B, post.RL.r$B, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 

##read d
mean(post.READ.jol$B); mean(post.READ.r$B)
sd(post.READ.jol$B); sd(post.READ.r$B)

##IS pbic
pbic1 = post.IS.jol[ , c(1,2)]
pbic2 = post.IS.r[ , c(1,2)]

pbic1$task = rep("jol")
pbic2$task = rep("recall")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = B,
        wid = Sub.ID,
        within = task,
        type = 3,
        detailed = T)

##RL pbic
pbic1 = post.RL.jol[ , c(1,2)]
pbic2 = post.RL.r[ , c(1,2)]

pbic1$task = rep("jol")
pbic2$task = rep("recall")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = B,
        wid = Sub.ID,
        within = task,
        type = 3,
        detailed = T)

##Forward
#Read
temp = t.test(post.READ.jol$F, post.READ.r$F, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#IS
temp = t.test(post.IS.jol$F, post.IS.r$F, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #UNDER CONFIDENCE

#RL
temp = t.test(post.RL.jol$F, post.RL.r$F, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 

#pbic read
pbic1 = post.READ.jol[ , c(1,3)]
pbic2 = post.READ.r[ , c(1,3)]

pbic1$task = rep("jol")
pbic2$task = rep("recall")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = F,
        wid = Sub.ID,
        within = task,
        type = 3,
        detailed = T)

#pbic rl
pbic1 = post.RL.jol[ , c(1,3)]
pbic2 = post.RL.r[ , c(1,3)]

pbic1$task = rep("jol")
pbic2$task = rep("recall")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = F,
        wid = Sub.ID,
        within = task,
        type = 3,
        detailed = T)

#d for is
mean(post.IS.jol$F); mean(post.IS.r$F)
sd(post.IS.jol$F); sd(post.IS.r$F)

##Symmetrical
#Read
temp = t.test(post.READ.jol$S, post.READ.r$S, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#IS
temp = t.test(post.IS.jol$S, post.IS.r$S, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #UNDER CONFIDENCE

#RL
temp = t.test(post.RL.jol$S, post.RL.r$S, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 

#get d's
mean(post.READ.jol$S); mean(post.READ.r$S)
sd(post.READ.jol$S); sd(post.READ.r$S)

mean(post.IS.jol$S); mean(post.IS.r$S)
sd(post.IS.jol$S); sd(post.IS.r$S)

#get pbic
pbic1 = post.RL.jol[ , c(1,4)]
pbic2 = post.RL.r[ , c(1,4)]

pbic1$task = rep("jol")
pbic2$task = rep("recall")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = S,
        wid = Sub.ID,
        within = task,
        type = 3,
        detailed = T)


##Unrelated
#Read
temp = t.test(post.READ.jol$U, post.READ.r$U, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#IS
temp = t.test(post.IS.jol$U, post.IS.r$U, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

#RL
temp = t.test(post.RL.jol$U, post.RL.r$U, paired = T, p.adjust.methods = "bonferroni", var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 

#get ds
mean(post.READ.jol$U); mean(post.READ.r$U)
sd(post.READ.jol$U); sd(post.READ.r$U)

mean(post.IS.jol$U); mean(post.IS.r$U)
sd(post.IS.jol$U); sd(post.IS.r$U)

#PBIC
pbic1 = post.RL.jol[ , c(1,5)]
pbic2 = post.RL.r[ , c(1,5)]

pbic1$task = rep("jol")
pbic2$task = rep("recall")

pbic3 = rbind(pbic1, pbic2)

ezANOVA(pbic3,
        dv = U,
        wid = Sub.ID,
        within = task,
        type = 3,
        detailed = T)

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
