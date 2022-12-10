####Build the different subsets for scoring####
dat = read.csv("combined.csv")

table(dat$Group)

#make the data subsets
datA = subset(dat,
              dat$Group == "JOL A")
datB = subset(dat,
              dat$Group == "JOL B")
datC = subset(dat,
              dat$Group == "JOL C")
datD = subset(dat,
              dat$Group == "JOL D")

#now make the answer keys
keyA = subset(datA,
              datA$Subject == 1)
keyA = keyA[ , c(4, 5, 10)]

keyB = subset(datB,
              datB$Subject == 2)
keyB = keyB[ , c(4, 5, 10)]

keyC = subset(datC,
              datC$Subject == 3)
keyC = keyC[ , c(4, 5, 10)]

keyD = subset(datD,
              datD$Subject == 4)
keyD = keyD[ , c(4, 5, 10)]

##write everything to file
#data
#write.csv(datA, file = "datA.csv", row.names = F)
#write.csv(datB, file = "datB.csv", row.names = F)
#write.csv(datC, file = "datC.csv", row.names = F)
#write.csv(datD, file = "datD.csv", row.names = F)

#answer keys
#write.csv(keyA, file = "keyA.csv", row.names = F)
#write.csv(keyB, file = "keyB.csv", row.names = F)
#write.csv(keyC, file = "keyC.csv", row.names = F)
#write.csv(keyD, file = "keyD.csv", row.names = F)


####score the data####
library(lrd)

#fix case
datA$Response = tolower(datA$Response)
datB$Response = tolower(datB$Response)
datC$Response = tolower(datC$Response)
datD$Response = tolower(datD$Response)

keyA$Target = tolower(keyA$Target)
keyB$Target = tolower(keyB$Target)
keyC$Target = tolower(keyC$Target)
keyD$Target = tolower(keyD$Target)

#now score!
#A
scoredA = prop_correct_cued(datA,
                  responses = "Response",
                  key = keyA$Target,
                  key.trial = keyA$Recall_Num,
                  id = "Subject",
                  id.trial = "Study_Num",
                  cutoff = 1)

View(scoredA$DF_Scored)

#B
scoredB = prop_correct_cued(datB,
                            responses = "Response",
                            key = keyB$Target,
                            key.trial = keyB$Recall_Num,
                            id = "Subject",
                            id.trial = "Study_Num",
                            cutoff = 1)

View(scoredB$DF_Scored)

table(datC$Subject)

#C
scoredC = prop_correct_cued(datC,
                            responses = "Response",
                            key = keyC$Target,
                            key.trial = keyC$Recall_Num,
                            id = "Subject",
                            id.trial = "Study_Num",
                            cutoff = 1)

View(scoredC$DF_Scored)

#D
scoredD = prop_correct_cued(datD,
                            responses = "Response",
                            key = keyD$Target,
                            key.trial = keyD$Recall_Num,
                            id = "Subject",
                            id.trial = "Study_Num",
                            cutoff = 1)

View(scoredD$DF_Scored)

#now combine!
scored = rbind(scoredA$DF_Scored, scoredB$DF_Scored,
               scoredC$DF_Scored, scoredD$DF_Scored)

#and drop extra columns
scored = scored[, -c(1, 5:6, 9:12)]

#and order by subject
scored = scored[order(scored$Sub.ID), ]

#and finall... write to file!
#write.csv(scored, file = "Ex 2 Scored.csv", row.names = F)