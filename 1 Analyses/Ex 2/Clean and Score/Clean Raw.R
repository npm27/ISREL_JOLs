##This script formats raw data for scoring
####Set up####
dat = read.csv("Ex 2 Raw.csv")

#drop unused columns
dat = dat[ , c(1:2, 34, 41, 47:54, 62, 72, 82, 92, 102, 112, 126)]

#remove practice trials
dat = subset(dat,
             dat$Procedure.Trial. != "Practice1")

#now remove buffer items
dat = subset(dat,
             is.na(dat$Buffer1) == T)

#drop a few more unused columns
dat = dat[ , -c(4, 8:10)]

####Reshape the data####
##I need to reshape (need jols and recall side by side)
table(dat$Procedure.Trial.)

jol = subset(dat,
             dat$Procedure.Trial. != "recallproc1")

#remove the other buffer
jol = subset(jol,
             jol$Procedure.Trial. != "StudyProc2")

recall = subset(dat,
                dat$Procedure.Trial. == "recallproc1")

#now to get everything sorted
jol = jol[order(jol$ListNum), ]
jol = jol[order(jol$Subject), ]

recall = recall[order(recall$ListNum), ]
recall = recall[order(recall$Subject), ]

#now combine
combined = cbind(jol, recall)

#now drop more unused columns
combined = combined[ , -c(3, 6, 8, 15:18, 20:21, 23:29)]
combined = combined[ , -c(6, 7, 9)]

#now write to.csv and finish editing in Excel
#write.csv(combined, file = "combined.csv", row.names = F)
