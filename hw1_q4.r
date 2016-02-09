# (a)

library(ISLR)
college <- read.csv("College.csv")

# (b)

rownames (college )=college [,1]
fix(college)
college = college[,-1]
fix(college)

# (c)

# i
summary(college)
# ii
pairs(college[,1:10])
# iii 
plot(college$Private, college$Outstate, xlab = "Private", ylab ="Out of State", main = "Outstate Tuition")
# iv
Elite =rep ("No",nrow(college ))
Elite [college$Top10perc >50]=" Yes"
Elite =as.factor (Elite)
college =data.frame(college ,Elite)
summary(Elite)
plot(college$Elite, college$Outstate, xlab = "Elite", ylab ="Out of State", main = "Outstate Tuition")
# v
par(mfrow=c(2,2))
hist(college$PhD)
hist(college$Accept)
hist(college$Enroll)
hist(college$Outstate)
# vi
