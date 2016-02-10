# (a)

auto = read.csv("Auto.csv")
fix(auto)
summary(auto)

# (b)
sapply(auto[, -c(4, 9)], range)

# (c)
sapply(auto[, -c(4, 9)], mean)
sapply(auto[, -c(4, 9)], sd)

#(d)
no_10_85 = auto[-c(10:85), -c(4,9)]
sapply(no_10_85, range)
sapply(no_10_85, mean)
sapply(no_10_85, sd)

#(e)
pairs(auto)
plot(auto$cylinders, auto$mpg)
plot(as.factor(auto$cylinders), auto$mpg)
plot(as.factor(auto$cylinders), auto$acceleration)

plot(auto$displacement, auto$horsepower)
plot(auto$acceleration, auto$horsepower)

#(f)
pairs(auto)
plot(auto$mpg, auto$horsepower)
