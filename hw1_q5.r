# (a)

auto = read.csv("Auto.csv", na.strings="?")
auto = na.omit(auto)
fix(auto)
summary(auto)

# (b)

sapply(auto[, -c(2, 8, 9)], range)

# (c)
sapply(auto[, -c(2, 8, 9)], mean)
sapply(auto[, -c(2, 8, 9)], sd)

#(d)
no_10_85 = auto[-c(10:85), -c(2, 8,9)]
sapply(no_10_85, range)
sapply(no_10_85, mean)
sapply(no_10_85, sd)

#(e)
pairs(auto)
plot(auto$displacement, auto$horsepower)
plot(auto$acceleration, auto$horsepower)

#(f)
pairs(auto)
cor(auto$displacement, auto$mpg)
cor(auto$weight, auto$mpg)
cor(auto$year, auto$mpg)
