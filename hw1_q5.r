# (a)

auto = read.csv("Auto.csv")
fix(auto)
summary(auto)

# (b)
summary(auto[, -c(4, 9)])

# (c)
sapply(auto[, -c(4, 9)], mean)
sapply(auto[, -c(4, 9)], sd)

#(d)

#(e)

#(f)