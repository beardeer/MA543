set.seed(42)
summary(Wage$maritl)

summary(Wage$jobclass)

par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)