# (a)

# i - iii
library(ISLR)
auto = ISLR::Auto
auto$horsepower = as.numeric(auto$horsepower)
summary(auto)
pairs(auto)
fit = lm(mpg ~ horsepower, data = auto)
summary(fit)

# iv
predict(fit, data.frame(horsepower = 98), interval = "confidence")
predict(fit, data.frame(horsepower = 98), interval = "prediction")


# (b)
plot(Auto$horsepower, Auto$mpg, main = "Scatterplot of mpg vs. horsepower", xlab = "horsepower", ylab = "mpg", col = "blue")
abline(fit, col = "red")

#(c)

par(mfrow = c(2, 2))
plot(fit)