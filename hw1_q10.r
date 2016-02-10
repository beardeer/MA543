# (a)
library(ISLR)
auto = ISLR::Auto
pairs(Auto)

# (b)

names(auto)
cor(auto[1:8])


# (c)

fit_c = lm(mpg ~ . - name, data = auto)
summary(fit_c)

#(d)

par(mfrow = c(2, 2))
plot(fit_c)


# (e)

fit_e <- lm(mpg ~ cylinders * displacement + horsepower * acceleration, data = auto[, 1:8])
summary(fit_e)


# (f)

par(mfrow = c(2, 2))
plot(log(auto$displacement), auto$mpg)
plot(sqrt(auto$displacement), auto$mpg)
plot((auto$displacement)^2, auto$mpg)

fit_f_1 = lm(mpg ~ log(auto$displacement), data = auto)
fit_f_2 = lm(mpg ~ auto$displacement, data = auto)
summary(fit_f_1)
summary(fit_f_2)