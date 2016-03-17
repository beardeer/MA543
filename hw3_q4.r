# a
set.seed(1)
library(ISLR)
data(College)

train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
College.train = College[train, ]
College.test = College[test, ]

summary(College.train)
summary(College.test)

# b
lm.fit = lm(Apps~., data=College.train)
lm.pred = predict(lm.fit, College.test)
lm.mse = mean((College.test[, "Apps"] - lm.pred)^2)

# c
library(glmnet)

train.mat = model.matrix(Apps~., data=College.train)
test.mat = model.matrix(Apps~., data=College.test)
grid = 10 ^ seq(4, -2, length=100)
mod.ridge = cv.glmnet(train.mat, College.train[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda.best = mod.ridge$lambda.min
lambda.best

ridge.pred = predict(mod.ridge, newx=test.mat, s=lambda.best)
ridge.mse = mean((College.test[, "Apps"] - ridge.pred)^2)

# d

mod.lasso = cv.glmnet(train.mat, College.train[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best = mod.lasso$lambda.min
lambda.best

lasso.pred = predict(mod.lasso, newx=test.mat, s=lambda.best)
lasso.mse = mean((College.test[, "Apps"] - lasso.pred)^2)

mod.lasso = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(mod.lasso, s=lambda.best, type="coefficients")

# e

library(pls)

pcr.fit = pcr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcr.pred = predict(pcr.fit, College.test, ncomp=10)
pcr.mse = mean((College.test[, "Apps"] - data.frame(pcr.pred))^2)


# f

pls.fit = plsr(Apps~., data=College.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")

pls.pred = predict(pls.fit, College.test, ncomp=10)
pls.mse = mean((College.test[, "Apps"] - data.frame(pls.pred))^2)

# g

test.avg = mean(College.test[, "Apps"])
lm.test.r2 = 1 - mean((College.test[, "Apps"] - lm.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
ridge.test.r2 = 1 - mean((College.test[, "Apps"] - ridge.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
lasso.test.r2 = 1 - mean((College.test[, "Apps"] - lasso.pred)^2) /mean((College.test[, "Apps"] - test.avg)^2)
pcr.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pcr.pred))^2) /mean((College.test[, "Apps"] - test.avg)^2)
pls.test.r2 = 1 - mean((College.test[, "Apps"] - data.frame(pls.pred))^2) /mean((College.test[, "Apps"] - test.avg)^2)
barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), names.arg=c("LR", "Ridge", "Lasso", "PCR", "PLS"), main="Test R2")

lm.test.r2
ridge.test.r2
lasso.test.r2
pcr.test.r2
pls.test.r2

barplot(c(lm.mse, ridge.mse, lasso.mse, pcr.mse, pls.mse), names.arg=c("LR", "Ridge", "Lasso", "PCR", "PLS"), main="Test MSE")

