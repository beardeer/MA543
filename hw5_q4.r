library(e1071)

set.seed(42)
x = rnorm(100)
y = 4 * x^2 + 2 + rnorm(100)
train = sample(100, 50)
y[train] = y[train] + 4
y[-train] = y[-train] - 4
# Plot using different colors
plot(x[train], y[train], pch="+", lwd=4, col="red", ylim=c(-4, 20), xlab="X", ylab="Y")
points(x[-train], y[-train], pch="o", lwd=4, col="blue")

z = rep(0, 100)
z[train] = 1
# Take 25 observations each from train and -train
final.train = c(sample(train, 25), sample(setdiff(1:100, train), 25))
data.train = data.frame(x=x[final.train], y=y[final.train], z=as.factor(z[final.train]))
data.test = data.frame(x=x[-final.train], y=y[-final.train], z=as.factor(z[-final.train]))

svm.linear = svm(z~., data=data.train, kernel="linear", cost=10)
plot(svm.linear, data.train)

table(z[final.train], predict(svm.linear, data.train))

svm.poly = svm(z~., data=data.train, kernel="polynomial", cost=10)
plot(svm.poly, data.train)

table(z[final.train], predict(svm.poly, data.train))

svm.radial = svm(z~., data=data.train, kernel="radial", gamma=1, cost=10)
plot(svm.radial, data.train)

table(z[final.train], predict(svm.radial, data.train))

plot(svm.linear, data.test)
table(z[-final.train], predict(svm.linear, data.test))

plot(svm.poly, data.test)
table(z[-final.train], predict(svm.poly, data.test))

plot(svm.radial, data.test)
table(z[-final.train], predict(svm.radial, data.test))