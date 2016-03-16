y = 3
lambda = 2
beta = seq(-10, 10, 0.1)
plot(beta, (y - beta)^2 + lambda * beta^2, pch = 20, xlab = "beta", ylab = "Ridge optimization")
beta.est = y / (1 + lambda)
points(beta.est, (y - beta.est)^2 + lambda * beta.est^2, col = "red", pch = 4, lwd = 5)


y = 3
lambda = 2
beta <- seq(-10, 10, 0.1)
plot(beta, (y - beta)^2 + lambda * abs(beta), pch = 20, xlab = "beta", ylab = "Lasso optimization")
beta.est = y - lambda / 2
points(beta.est, (y - beta.est)^2 + lambda * abs(beta.est), col = "red", pch = 4, lwd = 5)