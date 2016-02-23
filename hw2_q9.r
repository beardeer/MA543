library(ISLR)
attach(Default)
summary(Default)

glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)


boot.fn = function(data, index) {
  return(coef(glm(default ~ income + balance, data = data, family = binomial, subset = index)))
}

library(boot)
boot(Default, boot.fn, 100)
  