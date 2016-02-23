library(ISLR)
attach(Default)
summary(Default)

glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)


train = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial, subset = train)
summary(glm.fit)
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)


train = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
              subset = train)
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)