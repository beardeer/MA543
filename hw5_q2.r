library(ISLR)
library(tree)
attach(OJ)

# a
set.seed(42)

train = sample(dim(OJ)[1], 800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

summary(OJ.train)

# b
oj.tree = tree(Purchase ~ ., data = OJ.train)
summary(oj.tree)

# c 
oj.tree

# d

plot(oj.tree)
text(oj.tree, pretty = 0)

# e

oj.pred = predict(oj.tree, OJ.test, type = "class")
table(OJ.test$Purchase, oj.pred)

# f

cv.oj = cv.tree(oj.tree, FUN = prune.misclass)
cv.oj

# g

plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", ylab = "misclass")

# i

oj.pruned = prune.misclass(oj.tree, best = 5)
plot(oj.pruned)
text(oj.pruned, pretty = 0)

# j
summary(oj.tree)
summary(oj.pruned)

# k

pred.unpruned = predict(oj.tree, OJ.test, type = "class")
misclass.unpruned = sum(OJ.test$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)

pred.pruned = predict(oj.pruned, OJ.test, type = "class")
misclass.pruned = sum(OJ.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)