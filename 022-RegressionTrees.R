library(MASS)

# use the model of a regression tree in the data
set.seed(1)
train = sample(1: nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv ~ ., Boston, subset = train)
summary(tree.boston)

# see the tree
plot(tree.boston)
text(tree.boston, pretty = 0)

#use pruning to improve model predictive capacity
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

#calculate Mean Squared Error - em media a quanta distancia e que cada estimativa esta do valor real?
yhat = predict(tree.boston, newdata = Boston[-train,]) 
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
MSE = mean((yhat - boston.test)^2)
square = sqrt(MSE) # test predictions are within around 5 of the true median value
