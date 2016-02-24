library (MASS) #datasets and functions
library (ISLR)
library (car)

dataset = Boston

#1 LOOK TO THE DATA
# columns:
# medv (median house value) - dependent variable

#2 check some charts: is there any linear relation?
pairs(dataset)

#3 just check if a linear regression fits
lm.fit = lm(medv~lstat,dataset)
summary(lm.fit) 

#how to interpret RSE
rse = summary(lm.fit)$sigma
m = mean(dataset$medv)
percentage = rse/m

plot(dataset$lstat, dataset$medv)
abline (lm.fit)

#4 residuals plots
residuals = resid(lm.fit) #List of residuals
plot(dataset$lstat, residuals, ylab="Residuals", xlab="lstat") 
abline(0, 0)

#5 multiple regression
lm.fit2 = lm(formula = medv ~ lstat + age , dataset)
summary(lm.fit2) # -> one more variable only increases 1% of explained variance check r^2

#6 multiple regression with all variables (use .)
lm.fit3 =lm(medv ~ .,dataset)
summary(lm.fit3)

#r^2  
summary(lm.fit3)$r.sq

#vif -> variance inflation factors -> high values are removed 5-10
vif(lm.fit3)

#7 remove bad variables
lm.fit4 =lm(medv ~ . - rad - tax, dataset)
summary(lm.fit4)

lm.fit4 =lm(medv ~ . - age - indus - rad - tax, dataset)
summary(lm.fit4)

#8 Interaction Terms
lm(formula = medv ~ lstat * age , dataset)

#9 regression with polynomial terms
polyregression = lm(formula = medv ~ poly(lstat,4), dataset)

#9 how to use the model to predict 
teste = subset(dataset, rad < 2)
predict (lm.fit,teste)


