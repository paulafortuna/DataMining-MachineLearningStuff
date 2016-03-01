library (tree)
library (ISLR)

########################
# classification trees
########################

attach(Carseats)

#create the class column high 
High = ifelse(Carseats$Sales <= 8," No"," Yes ")

#merge the column in the dataset 
Carseats = data.frame(Carseats, High)

#generate a tree model for the dataset -> include all variables (.) except sales (-) 
tree.carseats = tree(High ~ . - Sales, Carseats)

summary(tree.carseats)
#A small deviance indicates a tree that provides a good fit to the (training) data
#check slides for the residual mean deviance

#display the decision rules
tree.carseats

#display the tree
plot(tree.carseats)
text(tree.carseats, pretty = 0)

##################################

#to evaluate the performance: use a *training* and *test* set

set.seed(2)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]
tree.carseats = tree(High ~ . - Sales, Carseats, subset=train) #subset specification of the instances to use
tree.pred = predict(tree.carseats, Carseats.test, type = "class")
table(tree.pred, High.test)
success_rate = (86 + 57)/(86 + 57 + 27 + 30)

#pruning to improve results
set.seed(3)
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass) #fun -> function in the pruning
names(cv.carseats)
cv.carseats

#discover the number of nodes that minimizes the error - check at the minimum dev and correspondent size

#apply the limit of nodes
prune.carseats = prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred=predict(prune.carseats, Carseats.test ,type="class")
table(tree.pred ,High.test)
success_rate_prunning = (94+60)/(94+60+22+24)