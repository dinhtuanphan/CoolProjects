#######################################################
############# BU.510.650 - Data Analytics, Fall 2020
#######################################################

## Thuc Tram (Kate) Phan - Assignment 5 ##

# replaces ?s with NA while reading the data from the .csv file
AutoLoss <- read.csv("AutoLoss.csv", na.strings = "?")

# remove all the observations with any NA.
AutoLoss <- na.omit(AutoLoss)

# QUESTION 1 #
library(glmnet)
# the data frame x will hold the data for predictors
x=model.matrix(Losses~.,data=AutoLoss)[,-1] 

# the vector y will hold the data for the response, Losses in this case
y=AutoLoss$Losses

# Use 5-fold cross-validation on the whole data, to determine the best lambda value
set.seed(1)
cv.out=cv.glmnet(x,y,nfolds=5,alpha=1)

# plot the MSE observed in the cross-validation as a function of log(lambda)
plot(cv.out)

# determine which lambda minimized the MSE, call it bestlam, and display it
bestlam=cv.out$lambda.min
bestlam

# display coefficients
lasso.final=glmnet(x, y, alpha=1, lambda=bestlam)
coef(lasso.final)


#######################################################

# QUESTION 2 #
library(class)
Titanic <- read.csv("TitanicforKNN.csv")

set.seed(1)

# split data into two subsets
train=sample(1:nrow(Titanic), nrow(Titanic)/2) 

# next, we create the part of x and y that will be our training data
# we will call these x.train and y.train
Titanic.train=Titanic[train,2:8]
Titanic.test=Titanic[-train,2:8]

Survived.train=Titanic$Survived[train]
Survived.test=Titanic$Survived[-train]

# run knn with K=1 and store prediction results in a variable called knn.pred
knn.pred = knn(Titanic.train,Titanic.test,Survived.train,k=1)

# display a table that shows prediction results
table(knn.pred,Survived.test)
round(prop.table(table(knn.pred,Survived.test)),2)

# calculate prediction accuracy
mean(knn.pred==Survived.test)

# run knn with K=3 and store prediction results in a variable called knn.pred
knn.pred = knn(Titanic.train,Titanic.test,Survived.train,k=3)

# display a table that shows prediction results
table(knn.pred,Survived.test)
round(prop.table(table(knn.pred,Survived.test)),2)

# calculate prediction accuracy
mean(knn.pred==Survived.test)

# run knn with K=5 and store prediction results in a variable called knn.pred
knn.pred = knn(Titanic.train,Titanic.test,Survived.train,k=5)

# display a table that shows prediction results
table(knn.pred,Survived.test)
round(prop.table(table(knn.pred,Survived.test)),2)

# calculate prediction accuracy
mean(knn.pred==Survived.test)


#######################################################

# QUESTION 3 #
# replaces ?s with NA while reading the data from the .csv file
AutoLoss <- read.csv("AutoLoss-DT.csv", na.strings = "?")

# remove all the observations with any NA.
AutoLoss <- na.omit(AutoLoss)

library(tree)
set.seed(25)

str(AutoLoss)

# create a decision tree that predicts whether the loss for a vehicle will be high or low
tree.autoloss = tree(AutoLoss$HighLoss ~ . , AutoLoss)
summary(tree.autoloss)

# plot the tree
plot(tree.autoloss)

# see what the branches are 
text(tree.autoloss,pretty=0)

# use cross-validation and pruning to obtain smaller trees
cv.autoloss = cv.tree(tree.autoloss,FUN=prune.misclass)

# check size and dev
names(cv.autoloss)

# display the information in cv.autoloss
# observe that dev is minimum when size is 8
cv.autoloss

# use a function called prune.misclass() to obtain the best tree
prune.autoloss = prune.misclass(tree.autoloss,best=8)

# plot the best tree
plot(prune.autoloss)

# display branch names on the tree
text(prune.autoloss, pretty = 0)

