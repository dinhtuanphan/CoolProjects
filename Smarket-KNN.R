# TASK 1 #
# load library ISLR and see what the dataframe Smarket looks like
library(ISLR)
head(Smarket)

# create a vector train, wth 1250 values, TRUE whenever Year < 2005, FALSE otherwise
train=Smarket$Year<2005

# Pick all the rows for which the year is 2001 - 2004, and columns correspondng to Lag1,..., Lag5, Volume (columns 2 through 7)
# this will be our training data
# store this data in a variable called Smarket.train
Smarket.train = Smarket[train,2:7]

# Pick all the rows for which the year is 2005, and columns correspondng to Lag1,..., Lag5, Volume (columns 2 through 7)
# this will be our test data
# store this data in a variable called Smarket.test
Smarket.test=Smarket[!train,2:7]

# store the vector of directions for the year 2001 - 2004 in a variable called Direction.train
Direction.train=Smarket$Direction[train]

# store the vector of directions for the year 2005 in a variable called Direction.test
Direction.test=Smarket$Direction[!train]

# TASK 2 #
# load library class, you may have to install it first
library(class)

# run knn with K=1 and store prediction results in a variable called knn.pred
knn.pred = knn(Smarket.train,Smarket.test,Direction.train,k=1)

# TASK 3 #
# display a table that shows predicted directions for 2005 versus actual directions in 2005
table(knn.pred,Direction.test)

# calculate prediction accuracy
mean(knn.pred==Direction.test)

# TASK 4 #
# run knn with K=3 and store prediction results in a variable called knn.pred
knn.pred = knn(Smarket.train,Smarket.test,Direction.train,k=3)

# display a table that shows precidcted directions for 2005 versus actual directions in 2005
table(knn.pred,Direction.test)

# calculate prediction accuracy
mean(knn.pred==Direction.test)