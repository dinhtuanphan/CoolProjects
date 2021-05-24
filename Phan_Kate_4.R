#######################################################
############# BU.510.650 - Data Analytics, Fall 2020
#######################################################

## Thuc Tram (Kate) Phan - Assignment 4 ##

# read the data in AutoLoss.csv to a data frame called AutoLoss
AutoLoss<-read.csv("AutoLoss.csv")

# replaces ?s with NA while reading the data from the .csv file
AutoLoss <- read.csv("AutoLoss.csv", na.strings = "?")

# remove all the observations with any NA.
AutoLoss <- na.omit(AutoLoss)

# check how many missing values there are in the dataframe AutoLoss
sum(is.na(AutoLoss))

# Question 1 #

# load library leaps
library(leaps)

# run regsubsets using Losses as the response, with data from AutoLoss
# store the results in a variable called regfit.full
regfit.full=regsubsets(Losses~., data = AutoLoss,nvmax = 15)
summary(regfit.full)


# run regsubsets using Losses as the response, with data from AutoLoss
# store the results in a variable called regfit.full
regfit.full=regsubsets(Losses~., data = AutoLoss,nvmax = 8)
summary(regfit.full)


# run forward stepwise selection, allowing subsets with up to 15 predictors
regfit.fwd=regsubsets(Losses~.,data=AutoLoss,nvmax=15, method="forward")
summary(regfit.fwd)
reg.summary = summary(regfit.fwd)

# display the number of predictors for which Cp reaches its minimum
which.min(reg.summary$cp)

# the coefficient estimates for the best model using forward selection method
coef(regfit.fwd,which.min(reg.summary$cp))


