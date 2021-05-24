# Load the library

library(ggplot2)
library(randomForest)
library(party)

# Load the data

titanic <-
  read.csv("titanic.csv",
           stringsAsFactors = FALSE,
           header = TRUE)

# Take a look at the data
head(titanic)
summary(titanic)
names(titanic)
dim(titanic)

# Function to extract features
cleanData <- function(data) {
  # helper function
  
  myMode <- function(x) {
    uniqueX <- unique(x)
    mode <- uniqueX[which.max(tabulate(match(x, uniqueX)))]
  }
  
  # replace missing values by NA
  
  data <- replace(data, data == "" , NA)
  
  # impute missing data
  # For the fare, using median (for variables have less variance)
  data$Fare[is.na(data$Fare)] <- median(data$Fare, na.rm = TRUE)
  data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)
  
  # for Embarked, we replace by using mode
  data$Cabin[is.na(data$Cabin)] <-
    myMode(data$Cabin[!is.na(data$Cabin)])
  data$Embarked[is.na(data$Embarked)] <-
    myMode(data$Embarked[!is.na(data$Embarked)])
  
  # coerce characters to factor levels
  data$Gender <- as.factor(data$Gender)
  data$Embarked <- as.factor(data$Embarked)
  data$Cabin <- as.factor(data$Cabin)
  data$Survived <- as.factor(data$Survived)
  
  return(data)
}

# Subset data
titanic <- cleanData(titanic)
titanic <- subset(titanic, select = -c(PassengerId, Name, Ticket))

# Build model
rf <- randomForest(
  Survived ~ . - Cabin,
  data = titanic,
  ntree = 1000,
  mtry = round(sqrt(length(names(titanic))),0),
  importance = TRUE
)
rf
plot(rf, log = "y")
varImpPlot(rf)
