# Load the library

library(ggplot2)
library(randomForest)
library(party)

# Load the data

titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE, header = TRUE)

# Take a look at the data
head(titanic)
summary(titanic)
names(titanic)
dim(titanic)

# Function to extract features
cleanData <- function(data){
  
  # helper function
  
  myMode <- function(x){
    uniqueX <- unique(x)
    mode <- uniqueX[which.max(tabulate(match(x, uniqueX)))]
  }
  
  # replace missing values
  
  data <- replace(data, data == "" , NA)

  # impute missing data
  data$Age[is.na(data$Age)] <- -1
  data$Cabin[is.na(data$Cabin)] <- "U"
  data$Fare[is.na(data$Fare)] <- mean(data$Fare, na.rm = TRUE)
  data$Embarked[is.na(data$Embarked)] <- myMode(data$Embarked[!is.na(data$Embarked)])
  
  # coerce character to levels
  data$Gender <- as.factor(data$Gender)
  data$Embarked <- as.factor(data$Embarked)
  data$Survived <- as.factor(data$Survived)
  data$Cabin <- as.factor(data$Cabin)
  
  return(data)
}

# Subset data
titanic <- cleanData(titanic)
titanic <- subset(titanic, select = -c(PassengerId,Name,Ticket ))

# Build model
rf <- randomForest(Survived ~ . -Cabin, data = titanic,
                      ntree = 1000, importance = TRUE)
rf
plot(rf, log = "y")
varImpPlot(rf)


