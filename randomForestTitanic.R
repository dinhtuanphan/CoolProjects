# Load the library

library(ggplot2)
library(randomForest)

# Load the data

titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE, header = TRUE)

# Take a look at the data
head(titanic)
summary(titanic)
names(titanic)
dim(titanic)

# Function to extract features
cleanData <- function(data){
  # replace missing values
  data <- replace(data, data == "" , NA)

  # impute missing data
  data$Age[is.na(data$Age)] <- -1
  data$Cabin[is.na(data$Cabin)] <- "U"
  data$Fare[is.na(data$Fare)] <- median(data$Fare, na.rm = TRUE)
  data$Embarked[is.na(data$Embarked)] <- "U"
  
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
model <- randomForest(Survived ~ . -Cabin, data = titanic)
model
