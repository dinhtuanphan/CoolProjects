# Load the library

library(randomForest, ROCR, party)

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

# 60% training data

set.seed(777)
indexes <-
  sample(1:nrow(titanic), size = round(0.6 * nrow(titanic)), 0)

testData <- titanic[indexes, ]
trainData <- titanic[-indexes, ]

# Build model on the train data
rf <- randomForest(
  Survived ~ . - Cabin,
  data = testData,
  ntree = 1000,
  mtry = 2,
  importance = TRUE
)

plot(rf, log = "y", main = "Variable Important")
varImpPlot(rf)
rf

# Predict on the test data
pre <- predict(rf, testData)
table(actual = testData$Survived , predict = pre)

# Calculate the accuracy of the model on test data
mean(testData$Survived == pre)

# prediction for ROC curve
preROC <- predict(rf, testData, type = "prob")

classes <- unique(testData$Survived)

for (i in 1:2){
  trueValues <- ifelse(testData$Survived == classes[i], 1, 0)
  pred <- prediction(preROC[,i], trueValues)
  
}

pred <- prediction(preROC[,2], )
