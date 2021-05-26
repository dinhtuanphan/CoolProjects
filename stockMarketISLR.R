library(ISLR)
library(ggplot2)

# read data file
# Smarket <- read.csv("filename", header = TRUE)

head(Smarket)
tail(Smarket)
summary(Smarket)
sum(is.na(Smarket))

# Check the correlation
cor(Smarket[, -9])

# Produce basic plots
plot(Smarket)
plot(Smarket$Today ~ Smarket$Lag1)
hist(Smarket$Today, 10)


# Subset data
train <- Smarket$Year( < 2005)

trainData <- Smarket[train, ]
directionTrain <- Smarket$Direction[train]
testData <- Smarket[!train, ]
directionTest <- Smarket$Direction[!train]

# Build prediction model
glmFit <-
  glm(
    fomula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
    family = "binomial",
    data = trainData
  )

summary(glmFit)

# Predict the proability of next day using the same training data and store the results in a variable called glmPre
glmPre <- predict(glmFit, data = trainData, type = "response")
head(glmPre, 5)

# Convert the probability prediction to "up" or "down" using a preset threshold 0.5
glmPre <- ifelse(glmPre > 0.5, "Up", "Down")

table(glmPre, Smarket$Direction)

mean(glmPre == Smarket$Direction)
