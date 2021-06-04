# 1. load the library

library(ISLR)
library(ggplot2)

# 2. load data

data(Smarket)

# 3. perform basic EDA: pairs plot, cor, summary, etc..

summary(Smarket)
names(Smarket)
pairs(Smarket)
cor(Smarket[,-ncol(Smarket)])
hist(Smarket$Today)


# 4. clean, manipulate data (if applicable), subset data

split <- Smarket$Year < 2005
train <- Smarket[split, ]
test <- Smarket[!split, ]


# 5. build model

model <- glm(Direction ~ . - Today - Direction , data = train, family  = "binomial")
summary(model)

# 6. test model

pred <- predict(model, data.frame(test), type = "response")

summary(pred)
head(pred)

# 7. predict

pred <- ifelse(pred >= 0.5, "Up", "Down" )
table(pred, test$Direction)

mean(pred == test$Direction)


