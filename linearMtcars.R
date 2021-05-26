data(mtcars)

pairs(mtcars)

cor(mtcars)
names(mtcars)
head(mtcars)
library(caret)

model <- train(mpg ~ wt, data = mtcars, method = "lm")
summary(model)

model <- train(mpg ~ ., data = mtcars, method = "ridge")

summary(model)


fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)
summary(fitControl)

modelCV <- train(mpg ~ ., data = mtcars, method = "lasso", trControl = fitControl)
summary(modelCV)

modelCV



