data(Smarket)

# 3. perform basic EDA: pairs plot, cor, summary, etc..

summary(Smarket)
pairs(Smarket)
hist(Smarket$Today)
cor(Smarket[,-ncol(Smarket)])

# 4. clean, manipulate data (if applicable), subset data

split <- Smarket$Year < 2005
train <- Smarket[split, ]
test <- Smarket[!split, ]

# 5. build model

model <- glm(Direction ~ Year + Lag1, data = train, family = "binomial")
summary(model)


# 6. test model

pred <- predict(model, data.frame(test), type = "response")
pred

pred <- ifelse(pred >= 0.6, "Up", "Down")
pred

# 7. predict

table(pred, test$Direction)
accuracy <- mean(pred == test$Direction)
print(accuracy)
