data(trees)
names(trees)
head(trees)
dim(trees)
summary(trees)
pairs(trees)
library(leaps)

anyNA(trees)

cor(trees)

lm <- lm(formula = Volume ~ Girth, data = trees)
summary(lm)

lm <- regsubsets(Volume ~ . , data = trees)
summary(lm)

lm
library(ggplot2)

ggplot(data = trees, aes(lm$residuals)) + geom_histogram()

lm$residuals

lm$coefficients

plot(lm$residuals)

ggplot(data = trees, aes(x = Girth, y = Volume)) +
  geom_point() + geom_smooth(method = "lm") 


predict(lm, data.frame(Girth = 20))


fit_2 <- lm(Volume ~ . ,data = trees)

ggplot(data = trees, aes(fit_2$residuals)) + geom_histogram()

ggplot(data = trees, aes(x = Girth,  y = Volume)) + geom_smooth(method = "lm")
fit_2
summary(fit_2)

predict(fit_2, data.frame(Girth = 18.2, Height = 72))


anova(lm,fit_2)


fit3 <- lm(Volume ~ Girth + Height + Girth * Height, data = trees)
summary(fit3)


predict(fit3, data.frame(Girth = 18.2, Height = 72))
