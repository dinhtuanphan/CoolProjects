library(dplyr)
set.seed(1234)
sample_n(iris, 10)

resManova <- manova(cbind(iris$Sepal.Length, iris$Petal.Length) ~ Species, data = iris)
summary(resManova)

summary.aov(resManova)
