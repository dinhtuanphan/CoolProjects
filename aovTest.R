data <- ToothGrowth
summary(data)
head(data)
library(dplyr)


str(data)
data$dose <- factor(data$dose, levels = c(0.5, 1, 2), 
                       labels = c("D0.5", "D1", "D2"))

table(data$supp, data$dose)

aovTest <- aov(len ~ supp + dose, data = data)
summary(aovTest)

aovTest2 <- aov(len ~ supp * dose, data = data)
summary(aovTest2)

group_by(data, supp, dose) %>% summarise(count = n(),
                                         mean = mean(len, na.rm = TRUE),
                                         sd = sd(len, na.rm = TRUE))
