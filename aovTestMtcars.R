# load data
data(mtcars)

# check basic statistics and visualization
head(mtcars)
summary(mtcars)

boxplot(mtcars$disp ~ factor(mtcars$gear))

anovaTest1 <- aov(mtcars$disp ~ factor(mtcars$gear))
summary(anovaTest1)

anovaTest2 <- aov(mtcars$disp ~ mtcars$hp * mtcars$gear)
summary(anovaTest2)

cor(mtcars$am, mtcars$gear)

