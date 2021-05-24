# Statistical Analysis Happiness Score 2019-R


##########################################
# Multiple Linear Regression

happy <- read.csv('WorldHappinessReport_2019.csv', header = TRUE) # read file

df <- as.data.frame(happy[,c('Country.or.region', 'Score', 'GDP.per.capita', 
                             'Social.support', 'Healthy.life.expectancy', 'Freedom.to.make.life.choices', 
                             'Generosity','Perceptions.of.corruption')])

happy <- as.data.frame(happy[,c( 'Score', 'GDP.per.capita', 'Social.support', 'Healthy.life.expectancy', 
                                 'Freedom.to.make.life.choices', 'Generosity', 'Perceptions.of.corruption')])

# We can see there is a strong positive relationship between Healthy life expectancy and GDP per Capita.
# First, we fit a multiple linear regression model. Score is the dependent variable.

fit <- lm(Score ~ GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + 
            Generosity + Perceptions.of.corruption, data=happy)

summ_fit <- summary(fit)

summary(fit)

# Since the p-value of Generosity is less than 0.05, it is not significant. 
# Also,  R2 , coefficient of determination, is a measure of goodness of fitness of the model, 
# between 0 and 1. In this case,  R2  equals 0.779, which is relatively a good fit.


fit2 <- lm(Score ~ GDP.per.capita + Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices
           , data=happy)

summary(fit2)


#########################################

# This data follows a Normal Distribution

residplot <- function(fit, nbreaks=20) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE, xlab="Studentized Residual",main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)), add=TRUE, col="blue", lwd=1) 
  lines(density(z)$x, density(z)$y, col="red", lwd=2, lty=2) 
  legend("topright",legend = c( "Normal Curve", "Kernel Density Curve"),lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(fit)

# The higher GDP per Capita and Life Expectancy, the higher the Score.
library('ggplot2')
ggplot(happy) + geom_point(aes(x=Score, y=GDP.per.capita , col=Healthy.life.expectancy))


plot(fit, which = 1) #residuals vs Fitted


plot(fit, which = 2) #Normality


plot(fit, which = 3)


plot(fit, which = 4) #Cook's distance

install.packages("car")
library('car')
influencePlot(fit, main="Influence Plot",sub="Circle size is proportial to Cook's Distance")


outlierTest(fit)
df[148,]
fitted(fit)[148]


avPlots(fit, ask=FALSE) # check linearity


crPlots(fit)


zstates <- as.data.frame(scale(happy)) #standardized
zfit <- lm(Score ~ GDP.per.capita + Social.support + Healthy.life.expectancy + 
             Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, data=happy)
plot(zfit$coefficients,col='dark red', ylab = 'Regression Coefficients',
     pch=19,cex = 2,xlab = '')
grid()
text(seq(1, 7, by=1),par("usr")[3] -0.1,labels = (names(zfit$coefficients)),
     srt = 45, pos = 1, xpd = TRUE)


# Logistic Regression: The first step is to transform Score to Low and High
# Countries with a score higher than the average Happiness Score are in group 'High', 
# and lower or equal to the mean value are in group 'Low'.

happy$Score.index <- factor(happy$Score> mean(happy$Score), levels=c(TRUE,FALSE),labels=c("High", "Low"))

# Next, we generate a logistic regression model.

fit.full<- glm(Score.index ~ GDP.per.capita + Social.support + Healthy.life.expectancy + 
                 Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption, 
               data=happy, family=binomial())
summary(fit.full)

# In this case, only Social Support, Healthy Life Expectancy and Freedom to Make Life Choices are significant, 
# since the p-values are less than 0.05. Residual deviance measure the badness of fit of the 5 predictors. 
# Then we need to test if the drop of deviance between the null model and our model is significant.
fit.null <- glm(Score.index ~ 1, data=happy, family=binomial()) 
anova(fit.null, fit.full, test="Chisq")

# Since the Chi-square test is very small, there is at least one predictor is associate with the outcome.
fit.reduced <- glm(Score.index ~ Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices,
                   data = happy, family=binomial())
summary(fit.reduced)


# P-values shows that only Social Support, Life Expectancy and Freedom are significant, 
# so here is a model with only these predictors. All predictors are significant in the reduced model. 
# The AIC in this model is 88.78 which is smaller and better than the full model.

###############################
# Model selection
###############################
# Best subset selection method

# load library leaps
library(leaps)

# run regsubsets using Score as the response, with data from happy
# store the results in a variable called regfit.full
regfit.full=regsubsets(Score~., data = happy,nvmax = 6)
summary(regfit.full)


# run regsubsets using Score as the response, with data from happy
# store the results in a variable called regfit.full
regfit.full=regsubsets(Score~., data = happy,nvmax = 5)
summary(regfit.full)


# run forward stepwise selection, allowing subsets with up to 6 predictors
regfit.fwd=regsubsets(Score~.,data=happy,nvmax=6, method="forward")
summary(regfit.fwd)
reg.summary = summary(regfit.fwd)

# display the number of predictors for which Cp reaches its minimum
which.min(reg.summary$cp)

# the coefficient estimates for the best model using forward selection method
coef(regfit.fwd,which.min(reg.summary$cp))

#######################################

# Shrinkage methods LASSO

library(glmnet)
# the data frame x will hold the data for predictors
x=model.matrix(Score~.,data=happy)[,-1] 

# the vector y will hold the data for the response, Score in this case
y=happy$Score

# Use 5-fold cross-validation on the whole data, to determine the best lambda value
set.seed(1)
cv.out=cv.glmnet(x,y,nfolds=5,alpha=1)

# plot the MSE observed in the cross-validation as a function of log(lambda)
plot(cv.out)

# determine which lambda minimized the MSE, call it bestlam, and display it
bestlam=cv.out$lambda.min
bestlam

# display coefficients
lasso.final=glmnet(x, y, alpha=1, lambda=bestlam)
coef(lasso.final)


###############################
# Classification method
###############################
# K-Nearest Neighbors

library(class)
set.seed(1)

# Countries with a score higher than the average Happiness Score are in group 'High', 
# and lower or equal to the mean value are in group 'Low'.

happy$Score.index <- factor(happy$Score> mean(happy$Score), levels=c(TRUE,FALSE),labels=c("High", "Low"))


# split data into two subsets
train=sample(1:nrow(happy), nrow(happy)/2) 

# next, we create the part of x and y that will be our training data
# we will call these x.train and y.train
happy.train=happy[train,2:7]
happy.test=happy[-train,2:7]

HighScore.train=happy$Score.index[train]
HighScore.test=happy$Score.index[-train]

# run knn with K=1 and store prediction results in a variable called knn.pred
knn.pred = knn(happy.train,happy.test,HighScore.train,k=1)

# display a table that shows prediction results
table(knn.pred,HighScore.test)
round(prop.table(table(knn.pred,HighScore.test)),2)

# calculate prediction accuracy
mean(knn.pred==HighScore.test)

# run knn with K=3 and store prediction results in a variable called knn.pred
knn.pred = knn(happy.train,happy.test,HighScore.train,k=3)

# display a table that shows prediction results
table(knn.pred,HighScore.test)
round(prop.table(table(knn.pred,HighScore.test)),2)

# calculate prediction accuracy
mean(knn.pred==HighScore.test)

# run knn with K=5 and store prediction results in a variable called knn.pred
knn.pred = knn(happy.train,happy.test,HighScore.train,k=5)

# display a table that shows prediction results
table(knn.pred,HighScore.test)
round(prop.table(table(knn.pred,HighScore.test)),2)

# calculate prediction accuracy
mean(knn.pred==HighScore.test)


###############################

# Decision Trees
# Using the binary response variable and other predictor, we can conduct a decision trees. 
# There are 5 steps. First, we divide the data into 80% of training samples and 20% percent of validation sample.


# create a decision tree that predicts whether the Score.index for a country will be high or low than the mean
library(tree)
set.seed(1)
happy.no.score=happy[,-1] 
tree.happy = tree(happy.no.score$Score.index ~ . , happy.no.score)
summary(tree.happy)

# plot the tree
plot(tree.happy)

# see what the branches are 
text(tree.happy,pretty=0)

# use cross-validation and pruning to obtain smaller trees
cv.happy = cv.tree(tree.happy,FUN=prune.misclass)

# check size and dev
names(cv.happy)

# display the information in cv.happy
# observe that dev is minimum when size is 2
cv.happy

# use a function called prune.misclass() to obtain the best tree
prune.happy = prune.misclass(tree.happy,best=2)

# plot the best tree
plot(prune.happy)

# display branch names on the tree
text(prune.happy, pretty = 0)

##########################










library(ISLR)
set.seed(1234)
train <- sample(nrow(happy), 0.8*nrow(happy)) 
training <- happy[train,]
validation <- happy[-train,]

install.packages("rpart")
library('rpart')
library('rpart.plot')
set.seed(1234)
dtree <- rpart(Score.index ~ GDP.per.capita + Social.support + Healthy.life.expectancy + 
                 Freedom.to.make.life.choices + Generosity, 
               data=training, method="class",parms=list(split="information"))

dtree$cptable
plotcp(dtree)
dtree.pruned <- prune(dtree, cp=.0125) 
prp(dtree.pruned, type = 2, extra = 104,fallen.leaves = TRUE, main="Decision Tree")

# Then, using the predict() function to classify each observation in the validation sample. 
# The table shows the actual numbers vs. predicted values. Based on the table, the prediction is 97% correct.
dtree.pred <- predict(dtree.pruned, validation, type="class")
dtree.perf <- table(validation$Score.index, dtree.pred,dnn=c("Actual", "Predicted"))
dtree.perf

###############################
# Random Forest

library(ISLR)
set.seed(1234)
train <- sample(nrow(happy), 0.8*nrow(happy)) 
training <- happy[train,]

install.packages('randomForest')
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(Score~GDP.per.capita + Social.support + Healthy.life.expectancy + 
                             Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption,
                           data=training,na.action=na.roughfix, importance=TRUE)
fit.forest

importance(fit.forest, type = 2)

# The higher value indicates that the variable is more important than others. 
# We can tell that Social Support has the highest number, which is the most significant, 
# and Generosity is the least important.