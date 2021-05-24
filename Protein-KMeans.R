# TASK 1 #

# After setting your working directory, read the data from protein.csv
# into a data.frame called food
food <- read.csv( "protein.csv" )

# check the data 
head(food, n=2)

# TASK 2 # 

# set random seed so that our results are reproducible
set.seed(1)

# store the white and red meat consumptions in a separate data.frame called food2
food2<-food[,c(2,3)]

# cluster the contries based on white and red meat consumption
# form three clusters and use 10 initializations
# store the results in a variable called Meats
Meats <- kmeans(food2,3,nstart=10)

# TASK 3 # 

# display the results
Meats

# plot all countries, with Red Meat on x-axis and White Meat on y-axis
plot(food$RedMeat, food$WhiteMeat, xlab="Red Meat", ylab="White Meat")

# use different colors for clusters
plot(food$RedMeat, food$WhiteMeat,col=Meats$cluster, xlab="Red Meat", ylab="White Meat")

# use different shapes for clusters
plot(food$RedMeat, food$WhiteMeat, pch=Meats$cluster, xlab="Red Meat", ylab="White Meat")

# use different colors and shapes for clusters
plot(food$RedMeat, food$WhiteMeat,col=Meats$cluster, pch=Meats$cluster, xlab="Red Meat", ylab="White Meat")

# display the country name below each point
text(x=food$RedMeat,y=food$WhiteMeat-0.25,labels=food$Country,col=Meats$cluster)

# TASK 4 # 

# cluster the contries based on white and red meat consumption
# form five clusters and use 10 initialization
# store the results in a variable called Meats
Meats <- kmeans(food2,5,nstart=10)

# display the results
Meats

# TASK 5 #
# cluster the contries based on all types of protein
# form seven clusters and use 10 initializations
# store the results in a variable called Protein
Protein <- kmeans(food[,-1],7,nstart=10)

# display the results
Protein

# plot all countries, with Red Meat on x-axis and White Meat on y-axis,
# using different colors and shapes for different clusters
plot(food$RedMeat, food$WhiteMeat, col=Protein$cluster,pch=Protein$cluster,xlab="Red Meat", ylab="White Meat") 

#display the country name below each point
text(x=food$RedMeat,y=food$WhiteMeat-0.25,labels=food$Country,col=Protein$cluster)

