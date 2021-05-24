#######################################################
############# BU.510.650 - Data Analytics, Fall 2020
#######################################################

## Thuc Tram (Kate) Phan - Assignment 2 ##

# read the data in AutoLoss.csv to a data frame called AutoLoss
AutoLoss<-read.csv("AutoLoss.csv")

# replaces ?s with NA while reading the data from the .csv file
AutoLoss <- read.csv("AutoLoss.csv", na.strings = "?")

# remove all the observations with any NA.
AutoLoss <- na.omit(AutoLoss)

# Question 1 #

# number of automobiles in each body style
m = table(AutoLoss$BodyStyle)

# proportion of automobiles in each body style
prop.table(m)

# average loss for each body style
aggregate(Losses~BodyStyle, data=AutoLoss,mean)

# average loss for each combination of body style and number of doors
aggregate(Losses~BodyStyle+NumDoors, data=AutoLoss,mean)

# subset of the data that includes only automobiles with rear-wheel drive
AutoLossRwd <- subset(AutoLoss, DriveWheels=="rwd")

# average loss for each body style with rear-wheel drive
aggregate(Losses~BodyStyle, data=AutoLossRwd,mean)

# Question 2 #
# average loss for cars with two doors versus four doors                   
AutoLossNumDoors <- tapply(AutoLoss$Losses, AutoLoss$NumDoors, mean)

# display a bar chart
barplot(AutoLossNumDoors)

# Create a new table t1, which will have our data in increasing order of the first column, 
# which is the loss
t1 <- AutoLoss[order(AutoLoss[,1],decreasing=FALSE),]

# Pick only the first 10 rows of t1 - these are 10 cars with the lowest losses
t2 <- t1[1:10,]

# average loss for 10 cars with each body style                  
AutoLoss10Cars <- tapply(t2$Losses, t2$BodyStyle, mean)

# average loss for cars with different types of Drive wheels                   
AutoLossDriveWheels <- tapply(AutoLoss$Losses, AutoLoss$DriveWheels, mean)

# display a bar chart
barplot(AutoLossDriveWheels)


# plot Price versus Losses
plot(AutoLoss$Price, AutoLoss$Losses)

