# set working directory
setwd("~/Dropbox/JHU Carey MBA/SMA/W1")

# install and library new packages to incorporate new functions
# install rtweet package to retrieve and analyze data from twitter
install.packages("rtweet")
library(rtweet)

# install data.table packages for data operation
install.packages("data.table")
library(data.table)

PPE = read_twitter_csv("PPEManufacturer1118onlinefinal.csv")

# List all the variable names in the data set
names(PPE)

# How many tweets from each manufacturer?
table(PPE$screen_name)

# Get a subset of data Select 3M's Messages
IDX1=PPE$screen_name=="3M"
summary(IDX1)
sum(IDX1)/nrow(PPE)
DATA3M=PPE[IDX1,]

# You can also write the program more efficiently as follows
DATA3M=PPE[PPE$screen_name=="3M",]
DATAHON=PPE[PPE$screen_name=="honeywell",]

# Number of observations
dim(DATA3M)

# Stacking two data sets together

DATANew=rbindlist(list(DATA3M,DATAHON))
dim(DATANew)


# Use summary statistics to describe how successful 3M's Social Media Messages were 
# favorite_count: number of likes
# retweet_count: number of retweets of each message
# quote_count: number of quote retweets of each message
# reply_count: number of replies of each message

summary(DATA3M$favorite_count)
var(DATA3M$favorite_count)
sd(DATA3M$favorite_count)
summary(DATA3M$retweet_count)


summary(DATA3M$quote_count)
summary(DATA3M$reply_count)

# Sort the data by favorite_count in ascending order. 
temp=DATA3M[order(DATA3M$favorite_count),]

# Sort the DATA by favorite_count in descending order. 
temp=DATA3M[order(-DATA3M$favorite_count),]

# What's the most liked message?
temp$text[1:2]

# Create new variables based on the existing variables
temp$interaction_count=temp$favorite_count+temp$retweet_count

# Summary statistics
summary(temp$interaction_count)

# Identify the most engaging tweet 
temp1=temp[order(-temp$interaction_count),]
temp1$text[1:2]

# other data operations:
temp1$t1=temp1$favorite_count-temp1$retweet_count
temp1$t2=temp1$favorite_count*temp1$retweet_count
temp1$t3=temp1$favorite_count/temp1$retweet_count

temp1$t4=temp1$favorite_count+1000
temp1$t5=temp1$favorite_count-1000
temp1$t6=temp1$favorite_count*1000
temp1$t7=temp1$favorite_count/1000
temp1$t8=temp1$favorite_count^2

# Remove the columns t1-t8
temp1=subset(temp1,select=-c(t1,t2,t3,t4,t5,t6,t7,t8))
# Only keep the columns t1-t8
#temp1=subset(temp,select=c(t1,t2,t3,t4,t5,t6,t7,t8))



# Exercise: 
# Find the most engaging tweet based on the interaction_count for honeywell.  
# Copy and paste the output to a word document. 



# Here is the answer
temp=DATAHON

# Create new variables based on the existing variables
temp$interaction_count=temp$favorite_count+temp$retweet_count

# Identify the most engaging tweet 
temp1=temp[order(-temp$interaction_count),]
temp1$text[1]



