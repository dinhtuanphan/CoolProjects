# TASK 1 #

# explore the data
head(USArrests, n=2)
help(USArrests)


# TASK 2 #

# set random seed so that our results are reproducible
set.seed(1)

# run hierarchical clustering, using complete as your measure of similarity
# store the results in a variable called hc.complete
hc.complete=hclust(dist(USArrests),method="complete")

# plot the dendrogram
plot(hc.complete)

# TASK 3 #

# cut the dendrogram to obtain three clusters and assign the output to variable, say hc.out
# notice the output, which tells you the the cluster for each observation
hc.out <- cutree(hc.complete,3)
hc.out

# The dataframe does not have a column with the states' names
# we will add such a column to the dataframe 
# create a column in the data, called State which holds the row names
USArrests$State <- rownames(USArrests)


# plot each state, showing Murders on x-axis and Assaults on y-axis 
plot(USArrests$Murder,USArrests$Assault,col=hc.out,xlab= "# Murders", ylab="# Assaults")

#display the state names on the plot
text(x=USArrests$Murder, y=USArrests$Assault - 0.15, labels =USArrests$State, col=hc.out)

# TASK 4 #

# scale the data and save the scaled data in a data.frame called USArrests.scaled
# before scaling, I am removing column 5, which holds the name of the State 
# (I added this to the data above)
USArrests.scaled <- scale(USArrests[,-5])
head(USArrests.scaled, n=2)

# run hierarchical clustering, using complete as your measure of similarity
# store the results in a variable called hc.complete.scaled
hc.complete.scaled=hclust(dist(USArrests.scaled),method="complete")

# plot the dendrogram
plot(hc.complete.scaled)

