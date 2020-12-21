



# https://s3-us-west-2.amazonaws.com/syr-mac/prod/IST+565+Data+Mining/PDFs/Assignments/Project-instructions-updated-11-27-2017.pdf 
# https://archive.ics.uci.edu/ml/datasets/Forest+Fires 


library(readr)
library(ggplot2)

#https://towardsdatascience.com/beginners-guide-to-k-nearest-neighbors-in-r-from-zero-to-hero-d92cd4074bdb

#install.packages("ISLR")
# install.packages(“ggplot2”) # install.packages(“plyr”)
# install.packages(“dplyr”) # install.packages(“class”)# Load libraries
library(ISLR) 
library(ggplot2) 
library(reshape2) 
library(plyr) 
library(dplyr) 
library(class)
library(ggvis)
library(readxl)

# Load files 
forestfires <- read_csv("/Users/jfshands/Downloads/forestfires.csv")
forestfiresEX <-read_excel("~/Downloads/ForestFiresWith(1).xlsx")
forestfires_na_factor <- read_csv("/Users/jfshands/Downloads/forestfires.csv")

# find mean for foest fires
mean(forest)
# Feature generation
## IF the area burned is greater than .5 , equals a significant fire
forestfires$fire_yes_no <- ifelse(forestfires$area>0.5,1,0) 

# Create a new data frame for newly made significant fire data
forestfiresmm <- forestfires %>% select(X,Y,month,day,FFMC,DMC,DC,ISI,temp,RH,wind,rain,area,fire_yes_no) %>% filter(forestfires$fire_yes_no == "1")
forestfiresmm

# Scale OG data frame
forestfires.scaled <- forestfires
forestfires.scaled$FFMC <- scale(forestfires$FFMC)
forestfires.scaled$DMC <- scale(forestfires$DMC)
forestfires.scaled$DC <- scale(forestfires$DC)
forestfires.scaled$ISI <- scale(forestfires$ISI)
forestfires.scaled$temp <- scale(forestfires$temp)
forestfires.scaled$RH <- scale(forestfires$RH)
forestfires.scaled$wind <- scale(forestfires$wind)
forestfires.scaled$rain <- scale(forestfires$rain)
forestfires.scaled$area <- scale(forestfires$area)

# Scale significant fire data frame
forestfiresmm.scaled <- forestfiresmm
forestfiresmm.scaled$FFMC <- scale(forestfiresmm.scaled$FFMC)
forestfiresmm.scaled$DMC <- scale(forestfiresmm.scaled$DMC)
forestfiresmm.scaled$DC <- scale(forestfiresmm.scaled$DC)
forestfiresmm.scaled$ISI <- scale(forestfiresmm.scaled$ISI)
forestfiresmm.scaled$temp <- scale(forestfiresmm.scaled$temp)
forestfiresmm.scaled$RH <- scale(forestfiresmm.scaled$RH)
forestfiresmm.scaled$wind <- scale(forestfiresmm.scaled$wind)
forestfiresmm.scaled$rain <- scale(forestfiresmm.scaled$rain)
forestfiresmm.scaled$area <- scale(forestfiresmm.scaled$area)

# View it
View(forestfires)
# Str 
str(forestfires)

# Descripitive Summary 
summary(forestfires)
(head(forestfires,n=5))

# Save col names in a variable
colnamesff <- colnames(forestfires)




## EDA ##  

# Plot unique variables 3d
library(plotrix)
slices <- c(1:13)
lbls <- colnamesff
pie3D(slices,labels=lbls,explode=0.2,theta=1,radius = 1,
      main="Distribution of unique variables")
# Plot unique variables 2d
colors = c('#4286f4','#bb3af2','#ed2f52','#efc023','#ea7441')
pie(slices, lbls, main='Distribution of unique variables',density=30 ,col=colors, angle=45)


## Check for missing data and make sure no missing data
forestfires[!complete.cases(forestfires),]
sum(is.na(forestfires))

# Create a scatter plot with  variables FFMC and DC filled by month
## View difference between scaled and not scaled
library(ggvis)
#install.packages("ggvis")
forestfires_na_factor %>% ggvis(~FFMC, ~DC, fill = ~month) %>% layer_points() # possible 2 or 3 key clusters

forestfires.scaled %>% ggvis(~FFMC, ~DC,fill=~month)%>% layer_points()

# Any cluster between Temp and DC? 
forestfires.scaled %>% ggvis(~temp, ~DMC, fill = ~area) %>% layer_points() 

## Visual Clusters found  in DC and FFMC !! 
## 1 months aug, sep, nov 
## 2 july, june, DEC
### 3 feb,march, april



#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("factoextra")

library(tidyverse)
library(cluster)
library(factoextra)

library('tidyverse')


# if chr, change to factor using dplyr 

#cluster <- select_(forestfires.scaled,-c(X,Y,month,day))

# create a dataframe for clusting 
## Only keep of interest variables and drop the rest of them !! 
cluster_scaled <- select(forestfiresmm.scaled,-c(X,Y,month,day,DMC,ISI,temp,RH,wind,rain,area,fire_yes_no))


library("factoextra") # cool viz for k means 

# use scaled data since k means is a distance measure
k1 = kmeans(cluster_scaled,centers = 2, nstart = 25)
k2 = kmeans(cluster_scaled,centers = 3, nstart = 25)
k3 = kmeans(cluster_scaled,centers = 4, nstart = 25)
k4 = kmeans(cluster_scaled,centers = 5, nstart = 25)
k5 = kmeans(cluster_scaled,centers = 6, nstart = 25)
k6 = kmeans(cluster_scaled,centers = 7, nstart = 25)
k7 = kmeans(cluster_scaled,centers = 8, nstart = 25)

# plot to compare 
p1 <- fviz_cluster(k1,geom = "point", cluster_scaled)+ggtitle("k=2")
p2 <- fviz_cluster(k2,geom = "point", cluster_scaled)+ggtitle("k=3")
p3 <- fviz_cluster(k3,geom = "point", cluster_scaled)+ggtitle("k=4")
p4 <- fviz_cluster(k4,geom = "point", cluster_scaled)+ggtitle("k=5")
p5 <- fviz_cluster(k5,geom = "point", cluster_scaled)+ggtitle("k=6")
p6 <- fviz_cluster(k6,geom = "point", cluster_scaled)+ggtitle("k=7")
p7 <- fviz_cluster(k7,geom = "point", cluster_scaled)+ggtitle("k=8")

library(gridExtra) # for a grid layout 
grid.arrange(p1,p2,p3,p4,p5,p6,p7,nrow=2)
grid.arrange(p1,p2,p3,nrow=1)


### Analyze the cluster results 

# Function to compute total within cluster sum of square
wss = function(k){kmeans(cluster_scaled,k,nstart = 10)$tot.withinss}

#Compute and plot wss for k =1 to k =15
k.values = 1:15

# Extract wsss for 2-15 clusters
wss_values = map_dbl(k.values,wss)

plot(k.values, wss_values,
     type = "b", pch = 19, frame = FALSE,
     main="Elbow Plot of K-Means Clustering",
     xlab="Number of Clusters K",
     ylab="Total within-clusters sum of squares")

# Silhoette scores

library('cluster')
silhouette_score = function(k){
  km = kmeans(cluster_scaled, centers = k, nstart = 25)
  ss = silhouette(km$cluster,dist(cluster_scaled))
  mean(ss[,3])
}

k=2:10
avg_sil = sapply(k,silhouette_score)
plot(k,type = 'b',avg_sil,xlab = 'number of clusters',ylab = 'average silhouette scores', main="Silhouette Plot of K-Means Clustering",frame ='False')

# Gap statistic 
library("factoextra")
fviz_nbclust(cluster_scaled,kmeans,method = "gap_stat")
# -->  shows 2 optimal clusters  

## View stats within a cluster

cluster_2 <- kmeans(cluster_scaled,centers = 2,nstart = 10)
cluster_2$cluster <- as.factor(cluster_2$cluster)
cluster_2

cluster_3 <- kmeans(cluster_scaled,centers = 3,nstart = 10)
cluster_3$cluster <- as.factor(cluster_3$cluster)
cluster_3

cluster_4 <- kmeans(cluster_scaled,centers = 4,nstart = 10)
cluster_4$cluster <- as.factor(cluster_4$cluster)
cluster_4

cluster_5 <- kmeans(cluster_scaled,centers = 5,nstart = 10)
cluster_5$cluster <- as.factor(cluster_5$cluster)
cluster_5

cluster_6 <- kmeans(cluster_scaled,centers = 6,nstart = 10)
cluster_6$cluster <- as.factor(cluster_6$cluster)
cluster_6

cluster_7 <- kmeans(cluster_scaled,centers = 7,nstart = 10)
cluster_7$cluster <- as.factor(cluster_7$cluster)
cluster_7

ggplot(cluster_2, aes(W1,W44,color=cluster_2$cluster))+geom_point()

ggplot(cluster_3, aes(W1,W44,color =cluster_3$cluster)) +geom_point()


# View counts within cluster 
group1 = data.frame(t(cluster_scaled[cluster_3$cluster == 3,]))
summary(sapply(group1, mean))
hist(sapply(group1, mean), main = "Histogram of Group 3", xlab = "Number of observations")

## Create a training a test set with scaled data 


ind <- sample(2, nrow(forestfires.scaled), replace=TRUE, prob=c(0.67, 0.33)) # Randomize (SHUFFLE) data


forestfires.scaled.training <- forestfires.scaled[ind==1, 4:11]

forestfires.scaled.test <- forestfires.scaled[ind==2, 4:11]

forestfires.scaled.trainLabels <- forestfires.scaled[ind==1, 3]

forestfires.scaled.testLabels <- forestfires.scaled[ind==2, 3]

## MORE TRANSFORMATION of the data using DPLYR 

# Change OG data into factor 
# if numeric, change to factor using dplyr 
forestfires <- forestfires %>% 
  mutate_if(is.numeric,funs(as.factor)) 
str(forestfires)

# if chr, change to factor using dplyr 
forestfires <- forestfires %>% 
  mutate_if(is.character,funs(as.factor)) 
str(forestfires)



# Nice viz to view any null values 

# R for Loop , for value in sequence :: names gives name of a set of object 
#:: CAT concatenate and print :: sum,values that are na, in subset of data
colnames(forestfires)
colname <- colnames(forestfires)


for(colname in names(forestfires)){
  cat("\n","\n Looking at column...", colname)
  NAcount <- sum(is.na(forestfires[colname]))
  cat("\nThe num of missing values in column ", colname, "is  ", NAcount)
}



##### Melt data frame ###### 
#
# Drop useless coloumns 
forestfires.scaled <- select(forestfires.scaled,c(-1,-2))


library(reshape2)
library(tidyr)
mdata <- melt(forestfires.scaled,id=c("month","day"))
mdata %>% drop_na()

# Vis of freq per month
g1 <- ggplot(forestfiresmm,aes(x=forestfiresmm$month,y=forestfiresmm$fire_yes_no))
g1 + geom_bar(stat = "identity",aes(fill=factor(month)))+labs(title = "Significant fires per month")+xlab("Month")+ylab("Freq of Fires")+theme_classic()

# Vis of freq per day
g2 <- ggplot(forestfires,aes(forestfires$day))
g2 + geom_bar(aes(fill=factor(day)))+labs(title = "Freq of observations per day")+xlab("Day")+ylab("Count")+theme_classic()


#install.packages("wesanderson") # this package has nice colors for graphs
#library(wesanderson)

mid <- mean(forestfires.scaled$area) # store the average value for area

# melted data frame , using scaled values 
## Any variables seem to vary by month? or correlated? 
ggplot(data = mdata, aes(x = month, y = value, fill = variable)) + 
  # `geom_col()` uses `stat_identity()`: it leaves the data as is.
  geom_col(position = 'dodge')


# Dot plot, freq of area burned by month
# Basic scatter plot
g1 = ggplot(data = forestfires.scaled, aes_string(x = "month", y = "area")) + 
  geom_point()
# Change the point size, and shape
g1 = g1 + geom_point(size = 1, shape = 23)
g1





# two variables continuous , plot for correlation
avg <- mean(forestfires.scaled$area)
c <- ggplot(forestfires.scaled, aes(ISI, area))
# Default plot 
c + geom_bin2d()
# Change the number of bins
c + geom_bin2d(bins = 15)+geom_hline(aes(yintercept = avg)) 



###### DECISION TREE ######
# Set up
# Plot Observations 
## We see the construction of the forest and points 
plot(forestfires_na_factor$X,forestfires$Y)


## View fire data
summary(forestfires_na_factor$area)


# Note the mean and median
# View points where above average forest fire destruction 
points(forestfires_na_factor$X[forestfires_na_factor$area>=.52], forestfires_na_factor$Y[forestfires_na_factor$area>=.52], col="green", pch=20)


# Check the RH over areas 
## View area
summary(forestfires_na_factor$area)
points(forestfires_na_factor$X[forestfires_na_factor$area>=.52], forestfires_na_factor$Y[forestfires_na_factor$area>=.52], col="red", pch=20)


# View wether the data is linear 
plot(forestfires_na_factor$X,forestfires_na_factor$area)
plot(forestfires_na_factor$Y,forestfires_na_factor$area)


# Linear Regression Model
latlonlm = lm(area ~ X + Y, data = forestfires_na_factor)
summary(latlonlm)
# R-Squared is around .3409 or 34%

# The linear model plots a blue money sign every time it thinks RH is above median value. 
library(rpart)
library(rpart.plot)
# CART model
latlontree = rpart(area ~ X + Y, data=forestfires_na_factor)# Plot the tree using prp command defined in rpart.plot package
prp(latlontree)


fittedvalues = predict(latlontree)


# Simplifying Tree by increasing minBucket
latlontree = rpart(area ~ X + Y, data=forestfires_na_factor, minbucket=50)
plot(latlontree)
text(latlontree)

# Prediction with Regression Trees 
library(MASS)
library(caTools)
set.seed(123)
#split=sample.split(forestfires_na_factor$area, SplitRatio = 0.7)
split=sample.split(forestfires_na_factor$area, SplitRatio = 0.7)
train=subset(forestfires_na_factor, split==TRUE)
test=subset(forestfires_na_factor, split==FALSE)

# CV 
library(tidyr)
CVdata <- dplyr::select(forestfires_na_factor,c(-1,-2))


Split_M <- as.matrix(CVdata)
Papers_M_N1 <- apply(Split_M, 1, function(i) round(i/sum(i),3))
Papers_Matrix_Norm <- t(Papers_M_N1)



# Create a CART model
tree = rpart(area ~ X + Y + FFMC + DMC + ISI + temp + RH + wind + rain, data=train)
prp(tree)

# Regression Tree Predictions
tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$area)^2)
tree.sse




# Visualize regression output
plot(forestfires_na_factor$X, forestfires_na_factor$Y)
points(forestfires_na_factor$X[forestfires_na_factor$area>=.52], forestfires_na_factor$Y[forestfires_na_factor$area>=.52], col="red", pch=20)> latlonlm$fitted.values
points(forestfires_na_factor$X[latlonlm$fitted.values >= .52], forestfires_na_factor$Y[latlonlm$fitted.values >= .52], col="blue", pch="$")

# Create basic x and y cord  plot in ggplot 
library(ggplot2)

pointgg = ggplot(forestfires_na_factor,aes(x = forestfires_na_factor$X, y = forestfires_na_factor$Y))
pointgg = pointgg + geom_point(color="red") 
pointgg = pointgg + scale_y_reverse(breaks = pretty(forestfires_na_factor$Y,n=9)) + scale_x_continuous(position = 'top',breaks = pretty(forestfires_na_factor$X, n = 9))
pointgg = pointgg + labs(title = "Montesinho Natural Park fires",x="",y="")
pointgg

                        
                        