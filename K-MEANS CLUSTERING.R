#K-MEANS CLUSTERING USING IRIS DATASET
#author: MUIMI JUSTUS
#explore the dataset
data("iris")
colnames(iris)
library(ggplot2)
ggplot(iris, aes(Sepal.Length,Petal.Width,color=Species))+geom_point()
ggplot(iris, aes(Sepal.Length,Sepal.Width,color=Species))+geom_point()
ggplot(iris,aes(Petal.Length,Petal.Width,colour=Species))+geom_point()

#from the 3rd plot plot, its seen that, Petal.length & 
#Petal.Width were similar among similar species but varied significantly among different species

#CLUSTERING
#set seed to ensure reproducibility and the number of clusters to 3
set.seed(30)
cluster_iris<-kmeans(iris[,3:4],3,nstart = 30)
cluster_iris
table(cluster_iris$cluster, iris$Species)

# determining the number of clusters,
# in the iris dataset, its obvious that, there are 3 clusters, but in a case where the 
# that could not be seen, the optimal no. of clusters can be established using Elbow method.

#computing the total within-cluster sum of squares
set.seed(30)
wss<-function(k){
    kmeans(iris[,1:4],k,nstart = 30)$tot.withinss
}

#compute wss for k=1 to k=15
k.values<-1:15

#extract the wss values for each k value
library(purrr)
wss.value<- map_dbl(k.values,wss)

#plot no. of clusters against tot.withinss

plot(k.values, wss.value,xlab = "No. of clusters", type = "b"
     ,ylab="Total within-clusters sum of squares")
#from the plot it can be seen that, the optimal no. of clusters is 3










