#INTRODUCTION TO MACHINE LEARNING USING IRIS DATA SET
#load the data set
data("iris")
iris
#peek the data
head(iris)
#levels in species
a<-levels(iris$Species)
a

#understand the data
#visualizing the relation between sepal.length and sepal.width using ggvis

library(ggvis)
plot1<-ggvis(iris, x=~Sepal.Length, y=~Sepal.Width)

#for ggvis to display, we call function layer_points()
layer_points(plot1)
 
#alternatively, the command below could be used
#layer_points(ggvis(iris, x=~Sepal.Length, y=~Sepal.Width))
#using piping operator %>%
#iris %>% ggvis(iris, x=~Sepal.Length, y=~Sepal.Width) %>%
#  layer_points()

library(dplyr)
#setting the fill to represent species
iris%>%ggvis(x=~Sepal.Length,y=~Sepal.Width, fill=~Species, size=~Petal.Length)%>%layer_points()

#plot for petals
iris %>% ggvis(x=~Petal.Length,y=~Petal.Width, fill=~Species)
#overal correlation
cor(iris$Sepal.Length,iris$Sepal.Width)
cor(iris$Petal.Length,iris$Petal.Width)

#correlation matrix for individual species
#1 SETOSA
print(a[1])
cor(iris[iris$Species==a[1],1:4])

#2.VERSICOLOR
print(a[2])
cor(iris[iris$Species==a[1],1:4])

#3 VIRGINICA

print(a[3])
cor(iris[iris$Species==a[1],1:4])

table(iris$Species)
print(colnames(iris))
set.seed(1234)
iris1<-sample(2,nrow(iris),replace = TRUE,prob = c(0.67,0.33))
iris1
#compose the training data
train.iris<-iris[iris1==1,1:4]
head(train.iris)
dim(train.iris)
#compose test data
test.iris<-iris[iris1==2,1:4]
nrow(test.iris)
head(test.iris)

#composing labels
train.iris.labels<-iris[iris1==1,5]
train.iris.labels
#length(train.iris.labels)

test.iris.labels<-iris[iris1==2,5]
test.iris.labels

#length(test.iris.labels)
library(class)
#building the model
predicted_iris<-knn(train = train.iris,test = test.iris,cl=train.iris.labels, k=3)
predicted_iris
#length(predicted_iris)
#Evaluation of the model
test_iris_labels<-data.frame(test.iris.labels)
marje<-data.frame(predicted_iris,test.iris.labels)
marje
#cross tabulation
library(gmodels)
CrossTable(x=test.iris.labels,y=predicted_iris, prop.chisq = FALSE)
#the chi-square contribution of each cell is not included






