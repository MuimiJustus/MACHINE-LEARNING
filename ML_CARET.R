#MACHINE LEARNING USING CARET
library(caret)
#creating an index to split the sets
indic<-createDataPartition(iris$Species,p=0.75,times = 1, list = FALSE)
#indic
#Subset the training data
train_iris<-iris[indic,]
#train_iris
class(train_iris)
#subset the test data
test_iris<-iris[-indic,]
#test_iris

#names(getModelInfo())
#build the model
model<-train(train_iris[,1:4],train_iris[,5], method = 'knn')

#model evaluation
predictions<-predict(object = model,test_iris[,1:4])
table(predictions)
confusionMatrix(predictions,test_iris[,5])