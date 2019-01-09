# SUPPORT VECTOR MACHINE IN R. USING PACKAGE CARET

# AUTHOR: MUIMI JUSTUS

#dataset heart_tidy shall be used available at
#http://dataaspirant.com/2017/01/19/support-vector-machine-classifier-implementation-r-caret-package/
# we shallmodel a classifier for predicting whether a patient is 
# suffering from any heart disease or not.

#load the dataset
heart_df<-read.csv("datasets/heart_tidy.csv", header = FALSE,sep = ',')
str(heart_df)


#understand the data
str(heart_df)
class(heart_df)
head(heart_df,5)
dim(heart_df)
colnames(heart_df)
anyNA(heart_df)
sum(is.na(heart_df))

#no missing value as per the above command
#convert the data type of target variable to factor
heart_df$Target_Variable<-as.factor(heart_df$Target_Variable)
levels(heart_df$Target_Variable)

#as seen above the dataset has no headers hence lets name all the columns
colnames(heart_df)<-c("Age","Sex","Cp","Trestbps","Chol","Fbs","restecg","Thalach","Exang",
                      "Oldpeak","Slope","Ca","Thal","Target_Variable")

#documentation of the dataset can be found in the on top
#the target variable is a binary (0 or 1) whether a patient has heart problem or not

#import the library caret
library(caret)

#slice the training data

set.seed(200)

#create the index for partitioning
index<-createDataPartition(heart_df$Target_Variable,p=0.7,list = FALSE)
index

#create the training set
svm_train<-heart_df[index,]


#create the testing data
svm_test<-heart_df[-index,]
class(svm_test)

# By passing values of index, we are splitting training data and testing data.
# The line svm_train<- heart_df[intrain,] is for putting the data from data frame
# to training data. Remaining data is saved in the testing data frame, 
# svm_test<- heart_df[-intrain,].


#before training we pass control parameter to the model
training_control<-trainControl(method="repeatedcv",number=10,repeats=3)

#train the model
#first convert the output variable to a factor
svm_train$Target_Variable<-as.factor(svm_train$Target_Variable)
str(svm_train)
svm_linear_model<-train(Target_Variable~.,data = svm_train,method="svmLinear",
                trControl=training_control, preProcess=c("center", "scale"),tuneLength=10)


#trained model result
svm_linear_model

#test our model using the test dataset
svm_test$Target_Variable<-as.factor(svm_test$Target_Variable)
svm_linear_model_test<-predict(svm_linear_model,newdata = svm_test)
svm_linear_model_test

#evaluate the accuracy of the model using confusion matrix
table(svm_linear_model_test,svm_test$Target_Variable)
confusionMatrix(svm_linear_model_test,svm_test$Target_Variable)

#tune the model using grid search to enhance the accuracy of the model
grid<-expand.grid(C=c(0,0.01,0.05,0.1,0.25,0.5,0.75,1,1.25,1.5,1.75,2,2.5))
svm_linear_model_grid<-train(Target_Variable~.,data = svm_train,method="svmLinear",                
            trControl=training_control, preProcess=c("center", "scale"),
            tuneLength=10,tuneGrid=grid)
svm_linear_model_grid












