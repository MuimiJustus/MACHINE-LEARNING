#CLASSIFICATION USING RANDOM TREE & BOOSTING

#      AUTHOR: MUIMI JUSTUS

#Install package MASS
#the dataset Boston is located in the installed package
#the dataset has 506 observation on 14 variables

#Load packages required
library(MASS)
library(randomForest)
names(Boston)
#peek the dataset
Boston
# class(Boston)
# table(names(Boston))
# summary(Boston)
# table(Boston)
# dim(Boston)
# ncol(Boston)
#Data preparation
set.seed(100)
#select 300 observations for training
training_data<-sample(1:nrow(Boston),300)

#Create the RF model
RF.Boston<-randomForest(medv~., data = Boston,subset = training_data)
RF.Boston
#set mtry to range from 1:13,ie 13 variables
#set 2 variables to record OOB and test errors
#restrict the number of trees to 350 as opposed to 500
#Fit the RF with those parameters

oob_err=double(13)
test_err=double(13)
#length of 13 double precision for higher accuracy
for( mtry in 1:13){
  fit<-randomForest(medv~.,data = Boston,subset = training_data,mtry=mtry,
                    ntree=350)
  oob_err[mtry]<-fit$mse[350]
  predicted<-predict(fit,Boston[-training_data,])
  #determine the test error
  test_err[mtry]=with(Boston[-training_data,], mean((medv-predicted)^2))
}
# fit
# oob_err
# test_err
#plot the output using matplot
cbind(test_err,oob_err)

# for phc "http://www.endmemo.com/program/R/pchsymbols.php"
matplot(1:mtry,cbind(test_err,oob_err),pch = 23,col = c("red","blue"),
        type = "b", ylab="Mean squared Error")
legend("topright",legend =c("OOB", "TEST"), pch =23,col = c("red","blue"))
# names(Boston)
# table(Boston[4])

#BOOSTING









