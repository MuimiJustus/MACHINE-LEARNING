#nAIVE BAYES CLASSIFIER
#   MUIMI JUSTUS
#guide: https://www.r-bloggers.com/understanding-naive-bayes-classifier-using-r/

#load package e1071 which provides naive bayes training

library(e1071)

#load the Titanic dataset
data("Titanic")

#understand  the dataset
View(Titanic)
class(Titanic)
?Titanic

#convert the datatype from table to dataframe.
Titanic.df<-data.frame(Titanic)
class(Titanic.df)
summary(Titanic.df)
dim(Titanic.df)
colnames(Titanic.df)
nrow(Titanic.df)

#as seen, the count is in col freq
#to represent the complete data, we repeat the sequence instead of using the frequency
repeating_seq<-rep.int(seq_len(nrow(Titanic.df)),Titanic.df$Freq)
length(repeating_seq)
#now create the dataset with rows repeated
Titanic_dataset<-Titanic.df[repeating_seq,]

#freq column is nolonger of interest hence we remove it
Titanic_dataset$Freq<-NULL
colnames(Titanic_dataset)


#the data is ready for naive bayes classifier
#fit the naive bayes model
naive_bayes_model<-naiveBayes(Survived~.,data = Titanic_dataset)
naive_bayes_model

#make the predictions
predicted_naive<-predict(naive_bayes_model,Titanic_dataset)
predicted_naive

#create the confusion matrix for accuracy check
table(predicted_naive,Titanic_dataset$Survived)


#now use naive bayes using mlr package

# The entire structure of this package relies on this premise:
 
#   Create a Task. Make a Learner. Train Them.


#install package mlr
install.packages("mlr")
#load the package
library(mlr)

#create a task
task<-makeClassifTask(data=Titanic_dataset,target ="Survived")

#define the learner
task_learner<-makeLearner("classif.naiveBayes")

#train the model
trained_task_learner<-train(task_learner,task)
trained_task_learner$learner.model

#now make the predictions and convert the output to dataframe
predicted_mlr<-as.data.frame(predict(trained_task_learner,newdata = Titanic_dataset[,1:3]))
colnames(predicted_mlr)
predicted_mlr

#confusion matrix for accuracy check
table(predicted_mlr[,1],Titanic_dataset$Survived)

# As we see, the predictions are exactly same. 
# The only way to improve is to have more features or more data








