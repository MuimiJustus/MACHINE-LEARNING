churn<-read.csv("datasets/Telco_churn.csv")
head(churn)
str(churn)
dim(churn)
sum(is.na(churn))
sapply(churn, function(x) sum(is.na(x)) )
churn<-churn[complete.cases(churn),]
summary(churn)

grep("OnlineSecurity",colnames(churn))


library(dplyr)
library(plyr)
cols_to_amend<-c(10:15)
for (i in 1:ncol(churn[cols_to_amend])) {
  churn[,cols_to_amend][,i]<-as.factor(mapvalues(churn[,cols_to_amend][,i],
      from = c("No internet service"), to=("No") ))
}
churn$OnlineSecurity
grep("MultipleLines", colnames(churn))
churn$MultipleLines<-mapvalues(churn$MultipleLines, from =c("No phone service"), to=c("No"))
churn$tenure_group<-cut(churn$tenure,breaks = c(0,12,24,48,60,72),
                        labels=c("0-12","12-24","24-48","48-60",">60"))
str(churn$tenure_group)
churn$SeniorCitizen<-mapvalues(churn$SeniorCitizen, from = c("0","1"), to=c("No","Yes"))

churn$customerID<-NULL
churn$tenure<-NULL
#names(churn)

num_var<-sapply(churn,is.numeric)
#corr<-cor(churn$MonthlyCharges,churn$TotalCharges)
corr_matrix<-cor(churn[,num_var])
corr_matrix
library(corrplot)
corrplot(corr_matrix,method = "number")

churn$TotalCharges<-NULL

library(ggplot2)

ggplot(churn, aes(x=MonthlyCharges))+geom_bar()

ggplot(churn,aes(x=tenure_group,y=MonthlyCharges))+geom_point(aes(col=MultipleLines))
ggplot(churn, aes(x=SeniorCitizen))+geom_area(fill)

ggplot(churn, aes(x=tenure_group))+geom_abline
ggplot(churn,aes(x=gender))+geom_bar()+ggtitle("Gender")

#TO PLOT THE OTHER CATEGORICAL VAR
ggplot(churn,aes(x=SeniorCitizen))+geom_bar(aes(fill=PaymentMethod))
ggplot(churn, aes(x=Partner))+geom_bar()

ggplot(churn,aes(x=PhoneService))+geom_bar()
ggplot(churn,aes(x=Contract))+geom_bar()
ggplot(churn,aes(x=PaymentMethod))+geom_bar(fill="red")+ggtitle("payment methods")
ggplot(churn,aes(x=Churn))+geom_bar()
ggplot(churn,aes(x=tenure_group))+geom_bar()
#TO PLOT THE OTHER CATEGORICAL VAR


#LOGISTIC REGRESSION
library(caret)
set.seed(2000)
partitioning<-createDataPartition(churn$Churn, p=0.7,list = FALSE)

training_set<-churn[partitioning,]
testing_set<-churn[-partitioning,]

dim(training_set)
dim(testing_set)

GLM_model<-glm(formula =Churn~.,family=binomial(link = "logit"),data=training_set)
summary(GLM_model)






