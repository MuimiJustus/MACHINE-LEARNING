#install library ISLR, it contains the dataset Carseats
library(ISLR)
Carseats
library(tree)
hist(Carseats$Sales)
High<-ifelse(Carseats$Sales<=8, "Yes", "No")
Carseats<-data.frame(Carseats,High)
#Carseats
#Excluding the Sales(-sales) column and filling a model using decision tree
tree.Carseats<-tree(High~.-Sales, data = Carseats)
class(tree.Carseats)
summary(tree.Carseats)
names(Carseats)
plot(tree.Carseats)
text(tree.Carseats, pretty = 0)
set.seed(101)
#create index for the training set
trained<-sample(1:nrow(Carseats), 250)
sort(trained, decreasing = FALSE)
#length(trained)
#refit the model and set the subset=trained
tree.Carseats<-tree(High~.-Sales, data = Carseats, subset = trained)
plot(tree.Carseats)
text(tree.Carseats,pretty = 0)
#make the predictions
tree.predicted<-predict(tree.Carseats,Carseats[-trained,],type = "class")
#tree.predicted
#table(tree.predicted)
with(Carseats[-trained,],table(tree.predicted, High))
#take the sum of the diagonals(43+72), off the diagonal are wrongly classfied

#use cross-validation to prune( remove sub-nodes of a decision node) the tree
cv.tree.Carseats<-cv.tree(tree.Carseats,FUN =prune.misclass)
cv.tree.Carseats
class(cv.tree.Carseats)
plot(cv.tree.Carseats)
#from the plot, you see a downward spiral part coz of the misclassification error
#pick the a value in the downward steps=12
#prune the tree to a size of 12 and plot
prune.cv.tree.Carseats<-prune.misclass(tree.Carseats, best = 12)
plot(prune.cv.tree.Carseats)
text(prune.cv.tree.Carseats,pretty = 0)







