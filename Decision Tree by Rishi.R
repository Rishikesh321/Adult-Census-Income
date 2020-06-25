setwd("F:\\Material Building for softanbees\\Demo Assignment\\Decission Tree Tutorial")

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(caTools)
library(rattle)
library(caret)

bank = read.csv("adult.csv")

attach(bank)

levels(bank$income)

summary(bank)

#Splitting

set.seed(213354)

split = sample.split(bank,SplitRatio = 0.7)

train  = subset(bank,split == "TRUE")

test = subset(bank,split == "FALSE")

#Lets create our first tree

tree = rpart(income~.,data = train,method="class")

#rpart syntax takes 'dependent attribute' and the rest of the attributes are independent in the analysis.

#rpart() returns a Decison tree created for the data.

#If you plot this tree, you can see that it is not visible, due to the limitations of the plot window in the R console.

fancyRpartPlot(tree)

# Validation of decision tree using the 'Complexity Parameter' and cross validated error :

# To validate the model we use the printcp and plotcp functions. 'CP' stands for Complexity Parameter of the tree.

#Syntax : printcp ( x ) where x is the rpart object.

#This function provides the optimal prunings based on the cp value.

#We prune the tree to avoid any overfitting of the data. The convention is to have a small tree and the one with least cross validated error given by printcp() function i.e. 'xerror'.

#To find out how the tree performs, is calculated by the printcp() function, based on which we can go ahead and prune the tree.
printcp(tree)

#From the above mentioned list of cp values, we can select the one having the least cross-validated error and use it to prune the tree.

#The value of cp should be least, so that the cross-validated error rate is minimum.

#To select this, you can make use of this :
  
# fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

#This function returns the optimal cp value associated with the minimum error.

#Let us see what plotcp() function fetches.

plotcp(tree)

#Plotcp() provides a graphical representation to the cross validated error summary. The cp values are plotted against the geometric mean to depict the deviation until the minimum value is reached.

#Prune the tree to create an optimal decision tree :

ptree = prune(tree,cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])

fancyRpartPlot(ptree, uniform=TRUE,main="Pruned Classification Tree")

pred = predict(ptree,test,type = "class")


confusionMatrix(test$income,pred)
