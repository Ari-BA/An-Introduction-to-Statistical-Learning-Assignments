library(ISLR)
install.packages("tree")
library(tree)

head(Carseats)
summary(Carseats) #10 columns total
set.seed(1126)

#(a) Split the data set into a training set and a test set. 
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
test.Sales <- Carseats[-train, 1]

#(b) Fit a regression tree to the training set. Plot the tree, and interpret 
#the results. What test MSE do you obtain?
tree.Carseats <- tree(Sales ~., subset = train, data = Carseats)
summary(tree.Carseats) #15 terminal nodes 
#variables actually used in tree construction:
#ShelveLoc, Price, CompPrice, Income, Age
plot(tree.Carseats)
text(tree.Carseats, pretty = 0)
yhat <- predict(tree.Carseats, newdata = Carseats[-train, ])
cbind(yhat, test.Sales)[1:10,]
mean((yhat - test.Sales)^2) #5.055247

#pruning
cv.Carseats <- cv.tree(tree.Carseats)
cv.Carseats
which.min(cv.Carseats$dev)

cv.Carseats$size[1]
prune.Carseats <- prune.tree(tree.Carseats, best = 15)
plot(prune.Carseats)
text(prune.Carseats, pretty = 0)

yhat <- predict(prune.Carseats, newdata = Carseats[-train, ])
test.Sales <- Carseats[-train, 1]
cbind(yhat, test.Sales)[1:10,]
mean((yhat - test.Sales)^2)

#(d) Use the bagging approach in order to analyze this data. 
#What test MSE do you obtain? Use the importance() function to 
#determine which variables are most important.
install.packages("randomForest")
library(randomForest)
set.seed(1126)
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
bag.Carseats <- randomForest(Sales ~., subset = train, data = Carseats, 
                             mtry = 10, importance = TRUE)
bag.Carseats
test.Sales <- Carseats[-train, 1]
yhat.bag <- predict(bag.Carseats, newdata = Carseats[-train,])
cbind(yhat.bag, test.Sales)[1:10,]
mean((yhat.bag - test.Sales)^2) #2.588667
varImpPlot(bag.Carseats) 

#(e) Use random forests to analyze this data. What test MSE do you obtain? 
#Use the importance() function to determine which variables are most important. 
#Describe the effect of m, the number of variables considered at each split, on 
#the error rate obtained.

rf.Carseats <- randomForest(Sales ~., subset = train, data = Carseats, mtry = 7)
yhat.rf <- predict(rf.Carseats, newdata = Carseats[-train,])
cbind(yhat.rf, test.Sales)[1:10,]
mean((yhat.rf - test.Sales)^2) #2.396501
varImpPlot(rf.Carseats)

