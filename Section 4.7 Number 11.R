#HW3
#Section 4.7 p. 183
#Question 11 


library(ISLR)
install.packages("FNN")

#part a): CONVERT MPG TO BINARY

head(Auto)
summary(Auto)
attach(Auto)
set.seed(1)
median(Auto$mpg) #median of mpg is 22.75
mpg.df <- data.frame(Auto)
mpg.median <- median(mpg.df$mpg)
mpg.df$mpg01 <- ifelse(mpg.df$mpg > mpg.median, 1, 0)
# 1 = mpg above mpg median
# 0 = mpg at or below mpg median

head(mpg.df)
summary(mpg.df)
names(mpg.df)

#part b): VISUALIZE
#library(corrplot)
#cor(Auto[, -9])
#corrplot::corrplot.mixed(cor(Auto[, -9]), upper="circle")

fac.cylinders <- as.factor(cylinders) 
cylinders <- as.factor(cylinders)
mpg.df$cylinders <- as.factor(cylinders)
summary(mpg.df)

fac.year <- as.factor(year) 
year <- as.factor(year)
mpg.df$year <- as.factor(year)
summary(mpg.df)

fac.origin <- as.factor(origin) 
origin <- as.factor(origin)
mpg.df$origin <- as.factor(origin)
summary(mpg.df)
mpg.df

#boxplots:
boxplot(horsepower ~ mpg01, data = mpg.df)
boxplot(weight ~ mpg01, data = mpg.df)
boxplot(displacement ~ mpg01, data = mpg.df)
boxplot(acceleration ~ mpg01, data = mpg.df)

#unique(mpg.df$cylinders)
brplt.cylinder <- table(mpg.df$cylinder, mpg.df$mpg01) 
barplot(brplt.cylinder, col = c("lightblue", "orange", "lightgreen", "violet","gold"), 
        main="MPG by Number of Cylinders", 
        beside=TRUE)
legend("topright", legend=c("3 cyl", "4 cyl", "5 cyl", "6 cyl", "8 cyl"),
       col = c("lightblue", "orange", "lightgreen", "violet","gold"), 
       cex = 1, pch = 19)

#unique(mpg.df$year)
brplt.year <- table(mpg.df$year, mpg.df$mpg01) 
barplot(brplt.year, col = c("lightblue", "orange", "lightgreen", "violet","gold"), 
        main="MPG by Year", 
        beside=TRUE, legend = rownames(brplt.year))

brplt.origin <- table(mpg.df$origin, mpg.df$mpg01) 
barplot(brplt.origin, col = c("lightblue", "orange", "lightgreen"), 
        main="MPG by Country of Origin",
        beside=TRUE)
legend("topright", legend=c("United States","Europe","Japan"),
       col = c("lightblue", "orange", "lightgreen"), cex = 1, pch = 19)
#beside = TRUE removes stacked feature
#cex = size
#pch = color-filled shape in legend

#part c: SPLIT
set.seed(1011)
train <- sort(sample(1:dim(mpg.df)[1], 196))
Auto.train <- mpg.df[train, ]
Auto.test <- mpg.df[-train, ]

#part d: LDA
library(MASS)
lda.fit.mpg01 <- lda(mpg01 ~ horsepower + weight + displacement + cylinders + year + origin, data = Auto.train)
lda.pred.mpg01 <- predict(lda.fit.mpg01, Auto.test)
lda.table <- table(lda.pred.mpg01$class, Auto.test$mpg01)
#mean(lda.pred.mpg01$class == Auto.test$mpg01)
#one.minus.mean <-1 - mean(lda.pred.mpg01$class == Auto.test$mpg01) 
#one.minus.mean
lda.test.error <- (lda.table[1,2] + lda.table[2,1])/sum(lda.table)
lda.test.error

#part e: QDA
qda.fit.mpg01 = qda(mpg01 ~ horsepower + weight + displacement + cylinders + year + origin, data = Auto.train)
qda.pred.mpg01 <- predict(qda.fit.mpg01, Auto.test)
names(qda.pred.mpg01) 
qda.table <- table(qda.pred.mpg01$class, Auto.test$mpg01)
#mean(qda.pred.mpg01$class == Auto.test$mpg01)
#one.minus.mean <-1 - mean(qda.pred.mpg01$class == Auto.test$mpg01) 
#one.minus.mean
qda.test.error <- (qda.table[1,2] + qda.table[2,1])/sum(qda.table)
qda.test.error

#part f: Logistic Regression
auto.lgrgsn <- glm(mpg01 ~ horsepower + weight + displacement + cylinders + year + origin, data = Auto.train, family = binomial)
auto.prob <- predict(auto.lgrgsn, type = "response")
auto.prob[1:10]
auto.lr.pred <- as.factor(ifelse(auto.prob> 0.5, 1, 0))
logit.table <-table(auto.lr.pred,Auto.test$mpg01)
#mean(auto.lr.pred==Auto.test$mpg01) 
#one.minus.mean <-1 - mean(auto.lr.pred==Auto.test$mpg01) 
#one.minus.mean
logit.test.error <- (logit.table[1,2] + logit.table[2,1])/sum(logit.table)
logit.test.error

#part g: KNN Regression

# KNN with k = 3
library(FNN)
x.train <- cbind(mpg.df$horsepower, mpg.df$weight, mpg.df$displacement, mpg.df$cylinders, mpg.df$year, mpg.df$origin)[train, ]
x.test <- cbind(mpg.df$horsepower, mpg.df$weight, mpg.df$displacement, mpg.df$cylinders, mpg.df$year, mpg.df$origin)[-train, ]
y.train <- mpg.df$mpg01[train] 
y.test <- mpg.df$mpg01[-train]
knn.k3.pred <- knn(x.train, x.test, y.train, k = 3)
knn.k3.table <- table(knn.k3.pred, y.test)
knn.k3.test.error <- (knn.k3.table[1,2] + knn.k5.table[2,1])/sum(knn.k3.table)
knn.k3.test.error

# KNN with k = 5
knn.k5.pred <- knn(x.train, x.test, y.train, k = 5)
knn.k5.table <- table(knn.k5.pred, y.test)
knn.k5.test.error <- (knn.k5.table[1,2] + knn.k5.table[2,1])/sum(knn.k5.table)
knn.k5.test.error

# KNN with k = 7
knn.k7.pred <- knn(x.train, x.test, y.train, k = 7)
knn.k7.table <- table(knn.k7.pred, y.test)
knn.k7.test.error <- (knn.k7.table[1,2] + knn.k7.table[2,1])/sum(knn.k7.table)
knn.k7.test.error

# KNN with k = 9
knn.k9.pred <- knn(x.train, x.test, y.train, k = 9)
knn.k9.table <- table(knn.k9.pred, y.test)
knn.k9.test.error <- (knn.k9.table[1,2] + knn.k9.table[2,1])/sum(knn.k9.table)
knn.k9.test.error

# KNN with k = 15
knn.k15.pred <- knn(x.train, x.test, y.train, k = 15)
knn.k15.table <- table(knn.k15.pred, y.test)
knn.k15.test.error <- (knn.k15.table[1,2] + knn.k15.table[2,1])/sum(knn.k15.table)
knn.k15.test.error

# KNN with k = 25
knn.k25.pred <- knn(x.train, x.test, y.train, k = 25)
knn.k25.table <- table(knn.k25.pred, y.test)
knn.k25.test.error <- (knn.k25.table[1,2] + knn.k25.table[2,1])/sum(knn.k25.table)
knn.k25.test.error

# KNN with k = 50
knn.k50.pred <- knn(x.train, x.test, y.train, k = 50)
knn.k50.table <- table(knn.k50.pred, y.test)
knn.k50.test.error <- (knn.k50.table[1,2] + knn.k50.table[2,1])/sum(knn.k50.table)
knn.k50.test.error
