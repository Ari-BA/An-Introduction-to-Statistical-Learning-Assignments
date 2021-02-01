# Chapter 2 Homework
# Number 9

#Establish working directory (Session-->Set working directory 
#--> Choose)

# Header 1 

#Then, load data, removing missing values:
Auto <- read.csv("Section 2.4 Numbers 7 and 9_Auto.csv", header=T, na.strings ="?") 
dim(Auto) #show record number

Auto = na.omit(Auto) #omit rows with missing observations
dim(Auto) #show new record number

#PART A: Which of the predictors are quantitative, and which 
#are qualitative? ####
summary(Auto) #this will give the five number summary of each 
#ASvariable: minimum, 1st Quartile, median, 3rd Quartile, 
#maximum and also the mean
fix(Auto)
attach(Auto)
str(Auto)

#PART B: What is the range of each quantitative predictor? ####
range(Auto$mpg)
range(Auto$cylinders) 
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)

#PART C: What is the mean and standard deviation of 
#each quantitative predictor? ####
summary(Auto) #this will give the five number summary of each 
# variable: minimum, 1st Quartile, median, 3rd Quartile, 
#maximum and also the ###MEAN### 
###SD:### 
sd(Auto$mpg) 
sd(Auto$cylinders) 
sd(Auto$displacement)
sd(Auto$horsepower)
sd(Auto$weight)
sd(Auto$acceleration)

#########
#PART D: Now remove the 10th through 85th observations. 
#What is the range, mean, and standard deviation of each 
#predictor in the subset of the data that remains?
new.Auto = subset(Auto[-c(10:85),])
sapply(new.Auto[,-c(9)],range)
sapply(new.Auto[,-c(9)],mean)
sapply(new.Auto[,-c(9)],sd)
#########

#PART E: Using the full data set, investigate the predictors 
#graphically, using scatterplots or other tools of your choice. 
#Create some plots highlighting the relationships among the 
#predictors. Comment on your findings.

fac.cylinders <- as.factor(cylinders) 
cylinders <- as.factor(cylinders)
Auto$cylinders <- as.factor(cylinders)
summary(Auto)

fac.year <- as.factor(year) 
year <- as.factor(year)
Auto$year <- as.factor(year)
summary(Auto)

fac.origin <- as.factor(origin) 
origin <- as.factor(origin)
Auto$origin <- as.factor(origin)
summary(Auto)

fac.name <- as.factor(name) 
name <- as.factor(name)
Auto$name <- as.factor(name)
summary(Auto)

pairs(Auto)
pairs(Auto,panel=panel.smooth,main="scatter plots of all variable pairs")
plot(fac.cylinders, mpg, col="gold", xlab="cylinders", ylab="MPG",main="MPG by Number of Cylinders")
plot(fac.origin,df.auto$mpg,names=(c("United States","Europe","Japan")), 
     col="blue", xlab="Country of Origin", ylab="MPG",
     main="MPG by Country of Origin")
plot(fac.year, mpg, col="orange", xlab="Year", ylab="MPG",main="MPG by Year of Manufacture")
plot(Auto$mpg, Auto$weight, col="green", xlab="MPG", ylab ="Weight",main="MPG by Weight")
plot(Auto$mpg, Auto$displacement, col="red", xlab="MPG", ylab ="Displacement",main="MPG by Displacement")
plot(Auto$mpg, Auto$horsepower, col="purple", xlab="MPG", ylab ="Horsepower",main="MPG by Horsepower")
pairs(~ mpg + horsepower + weight + displacement + cylinders + year + origin, data = Auto, panel = panel.smooth)
pairs(~ mpg + cylinders + year + origin, data = Auto)
pairs(~ mpg + horsepower + weight + displacement, data = Auto, panel = panel.smooth)
plot(fac.name, mpg, col="orange", xlab="Make and Model", ylab="MPG",main="MPG by Make and Model")

#PART F: Suppose that we wish to predict gas mileage (mpg) on 
#the basis of the other variables. Do your plots suggest that 
#any of the other variables might be useful in predicting mpg? 
#Justify your answer.

plot(fac.cylinders, mpg, col="gold", xlab="cylinders", ylab="MPG",main="MPG by Number of Cylinders")
plot(factor(Auto$origin),Auto$mpg,names=(c("United States","Europe","Japan")), col="blue", xlab="Country of Origin", ylab="MPG",main="MPG by Country of Origin")
plot(fac.year, mpg, col="orange", xlab="Year", ylab="MPG",main="MPG by Year of Manufacture")
plot(Auto$mpg, Auto$weight, col="green", xlab="MPG", ylab ="Weight",main="MPG by Weight")
plot(Auto$mpg, Auto$displacement, col="red", xlab="MPG", ylab ="Displacement",main="MPG by Displacement")
plot(Auto$mpg, Auto$horsepower, col="purple", xlab="MPG", ylab ="Horsepower",main="MPG by Horsepower")
pairs(~ mpg + cylinders + year + origin, data = Auto)
pairs(~ mpg + horsepower + weight + displacement, data = Auto, panel = panel.smooth)
