install.packages("ISLR")
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
install.packages("jsonlite", type = "source")
library(ISLR) # library() function is used to load libraries
library(MASS)
Carseats
attach(Carseats)
head(Carseats) 
names(Carseats) # check variable names in data
summary(Carseats) # five number summary statistics for each predictor
?Carseats

#3.7, Chapter 3 Linear Regression
#question 10
#his question should be answered using the Carseats data set.

#part a: Fit a multiple regression model to predict Sales 
#using Price, Urban, and US.
mlr <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(mlr)

#[see pdf for parts b - d]

#part e: On the basis of your response to the previous question, fit a smaller 
#model that only uses the predictors for which there is evidence of 
#association with the outcome.

loseUrban <- lm(Sales ~ Price + US, data = Carseats)
summary(loseUrban)

#######
#13. In this exercise, you will create some simulated data and will fit simple 
#linear regression models to it. Make sure to use set.seed(1) prior to starting 
#part (a) to ensure consistent results. 

#part a: Using the rnorm() function, create a vector, x, containing 100 
#observations drawn from a N(0, 1) distribution. This represents a feature, X.
set.seed(1)
x <-rnorm(100)

#part b: Using the rnorm() function, create a vector, eps, containing 100 
#observations drawn from a N(0, 0.25) distribution i.e. a normal distribution 
#with mean zero and variance 0.25.

eps <- rnorm(100, mean = 0, sd = sqrt(0.25))

#part c: Using x and eps, generate a vector y according to the model 
# Y = -1 + 0.5X + epsilon
#What is the length of the vector y? What are the values of beta-0 and beta-1
#in this linear model?

y <- -1 + 0.5*x + eps
length(y)

#part d: Create a scatterplot displaying the relationship between x and y. 
#Comment on what you observe. 
plot(x, y)
abline(lm(y ~ x),col = "red")

#part e: Fit a least squares linear model to predict y using x. 
#Comment on the model obtained. How do beta-0-hat and beta-1-hat compare
#to beta-0 and beta-1?
lm.fit <- lm(y ~ x)
summary(lm.fit)

#part f: Display the least squares line on the scatterplot obtained in (d). 
#Draw the population regression line on the plot, in a different color. 
#Use the legend() command to create an appropriate legend.
abline(lm.fit, col = "red")
abline(-1, 0.5, col = "blue")
legend("topleft", c("Least squares line", "Population regression line"), 
       col = c("red", "blue"), lty = c(1, 1))

#part g: Now fit a polynomial regression model that predicts y using x and x2. 
#Is there evidence that the quadratic term improves the model fit? Explain your answer.
polyrgrsn <- lm(y ~ x + I(x^2))
summary(polyrgrsn)
anova(lm.fit)
anova(polyrgrsn)
