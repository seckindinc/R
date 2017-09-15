#Package Loading
library(readr)
library(e1071)

#Support Vector Regression

#Step 1: Data Understanding

#Gathering Data
Risk <- read_delim("/home/seckindinc/Desktop/Projects/R/Data/Risk.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#Structre of Data
str(Risk)

#Summary of Data
summary(Risk)

#Selecting Integer Fields excep ID column
integer_fields = subset(Risk[,sapply(Risk, is.numeric)], select=-ID)

#Train and Test Data
dt = sort(sample(nrow(integer_fields), nrow(integer_fields)*.7))
train <- integer_fields[dt,]
test <- integer_fields[-dt,]

#Modelling
model <- svm(INCOME ~ ., data = train)

#Predicted value
train$predict <- model$fitted
#INCOME mean
INCOME_train_mean <- mean(train$INCOME)
#Total Sum of Squares
tss_train <- sum((train$INCOME - INCOME_train_mean)^2)
#Residuals
train$residuals <- train$INCOME - train$predict
#Residual Sum of Squares
rss_train <- sum(train$residuals^2)
#R-squared
rsq_train <- 1 - (rss_train/tss_train)
#RMSE Test
rmse_train <- sqrt(mean(train$residuals^2))

#Different Kernel Types
model <- svm(INCOME ~ ., data = train, kernel = "linear")
model2 <- svm(INCOME ~ ., data = train, kernel = "polynomial")
model3 <- svm(INCOME ~ ., data = train, kernel = "sigmoid")

#Plotting prediction vs INCOME
ggplot(train, aes(x = predict, y = INCOME)) +   geom_point() +   geom_abline()

#SVR Parameters

#Cost: The parameter C controls the trade off between errors of the SVM on training data and 
#margin maximization (C = ∞ leads to hard margin SVM). If it is too large, we have a high penalty for 
#nonseparable points and we may store many support vectors and overfit. 
#If it is too small, we may have underfitting.

#Epsilon
#The value of ε can affect the number of support vectors used to construct the regression function. 
#The bigger ε, the fewer support vectors are selected. On the other hand, bigger ε-values results 
#in more �flat� estimates(no relation). 

#Fine tuning
#Testing different parameters take so long. Maybe a batch job is best for this.
tuneResult <- tune(svm, INCOME ~ ., data = train, ranges = list(epsilon = seq(0,0.5,0.1), cost = 1:10))

#Best Model
best_model <- tuneResult$best.model
#Predicted value
train$predict_best <- predict(best_model, train)
#INCOME mean
INCOME_train_mean <- mean(train$INCOME)
#Total Sum of Squares
tss_train <- sum((train$INCOME - INCOME_train_mean)^2)
#Residuals
train$residuals <- train$INCOME - train$predict_best
#Residual Sum of Squares
rss_train <- sum(train$residuals^2)
#R-squared
rsq_train <- 1 - (rss_train/tss_train)
#RMSE Test
rmse_train <- sqrt(mean(train$residuals^2))
#SVM Plot
plot(tuneResult)
