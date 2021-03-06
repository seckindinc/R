#Package Loading
library(readr)
library(ggplot2)
library(corrplot)
library(MASS)
library(lmtest)
library(relaimpo)
library(bootstrap)
library(DAAG)
library(caret)
library(boot)

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
dt = sample(nrow(integer_fields), nrow(integer_fields)*.7)
train <- integer_fields[dt,]
test <- integer_fields[-dt,]

#Target Field
Target_Field <- Risk$RISK

#Histogram Function
histogram_func <- function (table_name, column_name) {
  g <- ggplot(table_name, aes(as.numeric(unlist(table_name[, column_name])))) + scale_fill_brewer(palette = "Spectral")
  g <- g + geom_histogram( aes(fill=Target_Field), bins=5, col="black", size=.1) + labs(x = i, y = "Row Number")
  print(g)
}

#Boxplot Function
boxplot_func <- function (table_name, column_name) {
  g <- ggplot(table_name, aes(Target_Field,as.numeric(unlist(table_name[, column_name]))))
  g <- g + geom_boxplot(varwidth=T, fill="plum") + labs(x=Target_Field, y=column_name)
  print(g)
}

#Density Function
density_func <- function (table_name, column_name) {
  g <- ggplot(table_name, aes(as.numeric(unlist(table_name[, column_name]))))
  g <- g + geom_density(aes(fill=factor(Target_Field)), alpha = 0.8) + labs(x=column_name, fill= Target_Field)
  print(g)
}

#Scatter Function
scatter_func <- function (table_name, column_name_1,column_name_2) {
  g <- ggplot(table_name, aes(as.numeric(unlist(table_name[, column_name_1])),as.numeric(unlist(table_name[, column_name_2]))))
  g <- g + geom_count(col = "tomato3", show.legend = F) + labs(x=i, y=j)
  print(g)
}

#Correlation Function
correlation_func <- function (table_name) {
  corr_matrix <- round(cor(table_name),1)  
  g <- corrplot(corr_matrix, method="number")
  print(g)
}

#Histogram Function Iterating Through Data Frame
for(i in names(integer_fields)) {
  histogram_func(integer_fields,i)
}

#Density Function Iterating Through Data Frame
for(i in names(integer_fields)) {
  density_func(integer_fields,i)
}

#Boxplot Function Iterating Through Data Frame
for(i in names(integer_fields)) {
  boxplot_func(integer_fields,i)
}

#Shapiro-Wilks Normallity Test
for(i in names(integer_fields)) {
  cat (i,"Column Shapiro Wilk Normallity Test")
  print (shapiro.test(as.numeric(unlist(integer_fields[,i]))))
}

#Scatter Function Iterating Through Data Frame
for(i in names(integer_fields)) {
  for(j in names(integer_fields)) {
    if (i != j) {
    scatter_func(integer_fields,i,j)  
    }
  }
}

#Correlation Function
correlation_func(integer_fields)
         
#Multiple Scatter Plot of Integer Fields
plot(integer_fields, pch=16, col="blue", main="Scatter Plot of Integers")

#Step 2: Data Preparation
#Outlier Detection Function
outlier_detection_sd_func <- function (table_name, column_name) {
  abs(as.numeric(unlist(table_name[, column_name]))-mean(as.numeric(unlist(table_name[, column_name])),na.rm=TRUE)) > 2*sd(as.numeric(unlist(table_name[, column_name])),na.rm=TRUE)
}

#Mahalanobis Distance
mean <- colMeans(integer_fields)
Sx <- cov(integer_fields)
integer_fields$Mahalanobis_Distance <- mahalanobis(integer_fields, mean, Sx) > 20

#Removing the last column from data frame
#integer_fields = subset(Risk[,sapply(Risk, is.numeric)], select=-integer_fields[,ncol(integer_fields)])

#Assigning outlier detection for every column
for(i in names(subset(integer_fields,select=-Mahalanobis_Distance))) {
  integer_fields <- cbind(integer_fields,outlier_detection_sd_func(integer_fields,i))
  names(integer_fields)[length(names(integer_fields))] <- paste("Outlier_",i,sep="")
}

#Step 3: Modelling

#Multiple Linear Regression
multiple_linear_model = lm(INCOME~. ,data=train)

#General Model Statistics
summary(multiple_linear_model)
#At variable selection phase we need to investigate every single variable to enter the model or not.
# H0 : β1 = 0
# Ha : β1 <> 0
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 19626.60     423.93  46.296  < 2e-16 ***
#   AGE           503.41      14.09  35.718  < 2e-16 ***
#   NUMKIDS     -1089.20     146.88  -7.416 1.46e-13 ***
#   NUMCARDS     -151.78      97.88  -1.551    0.121    
# STORECAR    -1547.29     118.49 -13.058  < 2e-16 ***
#   LOANS       -3068.11     194.85 -15.746  < 2e-16 ***
#By the t statistics we accept H0 for NUMCARDS and reject H0 for other variables.

#By this we come to the relation between the predictors and response variable.
#H0 : β 1 = β 2 = · · · = β p = 0
#Ha : at least one β j is non-zero.
#By the f statistics we reject H0

#Removing NUMCARDS from model
multiple_linear_model = lm(INCOME~AGE+NUMKIDS+STORECAR+LOANS ,data=train)

#10-Fold Cross Validated Regression
multiple_linear_cv_model <- train(INCOME~AGE+NUMKIDS+STORECAR+LOANS ,data=train, method = "lm", 
                             trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE))

#Boostraped Linear Regression
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 

# bootstrapping with 1000 replications 
results <- boot(data=train, statistic=rsq, R=1000, formula=INCOME~AGE+NUMKIDS+STORECAR+LOANS)

# view results
results 
plot(results)

# get 95% confidence interval 
boot.ci(results)

#Model Comparison multiple_linear_model vs multiple_linear_cv_model
multiple_linear_cv_model
summary(multiple_linear_model)

#Stepwise regression
stepwise_model <- stepAIC(multiple_linear_model, direction = "both")

#Model Summary
summary(stepwise_model)

#1)The regression model is linear in parameters

#2) The mean of residuals is zero
#If it zero (or very close), then this assumption is held true for that model. 
mean(stepwise_model$residuals)

#3) Homoscedasticity of residuals or equal variance
#Residuals vs Fitted and Scale-Location graphs are used for this assumption.
#Residuals vs Fitted: As the fitted values along x increase, the residuals decrease and then increase. 
#This pattern is indicated by the red line, which should be approximately flat if the disturbances are homoscedastic. 
#Scale-Location: Is more convenient as the disturbance term in Y axis is standardized.
layout(matrix(c(1,2,3,4),2,2))
plot(stepwise_model)

#4) No autocorrelation of residuals
#This is applicable especially for time series data. Autocorrelation is the correlation of a time Series with lags of itself. 
#When the residuals are autocorrelated, it means that the current value is dependent of the previous (historic) values 
#and that there is a definite unexplained pattern in the Y variable that shows up in the disturbances.

# Durbin-Watson test
#H0 : Autocorrelation is 0
#Ha : Autocorrelation is greater than 0
dwtest(stepwise_model)

#5) The X variables and residuals are uncorrelated
for(i in c("AGE","NUMKIDS","STORECAR","LOANS")) {
  cat ("Correlation Between ",i," and Residuals Test")
  print (cor.test(as.numeric(unlist(train[,i])),stepwise_model$residuals))
}
# Correlation Between  AGE  and Residuals Test
# Pearson's product-moment correlation
# 
# data:  as.numeric(unlist(predictors[, i])) and multiple_linear_model$residuals
# t = 2.3845e-14, df = 4115, p-value = 1
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.03054786  0.03054786
# sample estimates:
# cor 
# 3.71716e-16 

#6) The number of observations must be greater than number of Xs

#7) The variability in X values is positive
for(i in c("AGE","NUMKIDS","STORECAR","LOANS")) {
  cat ("Variance of ",i)
  print (var(as.numeric(unlist(train[,i]))))
}
# Variance of  AGE[1] 97.54978
# Variance of  NUMKIDS[1] 1.370254
# Variance of  NUMCARDS[1] 3.537112
# Variance of  STORECAR[1] 1.829982
# Variance of  LOANS[1] 0.7025512

#8) No perfect multicollinearity
#This can be handled by Variance Inflaction Factor
#VIF is a metric computed for every X variable that goes into a linear model. 
#If the VIF of a variable is high, it means the information in that variable is already explained by other X variables present in the given model, 
#which means, more redundant is that variable. So, lower the VIF (<2) the better.
sqrt(vif(stepwise_model))
# AGE  NUMKIDS NUMCARDS STORECAR    LOANS 
# 1.708857 2.606943 2.988686 2.265843 2.352308

#9) Normality of residuals
print(shapiro.test(stepwise_model$residuals))

#Hat Leverage Point
train <- cbind(train,hat(model.matrix(stepwise_model)) > 0.2)
names(train)[length(names(train))] <- paste("hat_leverage",sep="") 

#Cook's Distance
train <- cbind(train,cooks.distance(stepwise_model) > 1)
names(train)[length(names(train))] <- paste("cook_distance",sep="") 

#Variable Importance
calc.relimp(stepwise_model,rela=TRUE)

# K - Fold Cross Validation
# In cross-validation, a portion of the data is selected as the training sample and a
# portion is selected as the hold-out sample. A regression equation is developed on
# the training sample, and then applied to the hold-out sample. Because the hold-out
# sample wasn’t involved in the selection of the model parameters, the performance on
# this sample is a more accurate estimate of the operating characteristics of the model
# with new data.

#4)Model Evaluation

#RMSE train
train$prediction <- stepwise_model$fitted.values
train$residuals <- stepwise_model$residuals
rmse_train <- sqrt(mean(train$residuals^2))

#INCOME mean
INCOME_train_mean <- mean(train$INCOME)
#Total Sum of Squares
tss_train <- sum((train$INCOME - INCOME_train_mean)^2)
#Residual Sum of Squares
rss_train <- sum(stepwise_model$residuals^2)
#R-squared
rsq_train <- 1 - (rss_train/tss_train)

#Correlation between actual value and prediction
print (cor.test(train$INCOME,train$prediction))

#Train vs Test

#Test Prediction
test$prediction <- predict(stepwise_model, newdata = test)
#INCOME mean
INCOME_test_mean <- mean(test$INCOME)
#Total Sum of Squares
tss_test <- sum((test$INCOME - INCOME_test_mean)^2)
#Residuals
test$residuals <- test$INCOME - test$prediction
#Residual Sum of Squares
rss_test <- sum(test$residuals^2)
#R-squared
rsq_test <- 1 - (rss_test/tss_test)
#RMSE Test
rmse_test <- sqrt(mean(test$residuals^2))

# Evaluate the rmse on both training and test data and print them
rmse_train
rmse_test

# Evaluate the r-squared on both training and test data.and print them
rsq_train
rsq_test

# Plot the predictions (on the x-axis) against the outcome (cty) on the test data
ggplot(train, aes(x = prediction, y = INCOME)) +   geom_point() +   geom_abline()
ggplot(test, aes(x = prediction, y = INCOME)) +   geom_point() +   geom_abline()

shrinkage <- function(fit, k=10){
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef}
  x <- fit$model[,2:ncol(fit$model)]
  y <- fit$model[,1]
  results <- crossval(x, y, theta.fit, theta.predict, ngroup=k)
  r2 <- cor(y, fit$fitted.values)^2
  r2cv <- cor(y, results$cv.fit)^2
  cat("Original R-square =", r2, "\n")
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n")
  cat("Change =", r2-r2cv, "\n")
}

shrinkage(stepwise_model)

#5) Deployment
stepwise_model$coefficients
