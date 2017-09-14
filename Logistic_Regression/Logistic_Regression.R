#Package Loading
library(readr)
library(ggplot2)
library(corrplot)
library(caret)
library(ROCR)
library(Metrics)

#Step 1: Data Understanding

#Gathering Data
Risk <- read_delim("/home/seckindinc/Desktop/Projects/R/Data/Risk.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#Structre of Data
str(Risk)

#Summary of Data
summary(Risk)

#Train and Test Data
dt = sort(sample(nrow(Risk), nrow(Risk)*.7))
train <- subset(Risk[dt,], select=-ID)
test <- subset(Risk[-dt,], select=-ID)

#Categorizing RISK column
train$RISK_Category <- ifelse(train$RISK == "good risk",1,0)
train$RISK_Category <- factor(train$RISK_Category,levels=c(1,0),labels=c("Good","Bad"))
test$RISK_Category <- ifelse(test$RISK == "good risk",1,0)
test$RISK_Category <- factor(test$RISK_Category,levels=c(1,0),labels=c("Good","Bad"))
train <- subset(train, select=-RISK)

#Selecting Integer Fields excep ID column
integer_fields = subset(Risk[,sapply(Risk, is.numeric)], select=-ID)

#Histogram Function
histogram_func <- function (table_name, column_name) {
  g <- ggplot(table_name, aes(as.numeric(unlist(table_name[, column_name])))) + scale_fill_brewer(palette = "Spectral")
  g <- g + geom_histogram( aes(fill=Risk$RISK), bins=5, col="black", size=.1) + labs(x = i, y = "Kayıt Sayısı")
  print(g)
}

#Boxplot Function
boxplot_func <- function (table_name, column_name) {
  g <- ggplot(table_name, aes(Risk$RISK,as.numeric(unlist(table_name[, column_name]))))
  g <- g + geom_boxplot(varwidth=T, fill="plum") + labs(x=Risk$RISK, y=column_name)
  print(g)
}

#Density Function
density_func <- function (table_name, column_name) {
  g <- ggplot(table_name, aes(as.numeric(unlist(table_name[, column_name]))))
  g <- g + geom_density(aes(fill=factor(Risk$RISK)), alpha = 0.8) + labs(x=column_name, fill= Risk$RISK)
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
    scatter_func(integer_fields,i,j)  
  }
}

#Correlation Function
correlation_func(integer_fields)
         
#Multiple Scatter Plot of Integer Fields
plot(integer_fields, pch=16, col="blue", main="Scatter Plot of Integers")

#Step 2: Data Preparation
outlier_detection_sd_func <- function (table_name, column_name) {
  abs(as.numeric(unlist(table_name[, column_name]))-mean(as.numeric(unlist(table_name[, column_name])),na.rm=TRUE)) > 2*sd(as.numeric(unlist(table_name[, column_name])),na.rm=TRUE)
}

#Removing the last column from data frame
#integer_fields = subset(Risk$RISK[,sapply(Risk$RISK, is.numeric)], select=-integer_fields[,ncol(integer_fields)])

#Assigning outlier detection for every column
for(i in names(integer_fields)) {
  integer_fields <- cbind(integer_fields,outlier_detection_sd_func(integer_fields,i))
  names(integer_fields)[length(names(integer_fields))] <- paste("Outlier_",i,sep="")
}

#Step 3: Modelling

#Multiple Linear Regression
multiple_logistic_model_full = glm(RISK_Category~. ,data=train, family=binomial(link = "logit"))

#Summary
summary(multiple_logistic_model_full)
 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.0437   0.1870   0.3118   0.4724   2.2694  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    -6.409e-01  7.354e-01  -0.871  0.38352    
# AGE            -4.288e-02  9.520e-03  -4.505 6.65e-06 ***
#   INCOME         -6.738e-05  9.021e-06  -7.469 8.06e-14 ***
#   GENDERm         6.214e-03  1.146e-01   0.054  0.95675    
# MARITALmarried  4.057e+00  3.870e-01  10.485  < 2e-16 ***
#   MARITALsingle   3.967e+00  4.577e-01   8.668  < 2e-16 ***
#   NUMKIDS         2.835e-01  9.138e-02   3.103  0.00192 ** 
#   NUMCARDS        2.415e-01  7.758e-02   3.114  0.00185 ** 
#   HOWPAIDweekly   7.034e-01  2.204e-01   3.192  0.00141 ** 
#   MORTGAGEy      -5.262e-01  1.700e-01  -3.095  0.00197 ** 
#   STORECAR        1.968e-01  7.098e-02   2.773  0.00556 ** 
#   LOANS           8.381e-01  1.251e-01   6.698 2.11e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2866.0  on 2880  degrees of freedom
# Residual deviance: 1993.6  on 2869  degrees of freedom
# AIC: 2017.6
# 
# Number of Fisher Scoring iterations: 6

#Removing non significant variables
multiple_logistic_model_reduced = glm(RISK_Category~AGE+INCOME+MARITAL+NUMKIDS+NUMCARDS+HOWPAID+MORTGAGE+STORECAR+LOANS ,data=train, family=binomial())

#Model Summary
summary(multiple_logistic_model_reduced)

#compare two models
#With p > 0.05, this ANOVA test also corroborates the fact that the second model is better than first model.
anova(multiple_logistic_model_full,multiple_logistic_model_reduced,test = "Chisq")

# Performance of Logistic Regression Model
#1. AIC (Akaike Information Criteria) – The analogous metric of adjusted R² in logistic regression 
#is AIC. AIC is the measure of fit which penalizes model for the number of model coefficients. 
#Therefore, we always prefer model with minimum AIC value.
#2. Null Deviance and Residual Deviance – Null Deviance indicates the response predicted by a 
#model with nothing but an intercept. Lower the value, better the model. 
#Residual deviance indicates the response predicted by a model on adding independent variables. 
#Lower the value, better the model.

#3. Confusion Matrix: It is nothing but a tabular representation of Actual vs Predicted values. 
#This helps us to find the accuracy of the model and avoid overfitting. This is how it looks like:

#Response probabilty
train$predict <- multiple_logistic_model_reduced$fitted.values
#Response category
train$RISK_Category_predict <- ifelse(train$predict > 0.50, "Good","Bad")
#Confusion Matrix
confusionMatrix(train[["RISK_Category_predict"]], train[["RISK_Category"]])

#4. ROC Curve: Receiver Operating Characteristic(ROC) summarizes the model’s performance by evaluating
#the trade offs between true positive rate (sensitivity) and false positive rate(1- specificity). 
# For plotting ROC, it is advisable to assume p > 0.5 since we are more concerned about success rate. 
# ROC summarizes the predictive power for all possible values of p > 0.5.  
# The area under curve (AUC), referred to as index of accuracy(A) or concordance index, 
# is a perfect performance metric for ROC curve. 
# Higher the area under curve, better the prediction power of the model. Below is a sample ROC curve. 
# The ROC of a perfect predictive model has TP equals 1 and FP equals 0. 
# This curve will touch the top left corner of the graph.

ROCRpred <- prediction(train$predict, train$RISK_Category)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf) > auc(train$RISK_Category,train$predict)

#Test Data Prediction
test$predict <- predict(multiple_logistic_model_reduced,newdata = test,type = "response")
test$RISK_Category_predict <- ifelse(test$predict > 0.5, "Good","Bad")

#Confusion Matrix Details
# accuracy = (true positive + true negatve) / all (100 times this is the same as percentCorrect)
# sensitivity = true pasitive rate = true positive / all positive (sensitivity is also called recall)
# specificity = true negative rate = true negative / all negative
# precision = positive predictive velue = true positive rate

confusionMatrix(test[["RISK_Category_predict"]], test[["RISK_Category"]])