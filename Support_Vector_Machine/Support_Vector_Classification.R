#Package Loading
library(readr)
library(e1071)
library(ROCR)

#Support Vector Classification

#Step 1: Data Understanding

#Gathering Data
Risk <- read_delim("/home/seckindinc/Desktop/Projects/R/Data/Risk.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#Structre of Data
str(Risk)

#Summary of Data
summary(Risk)

#Selecting Integer Fields excep ID column
modelling_fields = subset(Risk, select=-ID)

#Train and Test Data
dt = sort(sample(nrow(modelling_fields), nrow(modelling_fields)*.7))
train <- modelling_fields[dt,]
test <- modelling_fields[-dt,]

#Categorizing
train$RISK_Category <- ifelse(train$RISK == "good risk",1,0)
train$RISK_Category <- factor(train$RISK_Category,levels=c(1,0),labels=c("Good","Bad"))
train <- subset(train, select=-RISK)

#Cost: The parameter C controls the trade off between errors of the SVM on training data and 
#margin maximization (C = ∞ leads to hard margin SVM). If it is too large, we have a high penalty for 
#nonseparable points and we may store many support vectors and overfit. 
#If it is too small, we may have underfitting.

#Modelling
model <- svm(RISK_Category ~ ., data = train, ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

#Summary
print(model)

# Call:
#   svm(formula = RISK_Category ~ ., data = train, ranges = list(cost = 10^(-1:2), gamma = c(0.5, 1, 2)))
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.07142857 
# 
# Number of Support Vectors:  197

#Best Model
model <- svm(RISK_Category ~ ., data = train, cost = 1, gamma = 0.07)

summary(model)

#Predicted
train$predict <- model$fitted
train$predict_Category <- ifelse(train$predict == "Good",1,0)
train$predict_Category <- factor(train$predict_Category,levels=c(1,0),labels=c("Good","Bad"))

#Confusion Matrix
confusionMatrix(train[["predict_Category"]], train[["RISK_Category"]])

ROCRpred <- prediction(as.numeric(train$predict_Category), as.numeric(train$RISK_Category))
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf) > auc(train$RISK_Category,train$predict_Category)

#Test and Evaluation
test$RISK_Category <- ifelse(test$RISK == "good risk",1,0)
test$RISK_Category <- factor(test$RISK_Category,levels=c(1,0),labels=c("Good","Bad"))
test <- subset(test, select=-RISK)

test$predict <- predict(model, test)
test$predict_Category <- ifelse(test$predict == "Good",1,0)
test$predict_Category <- factor(test$predict_Category,levels=c(1,0),labels=c("Good","Bad"))

confusionMatrix(test[["predict_Category"]], test[["RISK_Category"]])

ROCRpred <- prediction(as.numeric(test$predict_Category), as.numeric(test$RISK_Category))
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf) > auc(test$RISK_Category,test$predict_Category)