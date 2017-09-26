#Package Loading
library(readr)
library(MASS)
library(plyr)

#Gathering Data
Risk <- read_delim("/home/seckindinc/Desktop/Projects/R/Data/Risk.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#Structre of Data
str(Risk)

#Summary of Data
summary(Risk)

#Linear Discriminant Analysis
#Find a linear combination of the predictors that gives maximum separation between the centers of the data while at the same time minimizing the variation within each group of data.

dt = sort(sample(nrow(Risk), nrow(Risk)*.7))
train <- subset(Risk[dt,], select=-ID)
test <- subset(Risk[-dt,], select=-ID)

train$RISK_Category <- ifelse(train$RISK == "good risk",1,0)
train$RISK_Category <- factor(train$RISK_Category,levels=c(1,0),labels=c("Good","Bad"))
test$RISK_Category <- ifelse(test$RISK == "good risk",1,0)
test$RISK_Category <- factor(test$RISK_Category,levels=c(1,0),labels=c("Good","Bad"))
train <- subset(train, select=-RISK)

# Leave-one-out cross validation. LOOCV uses a single observation from the original sample as the validation data, and the remaining observations as the training data.
# This is repeated such that each observation in the sample is used once as the validation data. 
# This is the same as a K-fold cross-validation with K being equal to the number of observations in the original sample. 
# Leave-one-out cross-validation is usually very expensive from a computational point of view because of the large number of times the training process is repeated.

#Discriminant Model
discriminant_model <- lda(RISK_Category ~., train, CV = TRUE)

#Prior Probabilities
discriminant_model$prior

#Means
discriminant_model$means

#Linear Combination Coefficients
discriminant_model$scaling

#Prediction Train
predictions <- predict(discriminant_model,train)

#Adding Model Columns to Train Data Frame
for(i in names(data.frame(predictions))) {
  train <- cbind(train,data.frame(predictions)[i])
  names(train)[length(names(train))] <- paste(i,sep="")
}

#Basic Confusion Matrix
table(train$RISK_Category,train$class)

#Classification Rate
mean(train$class==train$RISK_Category)

#Testing Model
discriminant_model_test <- predict(object = discriminant_model, test)

#Model Predicted Class
test$class <- discriminant_model_test$class

#Basic Confusion Matrix
table(test$RISK_Category,test$class)

#Classification Rate
mean(test$class==test$RISK_Category)
