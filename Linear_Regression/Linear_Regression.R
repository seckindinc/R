library(readr)
Risk <- read_delim("C:/Users/Seckin/Desktop/R/Risk.txt", 
                   "\t", escape_double = FALSE, trim_ws = TRUE)
#Simple Linear Regression
simple_linear_model= lm(INCOME~AGE,data=Risk)
#General Model Statistics
summary(simple_linear_model)
#To Gather All Model Statistics
names(simple_linear_model)
#Plot Between Dependent Variable And Independent Variable
plot(AGE~INCOME,data=Risk)
#Adding Linear Line to Graph
abline(simple_linear_model)

#Multiple Linear Regression
multiple_linear_model = lm(INCOME~AGE+NUMKIDS+NUMCARDS+STORECAR+LOANS ,data=Risk)
summary(multiple_linear_model)
#Model Coefficients
coefficients(multiple_linear_model)
#Confidience IntervaÅŸs
confint(multiple_linear_model, level = 0.95)
#Predicted Values
fitted(multiple_linear_model)
#Residuals
residuals(multiple_linear_model)
#Anova Table
anova(multiple_linear_model)
#Covariance MAtris
vcov(multiple_linear_model)
#Diagnotics
influence(multiple_linear_model)
#Diagnotics Plots
layout(matrix(c(1,2,3,4),2,2))
plot(multiple_linear_model)
#Variace Inflation Factor
library(car)
vif(multiple_linear_model)

#Ilgili package lar indirilince bakilacak. statsmethod sayfasÄ±ndan

#K-Fold Cross Validation
library(DAAG)
cv.lm(df=Risk, fit, m=10)
cross-validation
# Sum the MSE for each fold, divide by the number of observations and take the 
# square root to get the cross validated standart errors of estimate.
# To access R2 shrinkage via K-Fold cross validation crossval() function can be
# used from bootstrap package

#Stepwise Regression
library(MASS)
multiple_linear_model = lm(INCOME~AGE+NUMKIDS+NUMCARDS+STORECAR+LOANS ,data=Risk)
stepwise <- stepAIC(multiple_linear_model, direction = "both")
stepwise$anova
#To Achive Relative Importance for Each Predictor
library(relaimpo)
#statmethod dan alÄ±nacak

modele test ve train de eklenecek.
uç değer ve diğer varsayımlar kontrol edilecek