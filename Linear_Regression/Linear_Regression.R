#Package Loading
library(readr)
library(ggplot2)
library(corrplot)
library(lmtest)
library(Hmisc)
library(car)

#Step 1: Data Understanding

#Gathering Data
Risk <- read_delim("/home/seckindinc/Desktop/Projects/R/Data/Risk.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#Structre of Data
str(Risk)

#Summary of Data
summary(Risk)

#Selecting Integer Fields excep ID column
integer_fields = subset(Risk[,sapply(Risk, is.numeric)], select=-ID)

#Prediction Class
Classification_Field = Risk$RISK

#Histogram Function
histogram_func <- function (table_name, column_name) {
  g <- ggplot(table_name, aes(as.numeric(unlist(table_name[, column_name])))) + scale_fill_brewer(palette = "Spectral")
  g <- g + geom_histogram( aes(fill=Classification_Field), bins=5, col="black", size=.1) + labs(x = i, y = "Kayıt Sayısı")
  print(g)
}

#Boxplot Function
boxplot_func <- function (table_name, column_name) {
  g <- ggplot(table_name, aes(Classification_Field,as.numeric(unlist(table_name[, column_name]))))
  g <- g + geom_boxplot(varwidth=T, fill="plum") + labs(x=Classification_Field, y=column_name)
  print(g)
}

#Density Function
density_func <- function (table_name, column_name) {
  g <- ggplot(table_name, aes(as.numeric(unlist(table_name[, column_name]))))
  g <- g + geom_density(aes(fill=factor(Classification_Field)), alpha = 0.8) + labs(x=column_name, fill= Classification_Field)
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
#integer_fields = subset(Risk[,sapply(Risk, is.numeric)], select=-integer_fields[,ncol(integer_fields)])

#Assigning outlier detection for every column
for(i in names(integer_fields)) {
  integer_fields <- cbind(integer_fields,outlier_detection_sd_func(integer_fields,i))
  names(integer_fields)[length(names(integer_fields))] <- paste("Outlier_",i,sep="")
}

#Selecting columns to be used in modelling 
modelling_fields <- integer_fields[, -grep("Outlier", names(integer_fields))]
predictors = subset(modelling_fields[,], select=-INCOME)

#Multiple Linear Regression
multiple_linear_model = lm(INCOME~. ,data=modelling_fields)

#General Model Statistics
summary(multiple_linear_model)
#At variable selection phase we need to investigate every single variables to enter the model or not.
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

#Diagnotics Plots
#Residuals vs Fitted: 
layout(matrix(c(1,2,3,4),2,2))
plot(multiple_linear_model)

#1)The regression model is linear in parameters

#2) The mean of residuals is zero
#If it zero (or very close), then this assumption is held true for that model. 
mean(multiple_linear_model$residuals)

#3) Homoscedasticity of residuals or equal variance
#Residuals vs Fitted and Scale-Location graphs are used for this assumption.
#Residuals vs Fitted: As the fitted values along x increase, the residuals decrease and then increase. 
#This pattern is indicated by the red line, which should be approximately flat if the disturbances are homoscedastic. 
#Scale-Location: Is more convenient as the disturbance term in Y axis is standardized.

#4) No autocorrelation of residuals
#This is applicable especially for time series data. Autocorrelation is the correlation of a time Series with lags of itself. 
#When the residuals are autocorrelated, it means that the current value is dependent of the previous (historic) values 
#and that there is a definite unexplained pattern in the Y variable that shows up in the disturbances.

#H0 : Autocorrelation is 0
#Ha : Autocorrelation is greater than 0
dwtest(multiple_linear_model)
# Durbin-Watson test
# data:  multiple_linear_model
# DW = 0.64967, p-value < 2.2e-16
# alternative hypothesis: true autocorrelation is greater than 0

#5) The X variables and residuals are uncorrelated
for(i in names(predictors)) {
  cat ("Correlation Between ",i," and Residuals Test")
  print (cor.test(as.numeric(unlist(predictors[,i])),multiple_linear_model$residuals))
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
# 
# Correlation Between  NUMKIDS  and Residuals Test
# Pearson's product-moment correlation
# 
# data:  as.numeric(unlist(predictors[, i])) and multiple_linear_model$residuals
# t = -2.1697e-13, df = 4115, p-value = 1
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.03054786  0.03054786
# sample estimates:
#   cor 
# -3.382378e-15 
# 
# Correlation Between  NUMCARDS  and Residuals Test
# Pearson's product-moment correlation
# 
# data:  as.numeric(unlist(predictors[, i])) and multiple_linear_model$residuals
# t = -1.928e-13, df = 4115, p-value = 1
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.03054786  0.03054786
# sample estimates:
# cor 
# -3.005495e-15 
# 
# Correlation Between  STORECAR  and Residuals Test
# Pearson's product-moment correlation
# 
# data:  as.numeric(unlist(predictors[, i])) and multiple_linear_model$residuals
# t = -2.2243e-13, df = 4115, p-value = 1
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.03054786  0.03054786
# sample estimates:
#           cor 
# -3.467364e-15 
# 
# Correlation Between  LOANS  and Residuals Test
# 	Pearson's product-moment correlation
# 	
# 	data:  as.numeric(unlist(predictors[, i])) and multiple_linear_model$residuals
# 	t = -3.9531e-13, df = 4115, p-value = 1
# 	alternative hypothesis: true correlation is not equal to 0
# 	95 percent confidence interval:
# 	-0.03054786  0.03054786
# 	sample estimates:
# 	cor 
# 	-6.162435e-15 

#6) The number of observations must be greater than number of Xs

#7) The variability in X values is positive
for(i in names(predictors)) {
  cat ("Variance of ",i)
  print (var(as.numeric(unlist(predictors[,i]))))
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
vif(multiple_linear_model)
# AGE  NUMKIDS NUMCARDS STORECAR    LOANS 
# 1.708857 2.606943 2.988686 2.265843 2.352308

#9) Normality of residuals
print (shapiro.test(multiple_linear_model$residuals))

#Diagnotics
head(influence.measures(multiple_linear_model))

#Ilgili package lar indirilince bakilacak. statsmethod sayfasından

#K-Fold Cross Validation
library(DAAG)
cv.lm(df=Risk, fit, m=10)
cross-validation
# Sum the MSE for each fold, divide by the number of observations and take the 
# square root to get the cross validated standart errors of estimate.
# To access R2 shrinkage via K-Fold cross validation crossval() function can be
# used from bootstrap package



