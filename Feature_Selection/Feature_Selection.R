#Package Loading
library(readr)
library(leaps)
library(glmnet)
library(pls)

#Gathering Data
Risk <- read_delim("/home/seckindinc/Desktop/Projects/R/Data/Risk.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

integer_fields = subset(Risk[,sapply(Risk, is.numeric)], select=-ID)

# 1)Subset Selection Methods

# 1-1) Mallow's Cp
#Cp= (1/n) * (RSS+2*d*σ**2)

# 1-2) Akaike information criterion
#AIC = (1/(n*σ**2)) (RSS+2*d*σ**2)

# 1-3) Bayesian information criterion
#BIC = (1/n) * (RSS+log(n)*d*σ**2)

# 1-4) Adjusted R**2
#Adjusted R**2 = 1 - ((RSS/ (n-d-1)) / (TSS / (n - 1)))

#Unlike Cp , AIC, and BIC, for which a small value indicates a model with a low test error, a large value of adjusted R**2 indicates a model with a small test error.

model <- regsubsets(INCOME~., integer_fields)

#In the rows the predictor variable entered model number and signifiance of that variable.
summary(model)

#R**2 increase as model adds new variables
summary(model)$rsq

#All informations
#RSS decrease chart
plot(summary(model)$rss, xlab ="Number of Variables", ylab ="RSS", type ="l")

#Adjusted R**2 increate chart
plot(summary(model)$adjr2, xlab ="Number of Variables", ylab ="R**2", type ="l")
#Each row in this graph represents a model; the shaded rectangles in the columns indicate the variables included in the given model. 
#Adjusted R**2 increase based on variables
plot(model,scale="r2")

#Mallow's Cp decrease chart
plot(summary(model)$cp, xlab ="Number of Variables", ylab ="Cp", type ="l")
#Adjusted R**2 increase based on variables
plot(model,scale="Cp")

#BIC decrease chart
plot(summary(model)$bic, xlab ="Number of Variables", ylab ="Bic", type ="l")
#Adjusted R**2 increase based on variables
plot(model,scale="bic")

#Forward Stepwise Selection
model2 <- regsubsets(INCOME~., integer_fields, method= "forward")

#Summary
summary(model2)

#Backward Stepwise Selection
model3 <- regsubsets(INCOME~., integer_fields, method= "backward")

#Summary
summary(model3)

# 2) Shrinkage Methods
#Shrinking the coefficient estimates can significantly reduce their variance.

# 2-1) Ridge Regression
# A) Ridge Regression is a remedial measure taken to alleviate multicollinearity amongst regression predictor variables in a model.
# B) Ridge regression is one of those methods that addresses the issue of multicollinearity by shrinking the coefficient estimates of the highly correlated variables.
#Unlike least squares method, ridge regression produces a set of coefficient estimates for different values of the tuning parameter. 
#So, it's advisable to use the results of ridge regession (the set of coefficient estimates) with a model selection 
#technique (such as, cross-validation) to determine the most appropriate model for the given data.
# C) Ridge regression is like least squares but shrinks the estimated coefficients towards zero.
# D) Tuning Parameter: When λ = 0, the penalty term has no effect, and ridge regression will produce the least squares estimates. However, as λ → ∞, the impact of
#the shrinkage penalty grows, and the ridge regression coefficient estimates will approach zero, null model. Selecting a good value for λ is critical.
# E) Ridge Regression will include all p predictors in the final model. Increasing the value of λ will tend to reduce the magnitudes of the coefficients,
#but will not result in exclusion of any of the variables.

#Ridge regression requires x and y vectors
x=model.matrix(INCOME~.-1,data=integer_fields) 
y=integer_fields$INCOME

ridge_model=glmnet(x,y,alpha=0)
#When log of lambda is 14, all the coefficients are essentially zero. Then as we relax lambda, the coefficients grow away from zero in a nice smooth way, 
#and the sum of squares of the coefficients is getting bigger and bigger until we reach a point where lambda is effectively zero.
plot(ridge_model,xvar="lambda",label=TRUE)

#Cross Validation
ridge_cv=cv.glmnet(x,y,alpha=0)
#In the beginning, the mean squared error is very high, and the coefficients are restricted to be too small, and then at some point, it kind of levels off. 
#This seems to indicate that the full model is doing a good job.
#There's two vertical lines.
#The one is at the minimum, 
#and the other vertical line is within one standard error of the minimum. 
#The second line is a slightly more restricted model that does almost as well as the minimum, and sometimes we'll go for that.
plot(ridge_cv)

# 2-2) Least Absolute Shrinkage and Selection Operator(Lasso)
# As with ridge regression, the lasso shrinks the coefficient estimates towards zero. However, unlike ridge regression, some of the coefficients are
#shrunken all the way to zero; such solutions, with multiple values that are identically zero, are said to be sparseThe lasso performs variable selection.

lasso_model=glmnet(x,y,alpha=1)
#A lot of the r squared was explained for quite heavily shrunk coefficients. And towards the end, with a relatively small increase in r squared from 
#between 0.3 and 0.4, coefficients grow very large. This may be an indication that the end of the path is overfitting.
plot(lasso_model,xvar="lambda",label=TRUE)
plot(lasso_model,xvar="dev",label=TRUE)

#The output below has 4 non-zero coefficients which shows that the function has chosen the second vertical second line on the cross-validation plot 
#(within one standard error of the minimum) because cross validation error is measured with some variance.
coef(lasso_cv)

#Cross Validation
lasso_cv=cv.glmnet(x,y)
plot(lasso_cv)

# 3) Dimension Reduction Methods

# 3-1) Principal Components Analysis
#PCA can be used to produce linear combinations of the covariates that are uncorrelated between each other.
#PCA projects {p}-dimensional data into a {q}-dimensional sub-space {(q <= p)} in a way that minimizes the residual sum of squares (RSS) of the projection.
#When performing Principal Components Regression, we generally recommend standardizing each predictor, using (6.6), prior to generating the principal components.
#Plotting (1-R^2) versus the number of components can be useful to visualize the number of principal components that retain most of the variability contained in the original data.

#Normalizing Numeric Fields
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

for(i in names(integer_fields)) {
  integer_fields <- cbind(integer_fields,normalize(integer_fields[,i]))
  names(integer_fields)[length(names(integer_fields))] <- paste("Normalized_",i,sep="")
}

#Selecting Normalized Fields
integer_fields_normalized <- integer_fields[,grep("Normal", names(integer_fields))]

#Principal Component Analysis
pca <- prcomp(integer_fields_normalized, center = TRUE, scale. = TRUE)

#Rotation shows loadings
pca

#Deciding how many principal components to be used
plot(pca, type = "l")

#summary
#The second row shows the proportion of the variance in the data explained by each component. 
#The third row describe the cumulative proportion of explained variance.
summary(pca)

#Adding Principal Components to Data Frame 
integer_fields$PC1 <- pca$x[,"PC1"]
integer_fields$PC2 <- pca$x[,"PC2"]
