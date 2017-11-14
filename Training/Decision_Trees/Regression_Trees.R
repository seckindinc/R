#Package Loading
library(readr)
library(rpart)
library(party)
library(randomForest)
library(ipred)

#Gathering Data
Risk <- read_delim("/home/seckindinc/Desktop/Projects/R/Data/Risk.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

modelling_data <- subset(Risk, select=-ID)
modelling_data <- subset(modelling_data, select=-RISK)

#Factorizing Function
modelling_data[,names(subset(modelling_data[,sapply(modelling_data, is.character)]))] <- lapply(modelling_data[,names(subset(modelling_data[,sapply(modelling_data, is.character)]))],as.factor)

#1) Classification and Regression Trees (CART) split attributes based on values that minimize a loss function, such as sum of squared errors.
#rpart and related algorithms usually employ information measures (such as the Gini coefficient) for selecting the current covariate.
#CART is using information gain for deciding between alternative splits. The concept comes from information theory and is related to entropy (the amount of disorder in a system).
#If the dataset contains only observations that all have the same value for the target variable then there is no disorder (entropy is 0).
#If the values of the target variable are equally distributed across the observations then the dataset contains the maximum amount of disorder (entropy is 1).
#Datasets containing different mixtures of the values of the target variable will have a measure of entropy between 0 and 1.
#Information Gain is the expected reduction in entropy caused by knowing the value a attribute.

#Train and Test Data
dt = sample(nrow(modelling_data), nrow(modelling_data)*.7)
train <- modelling_data[dt,]
test <- modelling_data[-dt,]

#Model
tree_CART_regression <- rpart(LOANS~., data=train, method = "anova", xval = 1000)

#Results
printcp(tree_CART_regression) 

#cross-validation results 
plotcp(tree_CART_regression)

#Detailed Summary
summary(tree_CART_regression)

# create additional plots 
# two plots on one page 
par(mfrow=c(1,2))
# visualize cross-validation results  	
rsq.rpart(tree_CART_regression)

# plot tree 
plot(tree_CART_regression, uniform=TRUE, main="Regression Tree for Loan")
text(tree_CART_regression, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(tree_CART_regression, file = "/home/seckindinc/Desktop/Projects/R/Decision_Trees/tree2.ps", title = "Regression Tree for Loans ")

#2) Conditional Decision Trees
#Condition Decision Trees are created using statistical tests to select split points on attributes rather than a loss function.
#ctree(party) uses a significance test procedure in order to select variables instead of selecting the variable that maximizes an information measure (e.g. Gini coefficient).
#Conditional inference trees address the overfitting and variable selection biases of rpart by making use of statistical p values
#Tree growth is based on statistical stopping rules, so pruning should not be required.
#Test statistics is chi-square if both variables are categorical. If one variable is categorical and other is numeric then test statistics is ANOVA.
#The Chi-Square test of Independence is used to determine if there is a significant relationship between two nominal (categorical) variables. 
#The frequency of one nominal variable is compared with different values of the second nominal variable.  The data can be displayed in an R*C contingency table, where R is the row and C is the column.
#ANOVA is a statistical technique that assesses potential differences in a scale-level dependent variable by a nominal-level variable having 2 or more categories. 

#Modelling
tree_Conditional_Tree_class <- ctree(LOANS~., data=train)

#Plotting
plot(tree_Conditional_Tree_class)

#Printing
print(tree_Conditional_Tree_class)

#Summary
tree_Conditional_Tree_class

# 3) Random Forests
# In a random forest each decision tree is built to its maximal depth. The individual decision trees are not pruned.
# Each individual tree will overfit the data, but this is outweighed by the multiple trees using different variables and (over) fitting the data differently.
# The randomness used by a random forest algorithm is in the selection of both observations and variables.
# In building a single decision tree in the forest the algorithm considers a random subset of the observations from the training dataset. Also, at each node in the process of building
# the decision tree, only a small fraction of all of the available variables are considered when determining how to best partition the dataset.
#Random forests improve predictive accuracy by generating a large number of bootstrapped trees (based on random samples of variables), 
#classifying a case using each tree in this new "forest", and deciding a final predicted outcome by combining the results across all of the trees 
#(an average in regression, a majority vote in classification).
# A problem with decision trees like CART is that they are greedy. They choose which variable to split on using a greedy algorithm that minimizes error. 
# As such, even with Bagging, the decision trees can have a lot of structural similarities and in turn have high correlation in their predictions.
#In CART, when selecting a split point, the learning algorithm is allowed to look through all variables and all variable values in order to select the most 
#optimal split-point. The random forest algorithm changes this procedure so that the learning algorithm is limited to a random sample of features of which to search.

#Model
tree_Random_Forest_class <- randomForest(LOANS~., data=train)

#Summary
print(tree_Random_Forest_class)

#Plot
plot(tree_Random_Forest_class)

# 4) Bootstrap Aggregation (Bagging) CART
#Bagging is the application of the Bootstrap procedure to a high-variance machine learning algorithm, typically decision trees.
# When bagging with decision trees, we are less concerned about individual trees overfitting the training data. 
# For this reason and for efficiency, the individual decision trees are grown deep (e.g. few training samples at each leaf-node of the tree) and the trees are not pruned. 
# These trees will have both high variance and low bias. These are important characterize of sub-models when combining predictions using bagging.
#The only parameters when bagging decision trees is the number of samples and hence the number of trees to include. 
# This can be chosen by increasing the number of trees on run after run until the accuracy begins to stop showing improvement 
# (e.g. on a cross validation test harness). Very large numbers of models may take a long time to prepare, but will not overfit the training data.

#Model
tree_Bagging_class <- bagging(LOANS~., data=train, nbagg = 1000, coob = TRUE)

#Summary
summary(tree_Bagging_class)
