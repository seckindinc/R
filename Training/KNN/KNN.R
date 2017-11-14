#Package Loading
library(readr)
library(class)
library(gmodels)
library(caret)

#Step 1: Data Understanding

#Gathering Data
Risk <- read_delim("/home/seckindinc/Desktop/Projects/R/Data/Risk.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#Structre of Data
str(Risk)

#Summary of Data
summary(Risk)

#Normalizing Data for KNN

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

for(i in names(subset(Risk[,sapply(Risk, is.numeric)], select=-ID))) {
  Risk <- cbind(Risk,normalize(Risk[,i]))
  names(Risk)[length(names(Risk))] <- paste("Normalized_",i,sep="")
}

Risk$RISK_Category <- ifelse(Risk$RISK == "good risk",1,0)
Risk$RISK_Category <- factor(Risk$RISK_Category,levels=c(1,0),labels=c("Good","Bad"))

#Train and Test Data
dt = sort(sample(nrow(Risk), nrow(Risk)*.7))
train <- Risk[dt,]
test <- Risk[-dt,]

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
#integer_fields = subset(Risk[,sapply(Risk, is.numeric)], select=-integer_fields[,ncol(integer_fields)])

#Step 3: Modelling
train_def <- train$RISK_Category
test_def <- test$RISK_Category

train_knn <- train[,grep("Normal", names(train))]
test_knn <- test[,grep("Normal", names(test))]

knn1 <-  knn(train_knn, test_knn, train_def, k=1)
knn3 <-  knn(train_knn, test_knn, train_def, k=3)
knn5 <-  knn(train_knn, test_knn, train_def, k=5)
knn10 <-  knn(train_knn, test_knn, train_def, k=10)

CrossTable(x = test_def, y = knn1, prop.chisq = FALSE)
CrossTable(x = test_def, y = knn3, prop.chisq = FALSE)
CrossTable(x = test_def, y = knn5, prop.chisq = FALSE)
CrossTable(x = test_def, y = knn10, prop.chisq = FALSE)

#Confusion Matrix Details
# accuracy = (true positive + true negatve) / all (100 times this is the same as percentCorrect)
# sensitivity = true pasitive rate = true positive / all positive (sensitivity is also called recall)
# specificity = true negative rate = true negative / all negative
# precision = positive predictive velue = true positive rate

confusionMatrix(test_def, knn1)
confusionMatrix(test_def, knn3)
confusionMatrix(test_def, knn5)
confusionMatrix(test_def, knn10)
