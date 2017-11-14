#Package Loading
library(readr)

#Gathering Data
Risk <- read_delim("/home/seckindinc/Desktop/Projects/R/Data/Risk.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

for(i in names(subset(Risk[,sapply(Risk, is.numeric)], select=-ID))) {
  Risk <- cbind(Risk,normalize(Risk[,i]))
  names(Risk)[length(names(Risk))] <- paste("Normalized_",i,sep="")
}

dt = sort(sample(nrow(Risk), nrow(Risk)*.7))
train <- Risk[dt,]
test <- Risk[-dt,]

train_normalized <- train[,grep("Normal", names(train))]
test_normalized <- test[,grep("Normal", names(test))]

# Hierarchical clustering is an alternative approach which builds a hierarchy from the bottom-up, and doesn’t require us to specify the number of clusters beforehand.
#Agglomerative clustering: It’s also known as AGNES (Agglomerative Nesting). It works in a bottom-up manner. That is, each object is initially considered as a single-element cluster (leaf). At each step of the algorithm, the two clusters that are the most similar are combined into a new bigger cluster (nodes). This procedure is iterated until all points are member of just one single big cluster

# The algorithm works as follows:

# Put each data point in its own cluster.
# Identify the closest two clusters and combine them into one cluster.
# Repeat the above step till all the data points are in a single cluster.

# There are a few ways to determine how close two clusters are:

# Complete linkage clustering: Find the maximum possible distance between points belonging to two different clusters.
# Single linkage clustering: Find the minimum possible distance between points belonging to two different clusters.
# Mean linkage clustering: Find all possible pairwise distances for points belonging to two different clusters and then calculate the average.
# Centroid linkage clustering: Find the centroid of each cluster and calculate the distance between centroids of two clusters.
# Complete linkage and mean linkage clustering are the ones used most often.

#For using hclust we need to convert our data frame to dissimilarity structure
d_train_normalized <- dist(train_normalized)

hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid", "ward.D2")

#Iterating through similarity measures
for(i in seq_along(hclust_methods)) {
  print(hclust_methods[i])
  agglomerative <- hclust(d_train_normalized, method = hclust_methods[i])
  agglomerative_dendogram <- as.dendrogram(agglomerative)
  plot(agglomerative_dendogram)  
}

agglomerative <- hclust(d_train_normalized, method = "complete")
+hclust_methods[3]