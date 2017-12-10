#Package Loading
library(readr)
library(NbClust)
library(cluster)
library(fpc)

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

train_knn <- train[,grep("Normal", names(train))]
test_knn <- test[,grep("Normal", names(test))]

#The basic idea behind partitioning methods is to define clusters such that the total intra-cluster variation [or total within-cluster sum of square (WSS)] is minimized. 
#The total WSS measures the compactness of the clustering and we want it to be as small as possible.

# 1) Elbow Method
#Method looks at the total WSS as a function of the number of clusters: One should choose a number of clusters so that adding another cluster doesn’t improve much better the total WSS.

# Method iteration:
# Compute clustering algorithm (e.g., k-means clustering) for different values of k. For instance, by varying k from 1 to 10 clusters.
# For each k, calculate the total within-cluster sum of square (wss).
# Plot the curve of wss according to the number of clusters k.
# The location of a bend (knee) in the plot is generally considered as an indicator of the appropriate number of clusters.

# 2) Average Silhouette Method
#It measures the quality of a clustering. That is, it determines how well each object lies within its cluster. A high average silhouette width indicates a good clustering.
#Average silhouette method computes the average silhouette of observations for different values of k. 
#The optimal number of clusters k is the one that maximize the average silhouette over a range of possible values for k (Kaufman and Rousseeuw 1990).

# Method iteration:
# Compute clustering algorithm (e.g., k-means clustering) for different values of k. For instance, by varying k from 1 to 10 clusters.
# For each k, calculate the average silhouette of observations (avg.sil).
# Plot the curve of avg.sil according to the number of clusters k.
# The location of the maximum is considered as the appropriate number of clusters.

# 3) Gap Statistic Method
# The gap statistic compares the total within intra-cluster variation for different values of k with their expected values under null reference distribution of the data. 
# The estimate of the optimal clusters will be value that maximize the gap statistic (i.e, that yields the largest gap statistic). 
# This means that the clustering structure is far away from the random uniform distribution of points.

# Method iteration:
# Cluster the observed data, varying the number of clusters from k = 1, …, kmax, and compute the corresponding total within intra-cluster variation Wk.
# Generate B reference data sets with a random uniform distribution. Cluster each of these reference data sets with varying number of clusters k = 1, …, kmax, and compute the corresponding total within intra-cluster variation Wkb.
# Compute the estimated gap statistic as the deviation of the observed Wk value from its expected value Wkb under the null hypothesis: Gap(k)=1B∑b=1Blog(W∗kb)−log(Wk)Gap(k)=1B∑b=1Blog(Wkb∗)−log(Wk). Compute also the standard deviation of the statistics.
# Choose the number of clusters as the smallest value of k such that the gap statistic is within one standard deviation of the gap at k+1: Gap(k)≥Gap(k + 1)−sk + 1.

set.seed(123)

# 1) Elbow Method
max_cluster <- 10
wss <- sapply(1:max_cluster, function(k){kmeans(train_knn, k, nstart=100, iter.max = 15 )$tot.withinss})
plot(1:max_cluster, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

# 2) Average Silhouette Method
max_cluster <- 10
sil <- rep(0, max_cluster)

# Compute the average silhouette width for 
for(i in 2:max_cluster){
  km.res <- kmeans(train_knn, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(train_knn))
  sil[i] <- mean(ss[, 3])
}

# Plot the  average silhouette width
plot(1:max_cluster, sil, type = "b", pch = 19, frame = FALSE, xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)

# 3) GAP Statistic Method
gap_stat <- clusGap(train_knn, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_stat, method = "firstmax")

plot(gap_stat, frame = FALSE, xlab = "Number of clusters k")
abline(v = 3, lty = 2)

#NbClust Package
nb <- NbClust(train_knn, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") 

#Print Result
nb

#Modelling
kmeans_3 <- kmeans(train_knn, 3, nstart=100, iter.max = 15)

clusplot(train_knn, kmeans_3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
plotcluster(train_knn, kmeans_3$cluster)

