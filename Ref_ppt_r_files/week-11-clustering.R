rm(list=ls())
library(tidyverse)
seed = read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/00236/seeds_dataset.txt', header=F)
seed = seed[,1:7]
colnames(seed) = c("area", "perimeter","campactness", "length", "width", "asymmetry", "groovelength")

seed <- scale(seed) # to standardise each column stdev so each column contributes on same scale

fit <- kmeans(seed, 5, nstart = 100) 
fit$cluster

library(fpc)
?cluster.stats
plotcluster(seed, fit$cluster)

wss <- (nrow(seed)-1)*sum(apply(seed,2,var))
for (i in 2:12) wss[i] <- sum(kmeans(seed, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

d<- dist(seed,method = "euclidean")
asw <- (nrow(seed)-1)*sum(apply(seed,2,var))
for (i in 1:12){
  cluster_ids<- kmeans(seed,centers=i)$cluster
  asw[i]<- cluster.stats(d=d, clustering= cluster_ids)$dunn
}

plot(2:12, asw[2:12], type="b", xlab="Number of Clusters",ylab="avg.silwidth") 
  
fit <- kmeans(seed, 4, nstart = 100) 
fit$cluster
plotcluster(seed, fit$cluster)

seed <- as.data.frame(seed)
seed$cluster_id <- fit$cluster

seed %>% group_by(cluster_id) %>% summarise_all(mean) %>% summarise_all(sd)

seed.dist <- dist(seed)
seed.hclust <- hclust(seed.dist, method="ward.D") # linkage method
plot(seed.hclust)
seed.10clust = cutree(seed.hclust,k=10)
seed.10clust
seed$cluster_id <- seed.10clust
seed %>% group_by(cluster_id) %>% summarise_all(mean) 

d<- dist(seed,method = "euclidean")
asw <- (nrow(seed)-1)*sum(apply(seed,2,var))
for (i in 1:12){
  cluster_ids<- cutree(seed.hclust,k=i)$dunn 
  asw[i]<- cluster.stats(d=d, clustering= cluster_ids)
}
