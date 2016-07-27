### data preprocessing for hierarchichal clustering. 
### Only 5% of the data have been used  for clustering. More data is not a problem for
### clustering algorithm but the visualization becomes extremely dense.
set.seed(4)
index <- sample(c(TRUE, FALSE), nrow(iris), p = c(0.05, 0.95), replace = TRUE)
myIris <- iris[index,3:4]
myIris

### Clustering done on a subset of iris data which is  named myIris
disM <- dist(myIris)
irisClust <- hclust(disM)
clusters <- cutree(irisClust, k = 3)
clusters
iris[index,5]


#############################################################################
# Challenge: Find 3 clusters in mtcars dataset using hierarchical clusterinng. 
# Do not use mpg variable in clustering. After clustering make a box plot of 
# mpg vs clustering group 
#############################################################################



