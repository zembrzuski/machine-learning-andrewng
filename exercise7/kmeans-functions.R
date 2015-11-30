# given a point (2-dimensional point) and a matrix that represents
# a list of 2-dimensional points, it returns the index of the closest
# centroid
findClosestCentroid <- function(point, centroids) {
  squaredDifferences <- (matrix(data = rep(point, nrow(centroids)), ncol=length(point), byrow=TRUE) - centroids)^2
  foo <- sqrt(rowSums(squaredDifferences))
  which.min(foo)
}


# input:
#    X is a 2-dimensional dataset
#    centroids is a list of 2-dimensional points, so it is a matrix (nx2)
# output: 
#    list of indices of centroids.  
findClosestCentroids <- function(X, centroids) {
  closestCentroids <- rep(NA, nrow(X))
  
  for(i in 1:nrow(X)) {
    closestCentroids[i] <- findClosestCentroid(X[i,], centroids)
  }
  
  closestCentroids
}




computeCentroids <- function(X, clusters, numberOfCentroids) {
  newCentroids <- matrix(nrow=numberOfCentroids, ncol=ncol(X))
  
  for(i in 1:numberOfCentroids) {
    clusterPoints <- X[which(clusters == i),]
    newCentroids[i,] <- 1/nrow(clusterPoints) * colSums(clusterPoints)
  }

  newCentroids  
}



kMeansInitCentroids <- function(X, numberOfCentroids) {
  as.matrix(X[sample(1:nrow(X))[1:numberOfCentroids],])
}

knn <- function(X, numberOfCentroids) {
  centroids <- kMeansInitCentroids(X, numberOfCentroids)
  newCluster <- findClosestCentroids(X, centroids)
  cluster <- rep(0, nrow(X))
  
  numberOfIterations <- 0
  while(!all(cluster == newCluster) && numberOfIterations < 80) {
    numberOfIterations <- numberOfIterations + 1
    print(numberOfIterations)
    cluster <- newCluster
    plot(X, col=cluster)
    centroids <- computeCentroids(X, cluster, numberOfCentroids)
    newCluster <- findClosestCentroids(X, centroids)
  }
  
  print('---')
  print(centroids)
  print('---')
  cluster
}