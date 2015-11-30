rm(list = ls())

# this is the object that will be returned from calling chooseNumberOfK
# why? this funcion do all the pca job and returns z (the principal components)
# and ureduce (the matrix that is used to reconstruct the data)
setClass('returnOfPca', representation(z = 'matrix', ureduce = 'matrix'))

# normalize a X matrix
normalizeFeatures <- function(X) {
  newX <- matrix(NA, ncol=ncol(X), nrow=nrow(X))
  
  for(i in 1:ncol(X)) {
    centeredColumn <- X[,i] - mean(X[,i])
    normalizedColumn <- centeredColumn/(max(centeredColumn)-min(centeredColumn)) 
    newX[,i] <- normalizedColumn
  }
 
  newX
}

calculateSvd <- function(X) {
  sigma <- 1/nrow(X) * t(X) %*% X
  svd(sigma)
}

calculateUreduce <- function(svd, numberOfDimensions) {
  ureduce <- cbind(svd$u[,1:numberOfDimensions])
}

calculateZ <- function(ureduce, X) {
  algo <- t(ureduce) %*% t(X)
  rbind(algo)
}

reconstructMatrix <- function(ureduce, z) {
  t(ureduce %*% z)
}


# check the variability of the reduction. 
doTheChecking <- function(svd, numberOfDimensions, percentual) {
  diagonal <- svd$d
  numerator <- sum(diagonal[1:numberOfDimensions])
  denominator <- sum(diagonal)
  result <- 1 - (numerator/denominator)
  result <= percentual
}

# this function do all the pca job. 
# input - a normalized matrix and the maximum variability that you accept
# it returns a 'returnOfPca' object, that contains z (the principal components)
# and ureduce, that is used to reconstruct the data.
chooseNumberOfK <- function(normalizedX, variabilidadeMaxima) {
  answerAchieved <- FALSE
  z <- NA
  ureduce <- NA
  numberOfDimensions <- 1
  while(!answerAchieved) {
    print(paste('trying pca with', numberOfDimensions))
    svd <- calculateSvd(normalizedX)
    ureduce <- calculateUreduce(svd, numberOfDimensions)
    z <- calculateZ(ureduce, normalizedX)
    answerAchieved <- doTheChecking(svd, numberOfDimensions, variabilidadeMaxima)
    numberOfDimensions <- numberOfDimensions + 1
  }

  print(paste('pca got with number of dimensions: ', numberOfDimensions -1))
  
  returnOfPca <- new('returnOfPca')
  returnOfPca@ureduce <- ureduce
  returnOfPca@z <- z
  returnOfPca
}
