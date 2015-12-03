# it works
addBiasColumn <- function(X) {
  cbind(rep(1, nrow(X)), X)
}

addZeroColumn <- function(X) {
  cbind(rep(0, nrow(X)), X)
}

# it works
costFunction <- function(X, thetas, y, rating) {
  prediction <- X %*% t(thetas)
  errorMatrix <- (prediction - y)^2 * rating
  1/2 * sum(errorMatrix)
}

# it works
gradient <- function(X, thetas, y, rating) {
  prediction <- X %*% t(thetas)
  t(((prediction - y) * rating)) %*% X
}


# trying regularization now

costFunctionWithRegularization <- function(X, thetas, y, rating, lambda) {
  prediction <- X %*% t(thetas)
  errorMatrix <- (prediction - y)^2 * rating
  regularization <- sum(thetas[,-1]^2)
  1/2 * sum(errorMatrix) + lambda/2*regularization
}

gradientWithRegularization <- function(X, thetas, y, rating, lambda) {
  prediction <- X %*% t(thetas)
  algo <- t(((prediction - y) * rating)) %*% X
  algo + lambda * addZeroColumn(thetas[,-1])
}


# no regularization
gradientDescent <- function(X, thetas, y, rating, alfa, nIter) {
  m <- nrow(X)
  cost <- rep(NA, nIter)
  
  for(i in 1:nIter) {
    cost[i] <- costFunction(X, thetas, y, rating)
    
    thetas <- thetas - alfa * gradient(X, thetas, y, rating)
  }

  plot(cost)  
  print(cost[nIter])
  thetas
}



gradientDescentWithRegularization <- function(X, thetas, y, rating, alfa, nIter, lambda) {
  m <- nrow(X)
  cost <- rep(NA, nIter)
  
  for(i in 1:nIter) {
    cost[i] <- costFunction(X, thetas, y, rating)
    
    thetas <- thetas - alfa * gradientWithRegularization(X, thetas, y, rating, lambda)
  }
  
  plot(cost)  
  print(cost[nIter])
  thetas
}
