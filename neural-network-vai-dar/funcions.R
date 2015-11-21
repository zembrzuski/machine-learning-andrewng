rm(list = ls())

addBiasThermFirstColumn <- function(x) {
  cbind(rep(1, nrow(x)), x)
}

hypothesis <- function(theta, x) {
  z <- x %*% t(theta)
  1/(1+exp(-z))
}

# create matrix of results to multiclass classification.
# it does not seem to be a generic function, but I'll keep it here.
createResultMatrix <- function(y) {
  yMatrix <- matrix(NA, nrow=length(y), ncol=10)
  
  for(i in 1:length(y)) {
    pointVector <- rep(0, 10)
    index <- y[i]
    pointVector[index] <- 1
    yMatrix[i,] <- pointVector
  }
  
  yMatrix
}

costFunction <- function(result, y) {
  m <- nrow(result)

  # just like logistic regularization :)
  soma <- sum(-y*log(result) - (1-y)*log(1-result))
  1/m * sum(soma)  
}

# it works only for a 3 layer neural network, i.e., for only one hidden layer.
# if I want, I can generalize it to accept n layers.
costFunctionWithRegularization <- function(result, y, theta1, theta2, lambda) {
  m <- nrow(result)
  
  soma <- sum(-y*log(result) - (1-y)*log(1-result))
  
  newTheta1 <- theta1[,2:ncol(theta1)]
  newTheta2 <- theta2[,2:ncol(theta2)]
  
  regularization <- lambda/(2*m)  * (sum(newTheta1^2) + sum(newTheta2^2))
  
  1/m * sum(soma) + regularization
}

# it is used to remove unused therms of my deltas
removeBiasTherm <- function(m) {
  m[2:nrow(m),]  
}

