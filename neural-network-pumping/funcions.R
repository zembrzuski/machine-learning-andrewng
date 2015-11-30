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

  

performForwardPropagation <- function(x, theta1, theta2) {
  a1 <- addBiasThermFirstColumn(x)
  a2 <- addBiasThermFirstColumn(hypothesis(theta1, a1))
  a3 <- hypothesis(theta2, a2)
  a3
}



performBackPropagation <- function(x, y, theta1, theta2) {
  a1 <- addBiasThermFirstColumn(x)
  a2 <- addBiasThermFirstColumn(hypothesis(theta1, a1))
  a3 <- hypothesis(theta2, a2)
  
  error3 = a3 - y
  error2 = error3 %*% theta2 * (a2 * (1-a2))
  
  delta1 = t(error2) %*% a1
  delta2 = t(error3) %*% a2
  
  der2 <- delta2/nrow(a1)
  der1 <- delta1/nrow(a1)
  der1 <- removeBiasTherm(der1)
  
  c(der1, der2)
}



performBackPropagationWithRegularization <- function(x, y, theta1, theta2, lambda) {
  a1 <- addBiasThermFirstColumn(x)
  a2 <- addBiasThermFirstColumn(hypothesis(theta1, a1))
  a3 <- hypothesis(theta2, a2)
  
  error3 = a3 - y
  error2 = error3 %*% theta2 * (a2 * (1-a2))
  
  delta1 = t(error2) %*% a1
  delta2 = t(error3) %*% a2
  
  der2 <- delta2/nrow(a1)
  der1 <- delta1/nrow(a1)
  der1 <- removeBiasTherm(der1)
  
  der1regularization <- der1
  der1regularization <- lambda/nrow(a1) * der1[, 2:ncol(der1)]
  der1regularization <- cbind(rep(0, nrow(der1regularization)), der1regularization)
  der1regularization <- der1 + der1regularization
  
  der2regularization <- der2
  der2regularization <- lambda/nrow(a1) * der2[, 2:ncol(der2)]
  der2regularization <- rbind(der2regularization)
  der2regularization <- cbind(rep(0, nrow(der2regularization)), der2regularization)
  der2regularization <- der2 + der2regularization
  
  c(der1regularization, der2regularization)
}





gradientDescent <- function(x, y,theta1, theta2, alfa,  numberIterations) {
  cost <- rep(NA, numberIterations)
  
  for(i in 1:numberIterations) {
    result <- performForwardPropagation(x, theta1, theta2)
    cost[i] <- costFunction(result, y)
    
    derivadasTripa <- performBackPropagation(x, y, theta1, theta2)
    
    theta1 <- theta1 - alfa * createMatrixFromTripa(derivadasTripa, ncol(theta1), nrow(theta1), 0)
    theta2 <- theta2 - alfa * createMatrixFromTripa(derivadasTripa, ncol(theta2), nrow(theta2), length(theta1))
  }

  plot(cost)
  c(theta1, theta2)
}



gradientDescentWithRegularization <- function(x, y,theta1, theta2, alfa,  numberIterations, lambda) {
  cost <- rep(NA, numberIterations)
  
  for(i in 1:numberIterations) {
    result <- performForwardPropagation(x, theta1, theta2)
    cost[i] <- costFunctionWithRegularization(result, y, theta1, theta2, lambda)

    derivadasTripa <- performBackPropagationWithRegularization(x, y, theta1, theta2, lambda)
    
    theta1 <- theta1 - alfa * createMatrixFromTripa(derivadasTripa, ncol(theta1), nrow(theta1), 0)
    theta2 <- theta2 - alfa * createMatrixFromTripa(derivadasTripa, ncol(theta2), nrow(theta2), length(theta1))
  }
  
  plot(cost)
  c(theta1, theta2)
}


createMatrixFromTripa <- function(tripa, ncol, nrow, offset) {
  matrix(data = tripa[(offset+1):(ncol*nrow+offset)], ncol=ncol, nrow=nrow)  
}
