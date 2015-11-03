rm(list = ls())

# hypothesis defined at 2.2.1
hypothesis <- function(theta, x) {
  z <- theta %*% t(x)
  1/(1+exp(-z))
}

cost <- function(x, y, theta, lambda) {
  m <- nrow(x)
  hipo <- hypothesis(theta, x)
  vectorCostForEachPoint <- -y*log(hipo) - (1-y)*log(1-hipo)
  regularization <- lambda/(2*m)*sum(tail(theta, -1)^2)

  1/m * sum(vectorCostForEachPoint) + regularization
}



derivativeZero <- function(x, y, theta, m) {
  summation <- (hypothesis(theta[1], x[,1]) - y[1]) %*% x[,1]
  (1/m) * summation
}


derivativeOther <- function(x, y, theta, m, lambda) {
  summation <- (hypothesis(theta, x) - y) %*% x
  regularization <- lambda/m * theta
  newThetas <- (1/m) * summation + regularization
  
  # discarding theta0
  tail(newThetas[1,], -1)
}



# implementation of gradient descent on the top of page 6
gradientDescent <- function(x, y, theta, alfa, lambda, nIter) {
  m <- nrow(x)
  cost <- rep(NA, nIter)
  
  for(i in 1:nIter) {
    cost[i] <- cost(x, y, theta, lambda)
    
    d0 <- derivativeZero(x, y, theta, m)
    dn <- derivativeOther(x, y, theta, m, lambda)
    theta <- theta - alfa * c(d0, dn)
  }
  
  print(theta)
  cost
}

