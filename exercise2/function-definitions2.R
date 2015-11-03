# hypothesis defined at 2.2.1
hypothesis <- function(theta, x) {
  z <- theta %*% t(x)
  1/(1+exp(-z))
}

# cost function defined at 2.2.1
cost <- function(x, y, theta) {
  m <- nrow(x)
  hipo <- hypothesis(theta, x)
  vectorCostForEachPoint <- -y*log(hipo) - (1-y)*log(1-hipo)
  
  1/m * sum(vectorCostForEachPoint)
}

# implementation of gradient descent on the top of page 6
gradientDescent <- function(x, y, theta, alfa, nIter) {
  m <- nrow(x)
  cost <- rep(NA, nIter)
    
  for(i in 1:nIter) {
    cost[i] <- cost(x, y, theta)
    summation <- (hypothesis(theta, x) - y) %*% x
    theta <- theta - alfa * (1/m) * summation  
  }

  print(theta)
  cost
}

