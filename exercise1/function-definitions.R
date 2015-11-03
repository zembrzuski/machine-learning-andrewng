# hypothesis defined at 2.2.1
hypothesis <- function(theta, x) {
  theta %*% t(x)
}

# cost function defined at 2.2.1
cost <- function(x, y, theta) {
  m <- nrow(x)
  squaredDifferences <- (hypothesis(theta, x) - y)^2
  
  1/(2*m) * sum(squaredDifferences)
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

