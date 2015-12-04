setClass('returnGradientDescent', representation(x = 'matrix', theta = 'matrix'))

hasRated <- function(y) {
  (y != -1)*1  
}

costFunction <- function(x, y, r, theta, lambda) {
  summation <- sum((((x %*% t(theta)) - y) * r)^2)
  xRegularization <- sum(x^2)
  thetaRegularization <- sum(theta^2)
  
  1/2 * summation + lambda/2*xRegularization + lambda/2*thetaRegularization
}


gradientX <- function(x, y, r, theta, lambda) {
  matrix <- (((x %*% t(theta)) - y) * r) %*% theta
  matrix + lambda * x
}

gradientTheta <- function(x, y, r, theta, lambda) {
  matrix <- t((x %*% t(theta) - y) * r) %*% x
  matrix + lambda * theta
}



gradientDescent <- function(x, y, r, theta, lambda, alfa, nIter) {
  m <- nrow(x)
  cost <- rep(NA, nIter)
  
  for(i in 1:nIter) {
    cost[i] <- costFunction(x, y, r, theta, lambda)
    
    theta <- theta - alfa * gradientTheta(x, y, r, theta, lambda)
    x <- x - alfa * gradientX(x, y, r, theta, lambda)
  }
  
  plot(cost)  
  print(cost[nIter])
  
  returnOfGradientDescent <- new('returnGradientDescent')
  returnOfGradientDescent@x <- x
  returnOfGradientDescent@theta <- theta
  returnOfGradientDescent
}
