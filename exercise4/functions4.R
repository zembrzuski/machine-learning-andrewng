hypothesis <- function(theta, x) {
  z <- x %*% t(theta)
  #z <- theta %*% t(x)
  1/(1+exp(-z))
}

cost <- function(x, y, theta) {
  m <- nrow(x)

  sumsum <- 0
  
  for(k in 1:nrow(theta)) {
    hipo <- hypothesis(theta, x)
    vectorCostForEachPoint <- -y*log(hipo) - (1-y)*log(hipo)
    sumsum <- sumsum + sum(vectorCostForEachPoint)
  }
    
  1/m * sumsum
}