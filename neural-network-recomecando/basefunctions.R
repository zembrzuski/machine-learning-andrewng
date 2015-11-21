#rm(list = ls())

hypothesis <- function(theta, x) {
  z <- theta %*% x
  1/(1+exp(-z))
}

addBiasTherm <- function(x) {
  rbind(rep(1, ncol(x)), x)
}


cost <- function(x, y, theta) {
  m <- nrow(x)
  hipo <- hypothesis(theta, x)
  vectorCostForEachPoint <- -y*log(hipo) - (1-y)*log(1-hipo)
  
  1/m * sum(vectorCostForEachPoint)
}

