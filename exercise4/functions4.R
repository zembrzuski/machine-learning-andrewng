hypothesis <- function(theta, x) {
  z <- x %*% t(theta)
  1/(1+exp(-z))
}

cost <- function(x, y, theta) {
  m <- nrow(x)

  hipo <- hypothesis(theta, x)
  vectorCostForEachPoint <- -y*log(hipo) - (1-y)*log(hipo)
  soma <- sum(vectorCostForEachPoint)

  1/m * soma
}




# usado para criar a matriz de resultados
createVectorForSinglePoint <- function(x) {
  vec <- rep(0, 10)
  
  vec[x] <- 1
  
  vec
}
