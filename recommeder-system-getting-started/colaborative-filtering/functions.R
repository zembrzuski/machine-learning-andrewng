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