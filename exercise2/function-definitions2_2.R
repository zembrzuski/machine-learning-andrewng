# rm(list = ls())

mapFeature <- function(x) {
  cbind(
    rep(1, nrow(x)),
    x[,2],
    x[,3],
    x[,2]^2,
    x[,2]*x[,3],
    x[,3]^2,
    x[,2]^3,
    x[,2]^2*x[,3],
    x[,2]*x[,3]^2,
    x[,3]^3,
    x[,2]^4,
    x[,2]^3*x[,3],
    x[,2]^2*x[,3]^2,
    x[,2]^1*x[,3]^3,
    x[,3]^4,
    x[,2]^5,
    x[,2]^4*x[,3],
    x[,2]^3*x[,3]^2,
    x[,2]^2*x[,3]^3,
    x[,2]^1*x[,3]^4,
    x[,3]^5,
    x[,2]^6,
    x[,2]^5*x[,3]^1,
    x[,2]^4*x[,3]^2,
    x[,2]^3*x[,3]^3,
    x[,2]^2*x[,3]^4,
    x[,2]^1*x[,3]^5,
    x[,3]^6
  )
}


hypothesis <- function(theta, x) {
  z <- theta %*% t(x)
  1/(1+exp(-z))
}

regularization <- function(lambda, m, theta) {
  thetaSquaredWithoutThetaZero <- tail(theta, -1)^2
  lambda / (2*m) * sum(thetaSquaredWithoutThetaZero)
}

cost <- function(x, y, theta, lambda) {
  m <- nrow(x)
  hipo <- hypothesis(theta, x)
  vectorCostForEachPoint <- -y*log(hipo) - (1-y)*log(1-hipo)
  regularization <- regularization(lambda, m, theta)
  
  1/m * sum(vectorCostForEachPoint) + regularization
}

gradientDescent <- function(x, y, theta, alfa, nIter, lambda) {
  m <- nrow(x)
  cost <- rep(NA, nIter)
  
  for(i in 1:nIter) {
    cost[i] <- cost(x, y, theta, lambda)

    # theta 0 derivative    
    summation <- (hypothesis(theta, x) - y) %*% x
    theta0 <- head(summation[1,], 1)
    
    # other thetas derivatives
    parenthesis <- 1/m * tail(summation[1,], -1)
    regularizationTerm <- lambda/m * tail(theta, -1)
    thetaOthers <- parenthesis + regularizationTerm

    theta <- theta - alfa * c(theta0, thetaOthers)
  }
  
  print(theta)
  cost
}