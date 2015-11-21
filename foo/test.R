
funcaoPrincipal <- function(x, y) {
	x^3 + y^4
}

derivadaX <- function(x, y) {
	3*(x^2)
}

derivadaY <- function(x, y) {
	4*(y^3)
}

testeDerivadaX <- function(x, y) {
	epsilon <- 0.1
	(funcaoPrincipal(x+epsilon, y) - funcaoPrincipal(x-epsilon, y)) / (2*epsilon) 
}


testeDerivadaY <- function(x, y) {
	epsilon <- 0.01
	(funcaoPrincipal(x, y+epsilon) - funcaoPrincipal(x, y-epsilon)) / (2*epsilon) 
}

#######################################

hypothesis <- function(theta, x) {
  z <- theta %*% t(x)
  1/(1+exp(-z))
}

derivadas <- function(x, y, theta) {
  m <- nrow(x)
  (1/m) * (hypothesis(theta, x) - y) %*% x
}


cost <- function(x, y, theta) {
  m <- nrow(x)
  hipo <- hypothesis(theta, x)
  vectorCostForEachPoint <- -y*log(hipo) - (1-y)*log(1-hipo)
  
  1/m * sum(vectorCostForEachPoint)
}


x <- rbind(c(0.1, 0.2, 0.3), c(0.4, 0.5, 0.6))
y <- c(1, 1)
theta <- c(0.5, 0.6, 0.9)

print(derivadas(x, y, theta))

epsilon <- 0.001

thetaP1 <- theta
thetaP2 <- theta

thetaP1[3] <- thetaP1[3]+epsilon
thetaP2[3] <- thetaP2[3]-epsilon

print((cost(x, y, thetaP1) - cost(x, y, thetaP2))/(2*epsilon))



