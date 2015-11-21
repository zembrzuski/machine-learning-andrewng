rm(list = ls())

hypothesis <- function(theta, x) {
  z <- x %*% t(theta)
  1/(1+exp(-z))
}

### 
x = rbind(c(1, 0, 0), c(1, 0, 1))
a1 = x
theta1 <- rbind(c(-30, 20, 20), c(-10, 20, 20))
a2 = cbind(1, hypothesis(theta1, x))



theta2 <- rbind(c(10, -20, -20))

a3 = hypothesis(theta2, a2)
y = rbind(1, 1)



sigma3 <- a3 - y


sigma2 <- t(theta2) %*% sigma3



theta2
sigma3
