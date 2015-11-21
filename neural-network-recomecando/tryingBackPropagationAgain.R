rm(list=ls())

input <- cbind(
  c(0, 0),
  c(0, 1),
  c(1, 0),
  c(1, 1)
)

input <- addBiasTherm(input)

theta1 <- rbind(
  c(0.1, 0.12, 0.4), 
  c(0.3, 0.15, 0.2)
)

theta2 <- rbind(c(0.21, 0.05, 0.17))


### forward propagation

a1 = input
a2 = addBiasTherm(hypothesis(theta1, a1))
a3 = hypothesis(theta2, a2)

output = rbind(c(1, 0, 0, 0))
y = output 

error3 = a3 - y
error2 = t(theta2) %*% error3 * (a2 * (1-a2))

delta_2 = error3 %*% t(a2)
delta_1 = error2 %*% t(a1)

m <- ncol(input)
DER_2 <- delta_2 / m
DER_1 <- delta_1 / m 


### fazer agora: checking gradient

a2 -> theta2 -> output

epsilon <- 0.001
thetaA <- theta2
thetaA[,2] <- thetaA[,2] + epsilon
thetaB <- theta2
thetaB[,2] <- thetaB[,2] - epsilon


cost(a2, y, thetaA) - cost(a2, y, thetaB) / (2*epsilon)
-529.7855

delta_2
DER_2
