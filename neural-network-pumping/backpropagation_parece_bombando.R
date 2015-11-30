### loading data to my simple neural network
input <- rbind(
  c(0, 0),
  c(0, 1),
  c(1, 0),
  c(1, 1)
)

input <- addBiasThermFirstColumn(input)

y = cbind(c(1, 0, 0, 1))
output = y

theta1 <- rbind(
  c(-30, 20, 20), 
  c(-10, 20, 20)
)

theta2 <- rbind(c(10, -20, -20))


### doing forward propagation
a1 = input
a2 = addBiasThermFirstColumn(hypothesis(theta1, a1))
a3 = hypothesis(theta2, a2)



## doint backward propagation
error3 = a3 - y
error2 = error3 %*% theta2 * (a2 * (1-a2))

delta1 = t(error2) %*% a1
delta2 = t(error3) %*% a2

der2 <- delta2/nrow(a1)
der1 <- delta1/nrow(a1)
der1 <- removeBiasTherm(der1)



## doing gradient checking now
xIndex <- 1
yIndex <- 1
epsilon <- 0.001

theta2MaisUmPouquinho <- theta2
theta2MenosUmPouquinho <- theta2MaisUmPouquinho
theta2MaisUmPouquinho[xIndex, yIndex] <- theta2MaisUmPouquinho[xIndex, yIndex] + epsilon
theta2MenosUmPouquinho[xIndex, yIndex] <- theta2MenosUmPouquinho[xIndex, yIndex] - epsilon

ha <- hypothesis(theta2MaisUmPouquinho,  a2)
hb <- hypothesis(theta2MenosUmPouquinho, a2)

(costFunction(ha, y) - costFunction(hb, y))/(2*epsilon)
der2

## my gradient checking performs well when I am changing last layer thetas.


## I'll try to see if it performs well when I am changing a hidden layer theta


xIndex <- 2
yIndex <- 2
epsilon <- 0.001

theta2MaisUmPouquinho <- theta1
theta2MenosUmPouquinho <- theta2MaisUmPouquinho
theta2MaisUmPouquinho[xIndex, yIndex] <- theta2MaisUmPouquinho[xIndex, yIndex] + epsilon
theta2MenosUmPouquinho[xIndex, yIndex] <- theta2MenosUmPouquinho[xIndex, yIndex] - epsilon

ha <- hypothesis(theta2MaisUmPouquinho,  a1)
ha <- addBiasThermFirstColumn(ha)
ha <- hypothesis(theta2,  ha)

hb <- hypothesis(theta2MenosUmPouquinho, a1)
hb <- addBiasThermFirstColumn(hb)
hb <- hypothesis(theta2,  hb)


(costFunction(ha, y) - costFunction(hb, y))/(2*epsilon)
der1


### yeah! it performs well with hidden layers also.




### now, I will put my regularization to my deltas. 
### after doing that, I must verify my solution using
### the regularized cost function.

lambda <- 0.5

der1regularization <- der1
der1regularization <- lambda/nrow(a1) * der1[, 2:ncol(der1)]
der1regularization <- cbind(rep(0, nrow(der1regularization)), der1regularization)
der1regularization <- der1 + der1regularization

der2regularization <- der2
der2regularization <- lambda/nrow(a1) * der2[, 2:ncol(der2)]
der2regularization <- rbind(der2regularization)
der2regularization <- cbind(rep(0, nrow(der2regularization)), der2regularization)
der2regularization <- der2 + der2regularization


## theorically, these are the derivative therms of thetas for regularized neural 
## networks


xIndex <- 1
yIndex <- 1
epsilon <- 0.001

theta2MaisUmPouquinho <- theta2
theta2MenosUmPouquinho <- theta2MaisUmPouquinho
theta2MaisUmPouquinho[xIndex, yIndex] <- theta2MaisUmPouquinho[xIndex, yIndex] + epsilon
theta2MenosUmPouquinho[xIndex, yIndex] <- theta2MenosUmPouquinho[xIndex, yIndex] - epsilon

ha <- hypothesis(theta2MaisUmPouquinho,  a2)
hb <- hypothesis(theta2MenosUmPouquinho, a2)

(costFunctionWithRegularization(ha, y, theta1, theta2MaisUmPouquinho, lambda) 
- costFunctionWithRegularization(hb, y, theta1, theta2MenosUmPouquinho, lambda))/(2*epsilon)

der2regularization[xIndex, yIndex]


### yeah, it is working for the last layer.
### now, I will try for my hidden layer




xIndex <- 1
yIndex <- 1
epsilon <- 0.001

theta1MaisUmPouquinho <- theta1
theta1MenosUmPouquinho <- theta1MaisUmPouquinho
theta1MaisUmPouquinho[xIndex, yIndex] <- theta1MaisUmPouquinho[xIndex, yIndex] + epsilon
theta1MenosUmPouquinho[xIndex, yIndex] <- theta1MenosUmPouquinho[xIndex, yIndex] - epsilon

ha <- hypothesis(theta2, addBiasThermFirstColumn(hypothesis(theta1MaisUmPouquinho,  a1)))
hb <- hypothesis(theta2, addBiasThermFirstColumn(hypothesis(theta1MenosUmPouquinho, a1)))

(costFunctionWithRegularization(ha, y, theta1MaisUmPouquinho, theta2, lambda) 
- costFunctionWithRegularization(hb, y, theta1MaisUmPouquinho, theta2, lambda))/(2*epsilon)

der1regularization[xIndex, yIndex]





### trying gradient descent now


theta1 <- rbind(
  c(0.5, 1, 1.3), 
  c(0.7, 1.7, 0.2)
)

theta2 <- rbind(c(1.2, 0.6, 0.4))

alfa <- 0.1

newThetasTripa <- gradientDescent(input, y, theta1, theta2, alfa, 8000)


newTheta1 <- createMatrixFromTripa(newThetasTripa, ncol(theta1), nrow(theta1), 0)
newTheta2 <- createMatrixFromTripa(newThetasTripa, ncol(theta2), nrow(theta2), length(theta1))

grr <- performForwardPropagation(input, newTheta1, newTheta2)
print(grr)
