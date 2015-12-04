# content based recommendations

y <- rbind(
  c( 5,  5,  0,  0),
  c( 5, -1, -1,  0),
  c(-1,  4,  0, -1),
  c( 0,  0,  5,  4),
  c( 0,  0,  5, -1)
)

# matrix that says if user j has rated movie i
r <- rbind(
  c( 1,  1,  1,  1),
  c( 1,  0,  0,  1),
  c( 0,  1,  1,  0),
  c( 1,  1,  1,  1),
  c( 1,  1,  1,  0)
)




x1 <- c(0.9, 1, 0.99, 0.1, 0)
x2 <- c(0, 0.01, 0, 1.0, 0.9)
X = cbind(x1, x2)
X = addBiasColumn(X)
X

theta1 <- c(0, 5, 0) #possui 3 dimensoes, pq só tem 2features (x1 e x2)
theta2 <- c(0, 5, 0) #idem
theta3 <- c(0, 0, 5)
theta4 <- c(0, 0, 5)

thetas <- rbind(theta1, theta2, theta3, theta4)

## com isso aqui, eu consigo predizer o 'rating' para cada filme.
## entretanto, aqui eu já dei os thetas. a grande sacada para esse
## momento é 'descobrir' sozinho os thetas.
## para isso, preciso saber a funcao de custo.
prediction <- X %*% t(thetas)
y

costFunction(X, thetas, y, r)
## 1.5775


gradient(X, thetas, y, r)




# trying gradient checking now

epsilon <- 0.001
i <- 3
j <- 3

thetasGrande <- thetas
thetasPequeno <- thetas

thetasGrande[i, j] <- thetasGrande[i, j] + epsilon
thetasPequeno[i, j] <- thetasPequeno[i, j] - epsilon


(costFunction(X, thetasGrande, y, r) - costFunction(X, thetasPequeno, y, r))/(2*epsilon)




lambda <- 1.3


gradientWithRegularization(X, thetas, y, r, lambda)


epsilon <- 0.001
i <- 4
j <- 2

thetasGrande <- thetas
thetasPequeno <- thetas

thetasGrande[i, j] <- thetasGrande[i, j] + epsilon
thetasPequeno[i, j] <- thetasPequeno[i, j] - epsilon


(costFunctionWithRegularization(X, thetasGrande, y, r, lambda) - costFunctionWithRegularization(X, thetasPequeno, y, r, lambda))/(2*epsilon)



theta1 <- c(0.2, 0.5, 1.7) #possui 3 dimensoes, pq só tem 2features (x1 e x2)
theta2 <- c(-0.2, -0.5, -0.7) #idem
theta3 <- c(0.3, -0.1, -0.3)
theta4 <- c(-0.3, 1.2, 1.1)

thetas <- rbind(theta1, theta2, theta3, theta4)


alfa <- 0.1
nIter <- 50
newThetas <- gradientDescentWithRegularization(X, thetas, y, r, alfa, nIter, 5)
newThetas <- gradientDescent(X, thetas, y, r, alfa, nIter)

