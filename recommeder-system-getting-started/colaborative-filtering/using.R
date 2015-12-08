rm(list = ls())

# this is the matrix with the rating of the users
# the dimension of this matrix is 5x4, because
# there are 5 movies and 4 users
y = rbind (
  c( 5,  5,  0,  0),
  c( 5, -1, -1,  0),
  c(-1,  4,  0, -1),
  c( 0,  0,  5,  4),
  c( 0,  0,  5, -1)
)

# this is the matrix that says which users have hated
# each movie. It is 1 if the user j has hated movie i
# or 0 if it hasn't
r = hasRated(y)

# this is the theta matrix.
# it has 4x3 dimensions.
# 4 is because there are 4 users in the y matrix
# and 3 is because I choose it. I am free to chose
# this number.
theta <- rbind(
  runif(3, min = -1, max=1), 
  runif(3, min = -1, max=1), 
  runif(3, min = -1, max=1), 
  runif(3, min = -1, max=1)
)


# this is the x matrix. it has 5x3 dimensions
# because there are 5 movies and I have choosen
# 3 for the number of thetas
x = rbind(
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1)
)

### trying gradient checking now

epsilon <- 0.001
lambda <- 0.5
i <- 2
j <- 2
xMaisUmPouco <- x
xMaisUmPouco[i, j] <- xMaisUmPouco[i, j] + epsilon

xMenosUmPouco <- x
xMenosUmPouco[i, j] <- xMenosUmPouco[i, j] - epsilon

(costFunction(xMaisUmPouco, y, r, theta, lambda) - costFunction(xMenosUmPouco, y, r, theta, lambda)) / (2*epsilon)
gradientX(x, y, r, theta, lambda)






i <- 2
j <- 2
epsilon <- 0.001
thetaMaisUmPouco <- theta
thetaMaisUmPouco[i, j] <- thetaMaisUmPouco[i, j] + epsilon

thetaMenosUmPouco <- theta
thetaMenosUmPouco[i, j] <- thetaMenosUmPouco[i, j] - epsilon


(costFunction(x, y, r, thetaMaisUmPouco, lambda) - costFunction(x, y, r, thetaMenosUmPouco, lambda)) / (2*epsilon)
gradientTheta(x, y, r, theta, lambda)



#####  trying gradient descent now

returnOfGradientDescent <- gradientDescent(x, y, r, theta, 0.1, 0.1, 30)

myPrediction <- returnOfGradientDescent@x %*% t(returnOfGradientDescent@theta)


