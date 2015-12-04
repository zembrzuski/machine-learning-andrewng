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
# because there are 5 users and I have choosen
# 3 for the number of thetas
x = rbind(
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1)
)

costFunction(x, y, r, theta, 50)

gradientX(x, y, r, theta, 2)
gradientTheta(x, y, r, theta, 2)


## i'll try gradient checking now