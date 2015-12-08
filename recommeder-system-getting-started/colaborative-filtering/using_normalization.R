rm(list = ls())

y = rbind (
  c( 5,  5,  0,  0, -1),
  c( 5, -1, -1,  0, -1),
  c(-1,  4,  0, -1, -1),
  c( 0,  0,  5,  4, -1),
  c( 0,  0,  5,  0, -1)
)


yNormalized <- normalizeByTheMean(y)

r = hasRated(y)

theta <- rbind(
  runif(3, min = -1, max=1), 
  runif(3, min = -1, max=1), 
  runif(3, min = -1, max=1), 
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1)
)

x = rbind(
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1),
  runif(3, min = -1, max=1)
)




returnOfGradientDescent <- gradientDescent(x, yNormalized, r, theta, 0.1, 0.1, 30)


myPrediction <- returnOfGradientDescent@x %*% t(returnOfGradientDescent@theta) + computeMeans(y)

