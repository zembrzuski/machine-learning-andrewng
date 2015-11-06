rm(list = ls())

hypothesis <- function(theta, x) {
  z <- x %*% t(theta)
  1/(1+exp(-z))
}

# and
theta = c(-30, 20, 20)
x = rbind(c(1, 1, 0))
hypothesis(theta, x)

# or
theta = c(-10, 20, 20)
x = rbind(c(1, 0, 0))
hypothesis(theta, x)


thetas <- rbind(c(-30, 20, 20), c(-10, 20, 20))
x = rbind(c(1, 0, 0))
hypothesis(thetas, x)[1,]
c(1, hypothesis(thetas, x)[1,])


simpleNeuralNetwork <- function(x) {

  # layer one
  thetas <- rbind(c(-30, 20, 20), c(-10, 20, 20))
  outputLayerOne <- hypothesis(thetas, x)[1,]
  inputLayerTwo <- c(1, outputLayerOne)
  print(inputLayerTwo > 0.9)

  # layer two
  thetas <- rbind(c(10, -20, -20))
  output <- hypothesis(thetas, rbind(inputLayerTwo))[1,]
    
  print(output > 0.9)
}

x = rbind(c(1, 0, 0))
simpleNeuralNetwork(x)
