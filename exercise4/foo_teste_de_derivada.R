rm(list=ls())

x = 16
y = x^2
dy = 2*x

epsilon = 5
teste = ((x+epsilon)^2 - (x-epsilon)^2) / (2*epsilon)

#####

x = 3
y = 3
z = 
dz = 2*x

epsilon = 2
teste = (((x+epsilon)^2 + y^2) - ((x-epsilon)^2 + y^2)) / (2*epsilon)

###


funcaoOriginal <- function(x, y) {
  x^2 + y^2 
}

derivada <- function(x) {
  2*x
}

teste <- function(x, y) {
  epsilon = 0.01
  (funcaoOriginal(x+epsilon, y) - funcaoOriginal(x-epsilon, y)) / (2*epsilon)
}

funcaoOriginal(3, 4)
derivada(3)
teste(2, 4)
