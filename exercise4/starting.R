rm(list = ls())

### reading data
setwd('/home/nozes/github-machine-learning-coursera/exercise4')

x = read.csv2('input/data1.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
x <- as.matrix(sapply(x, as.numeric))

y = read.csv2('input/y.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)$V1
y <- as.matrix(sapply(y, as.numeric))

theta1 = read.csv2('input/theta1.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
theta2 = read.csv2('input/theta2.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)

theta1 <- as.matrix(sapply(theta1, as.numeric))
theta2 <- as.matrix(sapply(theta2, as.numeric))

### visualizing data
indexNumber <- 1
digit <- matrix(as.numeric(matrix(x[indexNumber,], ncol = 20)), ncol=20)
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(digit))


### implementing forward propagation

x <- cbind(rep(1, nrow(x)), x)

dim(x)
dim(t(theta1))

outputLayerOne <- hypothesis(theta1, t(x))
dim(outputLayerOne)
inputLayerTwo <- cbind(1, outputLayerOne)

dim(inputLayerTwo)
dim(t(theta2))





m <- nrow(inputLayerTwo)
hipo <- hypothesis(theta2, inputLayerTwo)
dim(hipo)



# vou ter que criar uma matix de 5000x10

yMatrix <- matrix(NA, nrow = 5000, ncol=10)
for(i in 1:5000) {
  yMatrix[i, ] <- createVectorForSinglePoint(y[i])
}


### agora vou ver se consigo subtrair

dim(hipo)
dim(yMatrix)

vectorCostForEachPoint <- -yMatrix*log(hipo) - (1-yMatrix)*log(hipo)

sum(vectorCostForEachPoint)/5000

acertou <- 0
for(nnn in 1:5000) {
  if(which.max(yMatrix[nnn,]) == which.min(vectorCostForEachPoint[nnn,])) {
    acertou <- acertou+1
  } 
}


acertou
4876/5000


############################################################
############################################################
############################################################

## tentando fazer a derivada agora o.O


sigma3 <-