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
indexNumber <- 2010
digit <- matrix(as.numeric(matrix(x[indexNumber,], ncol = 20)), ncol=20)
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(digit))


### implementing forward propagation


class(x[1,1])
class(theta1[1,1])

class(x)
ncol(x)

x <- cbind(rep(1, nrow(x)), x)

dim(x)
dim(t(theta1))
class(theta1[1,1])
outputLayerOne <- hypothesis(theta1, x)
dim(outputLayerOne)
inputLayerTwo <- cbind(1, outputLayerOne)

outputLayerTwo <- hypothesis(theta2, inputLayerTwo)
dim(outputLayerTwo)
outputLayerTwo[1,] < 0.1
