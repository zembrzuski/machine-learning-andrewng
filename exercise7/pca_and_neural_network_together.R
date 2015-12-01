
# what I am doing with this file
# in this file, I'll perform PCA to reduce the problem of digits recognition from 400 dimension
# to 130 dimensions.
# I choose 130 dimensions because it maintain 99% of the variability.
# After reducing the number of dimensions, I will perform my neural network training and I'll
# that it still works fine.

# loading the data
setwd('/home/nozes/github-machine-learning-coursera/neural-network-pumping')
source('funcions.R')
x = read.csv2('input/data1.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
x <- as.matrix(sapply(x, as.numeric))

y = read.csv2('input/y.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)$V1
y <- as.matrix(sapply(y, as.numeric))

yMatrix <- createResultMatrix(y)


# perform PCA to reduce to 130 dimensions instead of 4000
naosei <- doPca(x, 0.20, 130)
xx <- t(naosei@z)

## now, my xx variable has the x variable reduced from 400 to 130


## now I'lls perform my neural network to my reduced dataset.
theta1 <- matrix(data = runif(25*131, min=-1, max=1) , nrow = 25, ncol = 131)
theta2 <- matrix(data = runif(10*26, min=-1, max=1) , nrow = 10, ncol = 26)

alfa <- 1.2
newThetasTripa <- gradientDescent(xx, yMatrix, theta1, theta2, alfa, 50)

newTheta1 <- createMatrixFromTripa(newThetasTripa, ncol(theta1), nrow(theta1), 0)
newTheta2 <- createMatrixFromTripa(newThetasTripa, ncol(theta2), nrow(theta2), length(theta1))

grr <- performForwardPropagation(xx, newTheta1, newTheta2)

# calculating accuracy
numberOfHits = 0
for(i in 1:nrow(grr)) {
  if(which.max(grr[i,]) == which.max(yMatrix[i,])) {
    numberOfHits <- numberOfHits + 1
  } 
}
print(numberOfHits/nrow(grr))

### in this point, I can see that my neural network works fine with the reduced dimension problem.