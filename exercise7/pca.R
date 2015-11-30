rm(list = ls())
setwd('/home/nozes/github-machine-learning-coursera/exercise7')
source('pca_funcions.R')

# loading data and plotting graphics
X <- as.matrix(read.csv('input/data1.txt', header=FALSE, sep=' '))
plot(X)
plot(normalizeFeatures(X))

## performing pca
normalizedX <- normalizeFeatures(X)
svd <- calculateSvd(normalizedX)
ureduce <- calculateUreduce(svd, 1)
z <- calculateZ(ureduce, normalizedX)
class(z)
class(ureduce)
plot(reconstructMatrix(ureduce, z))


#################

## choosing k

# try pca with k = 1
# compute ureduce
# do some checking



X <- as.matrix(read.csv('input/data1.txt', header=FALSE, sep=' '))
normalizedX <- normalizeFeatures(X)
naosei <- chooseNumberOfK(normalizedX, 0.20)
dim(naosei@ureduce)
dim(naosei@z)
plot(reconstructMatrix(naosei@ureduce, naosei@z))
