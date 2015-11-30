rm(list = ls())
setwd('/home/nozes/github-machine-learning-coursera/exercise7')

X <- as.matrix(read.csv('input/data1.txt', header=FALSE, sep=' '))
plot(X)

centeredColumn1 <- X[,1] - mean(X[,1])
centeredColumn2 <- X[,2] - mean(X[,2])

plot(cbind(centeredColumn1, centeredColumn2))

normalizedColumn1 <- centeredColumn1/(max(centeredColumn1)-min(centeredColumn1)) 
normalizedColumn2 <- centeredColumn2/(max(centeredColumn2)-min(centeredColumn2)) 


normalizedX <- cbind(normalizedColumn1, normalizedColumn2)

plot(normalizedX)


## calculating the covariance matrix
sigma <- 1/nrow(normalizedX) * t(normalizedX) %*% normalizedX

ureduce <- svd(sigma)$u[,1]
ureduce <- cbind(ureduce)


z <- (t(ureduce) %*% t(normalizedX))[1,]
z <- rbind(z)

xapprox <- ureduce %*% z
xapprox <- t(xapprox)
dim(xapprox)

plot(xapprox)

