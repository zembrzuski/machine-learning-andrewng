rm(list = ls())
setwd('/home/nozes/github-machine-learning-coursera/exercise7')
source('kmeans-functions.R')

X <- as.matrix(read.csv('input/data2.txt', header=FALSE, sep=' '))
plot(X)




knn(X, 3)

