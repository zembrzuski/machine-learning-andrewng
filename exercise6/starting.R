rm(list = ls())

setwd('/home/nozes/github-machine-learning-coursera/exercise6')

x <- read.csv2('input/xis.txt', sep = ' ' , header = FALSE, stringsAsFactors = FALSE)
x <- cbind(as.numeric(as.character(x[,1])), as.numeric(as.character(x[,2])))


y <- read.csv2('input/ipsilone.txt', sep = ' ' , header = FALSE, stringsAsFactors = TRUE)
y <- as.factor(y[,1])


colors = sapply(y, function(x) {
  if(x == 0) {
    return(2)
  } else {
    return(4)
  }
})

plot(x, col = colors, pch = 19)


library(e1071)
library(rpart)
data(Glass, package="mlbench")
glass <- Glass
index <- 1:nrow(glass)
testIndex <- sample(index, trunc(length(index)/3))
testset <- glass[testIndex,]
trainset <- glass[-testIndex,]




svm.model <- svm(y ~ x, cost = 0.1)
svm.pred <- predict(svm.model, x)
