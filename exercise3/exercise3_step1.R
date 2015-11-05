### preparing data

rm(list = ls())

setwd('/home/nozes/github-machine-learning-coursera/exercise3')

digits = read.csv2('input/data1.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
x <- digits 

y = read.csv2('input/y.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
y <- y$V1





### visualizing data
as.numeric(digits[1,])

indexNumber <- 501
digit <- matrix(as.numeric(matrix(digits[indexNumber,], ncol = 20)), ncol=20)
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(digit))





### training logistic regression for digit 0
yResult <- sapply(y, function(x){ 
  if (x == 10) {
    return(1)
  } else {
    return(0)
  }
})


theta <- rep(0, 400)
lambda <- 1

xxx <- as.matrix(sapply(x, as.numeric))
class(xxx[1,1])

length(xxx[1,])
length(yResult)

thetasDigitZero <- gradientDescent(xxx, yResult, theta, 0.05, 50, lambda)

prediction <- (hypothesis(thetasDigitZero, xxx)[1,] > 0.6)

realAnswer <- (y == 1)

comparison <- prediction == realAnswer
accuracy <- length(comparison[comparison == TRUE]) / length(comparison)
paste('accuracy: ', accuracy)

