### preparing data

rm(list = ls())

setwd('/home/nozes/github-machine-learning-coursera/exercise3')

digits = read.csv2('input/data1.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
x <- digits 

y = read.csv2('input/y.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
y <- y$V1

yy = read.csv2('input/yy.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
yy <- yy$V1




### visualizing data
as.numeric(digits[1,])

indexNumber <- 501
digit <- matrix(as.numeric(matrix(digits[indexNumber,], ncol = 20)), ncol=20)
rotate <- function(x) t(apply(x, 2, rev))
image(rotate(digit))





### training logistic regression for digit 0

yResult <- function(y, number) {
  sapply(y, function(x){ 
    if (x == number) {
      return(1)
    } else {
      return(0)
    }
  })
}

xxx <- as.matrix(sapply(x, as.numeric))
thetas0 <- gradientDescent(xxx, yResult(y, 10), rep(0, 400), 0.05, 500, 1)
thetas1 <- gradientDescent(xxx, yResult(y, 1), rep(0, 400), 0.05, 500, 1)
thetas2 <- gradientDescent(xxx, yResult(y, 2), rep(0, 400), 0.05, 500, 1)
thetas3 <- gradientDescent(xxx, yResult(y, 3), rep(0, 400), 0.05, 500, 1)
thetas4 <- gradientDescent(xxx, yResult(y, 4), rep(0, 400), 0.05, 500, 1)
thetas5 <- gradientDescent(xxx, yResult(y, 5), rep(0, 400), 0.05, 500, 1)
thetas6 <- gradientDescent(xxx, yResult(y, 6), rep(0, 400), 0.05, 500, 1)
thetas7 <- gradientDescent(xxx, yResult(y, 7), rep(0, 400), 0.05, 500, 1)
thetas8 <- gradientDescent(xxx, yResult(y, 8), rep(0, 400), 0.05, 500, 1)
thetas9 <- gradientDescent(xxx, yResult(y, 9), rep(0, 400), 0.05, 500, 1)


print('acabou')

thetasMatrix <- rbind(thetas0, thetas1, thetas2, thetas3, thetas4, thetas5, thetas6, thetas7, thetas8, thetas9)




acertou <- 0
errou <- 0

for(i in 1:5000) {
  expected <- yy[i]+1
  res <- hypothesis(thetasMatrix, xxx[i,])[1,]
  
  if (which.max(res) == expected) {
    acertou <- acertou+1
  } else {
    errou <- errou+1
  }
  
}
print(acertou)
print(errou)







#########################

prediction <- (hypothesis(thetasDigitZero, xxx)[1,] > 0.5)

realAnswer <- (y == 1)

comparison <- prediction == realAnswer
accuracy <- length(comparison[comparison == TRUE]) / length(comparison)
paste('accuracy: ', accuracy)







i <- 1
expected <- yy[i]+1
res <- hypothesis(thetasMatrix, xxx[i,])[1,]

