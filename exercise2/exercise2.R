rm(list = ls())
setwd('/home/nozes/github-machine-learning-coursera/exercise2')

data <- read.csv2('input/ex2data1.txt', sep = ',' , header = FALSE, stringsAsFactors = FALSE)

plot(data$V1, data$V2, col = data$V3)

# extracting x axis from the dataset
x <- cbind(as.numeric(as.character(data$V1)), as.numeric(as.character(data$V2)))

# appending 1 column (bias)
x <- cbind(rep(1, nrow(x)), x)

# extracting y from the dataset
y <- as.numeric(as.character(data$V3))

# calling gradien descent
theta <- c(0, 0, 0)
costs <- gradientDescent(x, y, theta, 0.001, 1, 100000)
plot(costs)

# setting theta to use the result of gradient descent
thetas <- c(-4.8118, 0.04528064, 0.03819149) ## thetas that I have found without regularization
thetas <- c(-4.56884944, 0.04345014, 0.03628057) ## thetas that I have found using regularization
                                                  ## it seems to work, because the values are very close!


# with this line, I can create a point and test how good
# is my predictor
hypothesis(thetas, rbind(c(1, 40, 80)))

# comparing the prediction for all the dataset with the real result
prediction <- (hypothesis(thetas, x)[1,] > 0.5)
realAnswer <- (y > 0.5)
comparison <- prediction == realAnswer
accuracy <- length(comparison[comparison == TRUE]) / length(comparison)
paste('accuracy: ', accuracy)





tail(((hypothesis(theta, x) - y) %*% x)[1,], -1)
