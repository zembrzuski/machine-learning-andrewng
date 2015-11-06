#rm(list=ls())
setwd('/home/nozes/github-machine-learning-coursera/exercise2')

data <- read.csv2('input/ex2data2.txt', sep = ',' , header = FALSE, stringsAsFactors = FALSE)

plot(data$V1, data$V2, col = data$V3)

# extracting x axis from the dataset
x <- cbind(as.numeric(as.character(data$V1)), as.numeric(as.character(data$V2)))

# appending 1 column (bias)
x <- cbind(rep(1, nrow(x)), x)

# extracting y from the dataset
y <- as.numeric(as.character(data$V3))
length(y)

x <- mapFeature(x)
theta <- rep(0, 28)
lambda <- 1
print(cost(x, y, theta, lambda))

costs <- gradientDescent(x, y, theta, 0.05, 40000, lambda)
plot(costs)

thetasAnswer <- c(
    1.27273951,  0.62527180,  1.18108869, -2.01996086, -0.91742375, -1.43166444,  0.12400635, -0.36553437, -0.35723962, -0.17513048, -1.45815646,
   -0.05098906, -0.61555504, -0.27470631, -1.19281652, -0.24218823, -0.20600609, -0.04473075, -0.27778450, -0.29537810, -0.45635749, -1.04320249,
    0.02777171, -0.29243131,  0.01556680, -0.32737959, -0.14388693, -0.92465257
)

prediction <- (hypothesis(thetasAnswer, x)[1,] > 0.5)
realAnswer <- (y > 0.5)

comparison <- prediction == realAnswer
accuracy <- length(comparison[comparison == TRUE]) / length(comparison)
paste('accuracy: ', accuracy)

