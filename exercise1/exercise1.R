setwd('/home/nozes/github-machine-learning-coursera/exercise1')

data <- read.csv2('input/ex1data1.txt', sep = ',' , header = FALSE, stringsAsFactors = FALSE)

plot(data$V1, data$V2)

# example using hypothesis
# hypothesis(c(1, 2), c(3, 4))
hypothesis(c(1, 2), rbind(c(2, 3), c(5, 6)))



# example using cost function for linear regression
x <- rbind(c(1, 1), c(1, 2), c(1, 3.5))
y <- c(1, 2, 3)
theta <- c(0, 1)
cost(x, y, theta)


# example using gradient descent
x <- rbind(c(1, 1), c(1, 2), c(1, 3.5), c(1, 4.5))
y <- c(2, 4, 6, 8)
theta <- c(0, 0)
cost <- gradientDescent(x, y, theta, 0.001, 10000)
plot(cost)
