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



mapFeature <- cbind(
  rep(1, nrow(x)),
  x[,2],
  x[,3],
  x[,2]^2,
  x[,2]*x[,3],
  x[,3]^2,
  x[,2]^3,
  x[,2]^2*x[,3],
  x[,2]*x[,3]^2,
  x[,3]^3,
  x[,2]^4,
  x[,2]^3*x[,3],
  x[,2]^2*x[,3]^2,
  x[,2]^1*x[,3]^3,
  x[,3]^4,
  x[,2]^5,
  x[,2]^4*x[,3],
  x[,2]^3*x[,3]^2,
  x[,2]^2*x[,3]^3,
  x[,2]^1*x[,3]^4,
  x[,3]^5,
  x[,2]^6,
  x[,2]^5*x[,3]^1,
  x[,2]^4*x[,3]^2,
  x[,2]^3*x[,3]^3,
  x[,2]^2*x[,3]^4,
  x[,2]^1*x[,3]^5,
  x[,3]^6
)



x <- mapFeature
nrow(x)
length(y)
theta <- rep(0, 28)
lambda <- 1
print(cost(x, y, theta, lambda))



xxx <- gradientDescent(x, y, theta, 0.00001, lambda, 5000)
plot(xxx)
