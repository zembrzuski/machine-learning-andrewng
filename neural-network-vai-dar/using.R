### 
### loading data
###
setwd('/home/nozes/github-machine-learning-coursera/neural-network-vai-dar')
x = read.csv2('input/data1.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
x <- as.matrix(sapply(x, as.numeric))

y = read.csv2('input/y.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)$V1
y <- as.matrix(sapply(y, as.numeric))

theta1 = read.csv2('input/theta1.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)
theta2 = read.csv2('input/theta2.txt', sep=',', stringsAsFactors = FALSE, header = FALSE)

theta1 <- as.matrix(sapply(theta1, as.numeric))
theta2 <- as.matrix(sapply(theta2, as.numeric))



# implementing forward propagation.
# this is the classical approach that makes sense to me.
# each point is a row
a1 <- addBiasThermFirstColumn(x)
a2 <- addBiasThermFirstColumn(hypothesis(theta1, a1))
a3 <- hypothesis(theta2, a2)

yMatrix <- createResultMatrix(y)

print(costFunction(a3, yMatrix))

print(costFunctionWithRegularization(a3, yMatrix, theta1, theta2, 1))






### recomecar calculando os erros
error3 = a3 - yMatrix
error2 = error3 %*% theta2 * (a2 * (1-a2))
#error1 = error2 %*% theta1 * (a1 * (1-a1)) it does not exist !

delta1 = t(error2) %*% a1
delta2 = t(error3) %*% a2

der2 <- delta2/5000
der1 <- delta1/5000
der1 <- removeBiasTherm(der1)

### acho que até aqui (inclusive a parte de erros e deltas, está ok)






### sim, está ok, porque eu verifiquei de acordo com o meu grandeoso gradient-checking
epsilon <- 0.001
xIndex <- 7
yIndex <- 3

theta2PrimeiraParte <- theta2
theta2SegundaParte <- theta2

theta2PrimeiraParte[xIndex, yIndex] <- theta2PrimeiraParte[xIndex, yIndex] + epsilon
theta2SegundaParte[xIndex, yIndex] <- theta2SegundaParte[xIndex, yIndex] - epsilon

resultPrimeiraParte <- hypothesis(theta2PrimeiraParte, a2)
resultSegundaParte  <- hypothesis(theta2SegundaParte, a2)

(costFunction(resultPrimeiraParte,  yMatrix) - costFunction(resultSegundaParte, yMatrix)) / (2*epsilon)
der2[xIndex, yIndex]


### teste mais sinistro agora, mudando um theta da primeira layer para ver se funciona.

epsilon <- 0.001
xIndex <- 3
yIndex <- 4

thetaPrimeiraParte <- theta1
thetaSegundaParte <- theta1

thetaPrimeiraParte[xIndex, yIndex] <- thetaPrimeiraParte[xIndex, yIndex] + epsilon
thetaSegundaParte[xIndex, yIndex] <- thetaSegundaParte[xIndex, yIndex] - epsilon

resultPrimeiraParte <- hypothesis(thetaPrimeiraParte, a1)
resultPrimeiraParte <- addBiasThermFirstColumn(resultPrimeiraParte)
resultPrimeiraParte <- hypothesis(theta2, resultPrimeiraParte)

resultSegundaParte  <- hypothesis(thetaSegundaParte, a1)
resultSegundaParte <- addBiasThermFirstColumn(resultSegundaParte)
resultSegundaParte <- hypothesis(theta2, resultSegundaParte)

(costFunction(resultPrimeiraParte,  yMatrix) - costFunction(resultSegundaParte, yMatrix)) / (2*epsilon)
der1[xIndex, yIndex]





