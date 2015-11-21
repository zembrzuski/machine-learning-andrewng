## based on the video lecture

## I am trying to compose some logistic regressions to make my
## first neural network.


rm(list = ls())

input <- rbind(
  c(0, 0),
  c(0, 1),
  c(1, 0),
  c(1, 1)
)

input <- addBiasTherm(input)


## first layer -> and + or

thetasFirstLayer <- rbind(
  c(-30, 20, 20), 
  c(-10, 20, 20)
)


## second layer -> xnor
thetasSecondLayer <- rbind(c(10, -20, -20))



## na regressao logistica, no gradient descent, o que eu faço
## é ir calculando a derivada de cada theta, que está num vetor
## lá, eu calculo a derivada de theta1, theta2, theta3, thetan.
## agora, na primeira layer, eu tenho uma matriz de thetas, e vou
## ter que calcular a derivada de cada item da matriz, ou theta
## theta11, theta12, theta1n, theta21, theta22, theta22, e assim por diante
## isso eh o que o professor chama de thetaij
## alem disso, tenho que repetir esse procedimento para cada uma das
## layers, que é o l dependurado.
## ou seja, isso eu já entendi.

## implementing forward propagation: 

## propagation for the first layer
outputLayerOne <- hypothesis(input, thetasFirstLayer)

## propagation for the second layer
inputLayerTwo <- addBiasTherm(outputLayerOne)
output <- hypothesis(inputLayerTwo, thetasSecondLayer)


## até agora, implementei a solucao para binario.
## talvez eu tenha que implementar a solucao para multiclass
## mas vou deixar para depois.


## agora estou aplicando a funcao de custo (sem regularizacao)
## para ver se funciona.
## ela deve se aplicar da seguinte forma: faço uma forward
## propagation, daí tenho o input da última layer. minha
## funcao de custo aqui tá funcionando porque só tem uma classificação,
## eu acho.
cost <- cost(inputLayerTwo, y, thetasSecondLayer)

