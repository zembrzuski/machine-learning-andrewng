rm(list=ls())

### preparing data
input <- cbind(
  c(0, 0),
  c(0, 1),
  c(1, 0),
  c(1, 1)
)

input <- addBiasTherm(input)
output = rbind(c(1, 0, 0, 0))
y = output 

theta1 <- rbind(
  c(-30, 20, 20), 
  c(-10, 20, 20)
)

theta2 <- rbind(c(10, -20, -20))



### forward propagation

a1 = input
a2 = addBiasTherm(hypothesis(theta1, a1))
a3 = hypothesis(theta2, a2)


### backpropagation 

# calculating error
##### TODO fazer essa coisa aqui: (a2 * (1-a2)) : virar uma funcao sigmoidGradient
error3 = a3 - y
error2 = t(theta2) %*% error3 * (a2 * (1-a2))

### derivative of theta

### calculating derivative using for, because I am stupid.


for(i in 1:nrow(theta1)) {
  for(j in 1:ncol(theta1)) {
    print(a1[,j] * error2[i, ])
  }
}


i = 1
j = 2

delta_2 = error3 %*% t(a2)
delta_1 = error2 %*% t(a1)

m <- ncol(input)
DER_2 <- delta_2 / m
DER_1 <- delta_1 / m 


theta1

