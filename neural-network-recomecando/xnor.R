### x1 or x2 
thetas <- rbind(c(10, -20, -20))

input <- rbind(
  c(0, 0),
  c(0, 1),
  c(1, 0),
  c(1, 1)
)

input <- addBiasTherm(input)

result <- hypothesis(input, thetas)

result > 0.9
result < 0.1
