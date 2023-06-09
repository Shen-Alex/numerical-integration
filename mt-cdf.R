
set.seed(1)
x <- rnorm(10^8)
bound <- qnorm(c(0.5,0.75,0.8,0.9,0.95,0.99,0.999,0.9999))
res <- matrix(0, ncol =8, nrow = 7)
for (i in 2:8){
  for (j in 1:8){
    res[i-1, j] = mean(x[1:10^i] < bound[j])
  }
}