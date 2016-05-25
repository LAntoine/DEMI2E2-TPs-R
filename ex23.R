rm(list=ls())
theta <- rnorm(1)

#question 1
alpha <- qnorm(0.025)
beta <- qnorm(0.975)

#question 2/3
X <- matrix(rnorm(10000, mean = theta),nrow=100)
c <- 0
for (i in 1:100){
  if(theta >= (alpha/sqrt(100) + (1/100)*(sum(X[i,]))) && theta <= ((1/100)*sum(X[i,]) + beta/sqrt(100)))
     c <- c+1
}