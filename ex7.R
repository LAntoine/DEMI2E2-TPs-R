rm(list=ls())
poisson<-function(n){
  y<-0:(n-1)
  y<-(pi^y/factorial(y))*exp(-pi)
  return(1-sum(y))
}
n<-5
poisson(n)