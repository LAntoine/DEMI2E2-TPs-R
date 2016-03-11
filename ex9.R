rm(list=ls())
n<-8
y<-4:n
y<-(factorial(n)/(factorial(n-y)*factorial(y)))*((1/pi)^y)*((1-1/pi)^(n-y))
sum(y)