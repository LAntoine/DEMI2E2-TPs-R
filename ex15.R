rm(list=ls())
x<-rcauchy(1000)
n<-1:1000
u<-cumsum(x)/n
plot(n,u,type='l')