rm(list=ls())
x<-matrix(rpois(500*500,1),ncol=500)
y<-matrix(runif(500*500),ncol=500)
n<-500

mxjn<-rowSums(x)/n
myjn<-rowSums(y)/n

txjn<-sqrt(n)*(mxjn-1)
tyjn<-sqrt(n)*((myjn-1/2)/sqrt(1/12))

h1<-hist(txjn,col="red")
h2<-hist(tyjn,col="cyan")

z<-seq(from=-3,to=3,length=500)
w<-dnorm(z)
plot(z,w,type="l",col="black")

p<-5
mxj5<-rowSums(x[1:5,])/p
myj5<-rowSums(y[1:5,])/p

txj5<-sqrt(p)*(mxj5-1)
tyj5<-sqrt(p)*((myj5-1/2)/sqrt(1/12))

h1<-hist(txj5,col="orange")
h2<-hist(tyj5,col="pink")

plot(z,w,type="l",col="black")