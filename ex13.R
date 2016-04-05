rm(list=ls())
fc_dens<-function(x){
  if (x<0 || x>2)
    return (0)
  return (1.5/(1+x)^2)
}

fc_rep_inv<-function(y){
  return (-3/(2*y-3)-1)
}

y<-runif(1000)
tirage<-fc_rep_inv(y)

h<-hist(tirage,breaks=50,plot=TRUE,freq = FALSE, ylim = c(0,2), xlim = c(0,2))

par(new=TRUE)

x<-seq(from=0,to=2,length=1000)
z<-fc_dens(x)

plot(x,z,type='l', ylim = c(0,2), xlim = c(0,2), xlab = "", ylab="")