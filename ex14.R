rm(list=ls())

# f=Ce^x où C=1/(e-1)
fc_dens<-function(x){
  if (x<0 || x>1)
    return (0)
  return (exp(x)/(exp(1)-1))
}

#je me suis inspiré de l'algorithme de Wikipedia

fc_methode_rejet<-function(n){
  # la fonction de densité ne va jamais au dessus de 1.6
  # je prend donc g=unif(0,1) et c = 1.6
  # ainsi j'ai : pour tout x f(x)<=c*g(x)
  y<-runif(n,0,1)
  u<-runif(n,0,1)
  for (i in 1:n){
    while (u[i]*1.6>fc_dens(y[i])){
      y[i]<-runif(1,0,1)
      u[i]<-runif(1,0,1)
    }
  }
  return (y)
}

tirage<-fc_methode_rejet(10^3)
h<-hist(tirage,breaks=50,plot=TRUE,freq = FALSE, ylim = c(0,2), xlim = c(0,1))
par(new=TRUE)
x<-seq(from=0,to=1,length=1000)
z<-fc_dens(x)
plot(x,z,type='l', ylim = c(0,2), xlim = c(0,1))