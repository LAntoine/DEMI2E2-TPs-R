rm(list=ls())

fc_dens<-function(x){
  if (x<0 || x>1)
    return (0)
  return (exp(x)/(exp(1)-1))
}
#la fonction de densité ne va jamais au dessus de 1.6
#on utilisera donc une loi uniforme entre 0 et 1
#je me suis inspiré de l'algorithme de Wikipedia
#je vais faire la methode de rejet pour une valeur à la fois pour commencer
#je ne l'ai pas encore testé

fc_methode_rejet<-function(n){
  # f=Ce^x où C=1/(e-1)
  # g=unif(0,1)
    y<-runif(n,0,1)
    u<-runif(n,0,1)
    for (i in 1:n){
      while (u[i]>fc_dens(y[i])/1.6){
        y[i]<-runif(1,0,1)
        u[i]<-runif(1,0,1)
      }
    }  
  return (y)
}

tirage<-fc_methode_rejet(1000)

h<-hist(tirage,breaks=50,plot=TRUE,freq = FALSE)

par(new=TRUE)
x<-seq(from=0,to=1,length=1000)
z<-fc_dens(x)
plot(x,z,type='l')

