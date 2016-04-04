rm(list=ls())

fc_dens<-function(x){
  if (x<0 || x>1)
    return (0)
  return (exp(x)/(exp(1)-1))
}

#je me suis inspiré de l'algorithme de Wikipedia
#je vais faire la methode de rejet pour une valeur à la fois pour commencer
#je ne l'ai pas encore testé

# f=Ce^x où C=1/(e-1)
# g=unif(0,1)
b = TRUE
while (b) {
  y<-runif(1,0,1)
  u<-runif(1,0,1)
  if (u<=f(y)/y){
    b<-FALSE
  }
}