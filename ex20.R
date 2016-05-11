rm(list=ls())
fc_tirage <- function(k){
  p<-rep(1/6,6)
  x<-1:6
  tirage<-matrix(sample(x=x,size=2*100,replace=TRUE,prob=p),ncol = 2)
  premier_tirage_bon<-tirage[,1]>=k
  tirage[,1]<-premier_tirage_bon*tirage[,1] #on remplace par 0 tirage 1 si il est inférieur à k
  premier_tirage_bon<-!(premier_tirage_bon)
  return (tirage[,1]+premier_tirage_bon*tirage[,2]) #on renvoie la première colonne avec les deuxièmes tirages si necessaire
}


tirage<-fc_tirage(3)