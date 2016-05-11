rm(list=ls())
fc_tirage <- function(k){
  p<-rep(1/6,6)
  x<-1:6
  tirage<-matrix(sample(x=x,size=2*100,replace=TRUE,prob=p),ncol = 2)
  for (i in 1:100){
    if (tirage[i,1]>=k) tirage[i,2]<-tirage[i,1]
  }
  return (tirage[,2])
}

tirage<-fc_tirage(3)
tirage
