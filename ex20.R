rm(list=ls())
fc_tirage1 <- function(k){
  p<-rep(1/6,6)
  x<-1:6
  tirage<-matrix(sample(x=x,size=2*100,replace=TRUE,prob=p),ncol = 2)
  premier_tirage_bon<-tirage[,1]>=k
  tirage[,1]<-premier_tirage_bon*tirage[,1] #on remplace par 0 tirage 1 si il est inférieur à k
  premier_tirage_bon<-!(premier_tirage_bon)
  return (tirage[,1]+premier_tirage_bon*tirage[,2]) #on renvoie la première colonne avec les deuxièmes tirages si necessaire
}

for (k in 2:6){
  tirage1<-fc_tirage1(k)
  abscise<-seq(from=0.5,to=6.5, length=7)
  hist(tirage1,abscise, freq = TRUE, plot = TRUE, main = k)
}

#question2

fc_tirage2 <- function(k1,k2){
  p<-rep(1/6,6)
  x<-1:6
  tirage<-matrix(sample(x=x,size=3*100,replace=TRUE,prob=p),ncol = 3)
  tirage_bon<-tirage[,1]>=k1
  tirage[,1]<-tirage_bon*tirage[,1] #on remplace par 0 tirage 1 si il est inférieur à k
  tirage_bon<-!(tirage_bon)
  tirage[,1]<-tirage[,1]+tirage_bon*tirage[,2] #on renvoie la première colonne avec les deuxièmes tirages si necessaire
  tirage_bon<-tirage[,1]>=k2
  tirage[,1]<-tirage_bon*tirage[,1]
  tirage_bon<-!(tirage_bon)
  return (tirage[,1]+tirage_bon*tirage[,3])
}

moyennes<-cbind(rep(2:6,each=5),rep(2:6,5),rep(0,25))

for (i in 1:25){
  moyennes[i,3]<-mean(fc_tirage2(moyennes[i,1],moyennes[i,2]))
  
}