rm(list=ls())
n <- 20
nA <- 300
nB <-400

fc<-function(n,nA,nB){
  N<-nA+nB
  nnA<-0
  for (i in 1 : n){
    if (nA>0){
      resultat<-sample(x=c(0,1),size=1,replace=TRUE,prob=c(nA/N, nB/N))
      if (resultat==0){
        nA<-nA-1
        nnA<-nnA+1
      }
      else nB<-nB-1
    }
  }
  return (nnA)
}
test<-c()
test[1]<-fc(n,nA,nB)
for(j in 2:1000)
  test[j]<-fc(n,nA,nB)

nb_vainqueur<-0
for(k in 1:1000){
  if(test[k]>n/2)
    nb_vainqueur<-nb_vainqueur+1
}

moyenne<-sum(test)/1000
var<-1/1000*(sum((test-moyenne)^2))


#pour n=50
n <- 50

test2<-c()
test2[1]<-fc(n,nA,nB)
for(j in 2:1000)
  test2[j]<-fc(n,nA,nB)

nb_vainqueur2<-0
for(k in 1:1000){
  if(test2[k]>n/2)
    nb_vainqueur2<-nb_vainqueur2+1
}

moyenne2<-sum(test2)/1000
var2<-1/1000*(sum((test2-moyenne2)^2))

#avec indecis
n <- 20
nA <- 240
nB <-320
nC<-140

fcInd<-function(n,nA,nB,nC){
  N<-nA+nB+nC
  nnA<-0
  nnB<-0
  for (i in 1 : n){
    if (nA>0){
      resultat<-sample(x=c(0,1,2),size=1,replace=TRUE,prob=c(nA/N, nB/N, nC/N))
      if (resultat==0){
        nA<-nA-1
        nnA<-nnA+1
      }
      else if (resultat == 1){ 
        nB<-nB-1
        nnB<-nnB+1
      }
      else nC<-nC-1
    }
  }
  return (c(nnA,nnB))
}

testInd<-matrix(rep(0,2000),ncol=2)
testInd[1,]<-fcInd(n,nA,nB,nC)
for(j in 2:1000)
  testInd[j,]<-fcInd(n,nA,nB,nC)

nb_vainqueurInd<-0
for(k in 1:1000){
  if(testInd[k,1]>testInd[k,2])
    nb_vainqueurInd<-nb_vainqueurInd+1
}

moyenneInd<-sum(testInd[,1])/1000
varInd<-1/1000*(sum((testInd-moyenneInd)^2))