rm(list=ls())

#question 1
fc_tirage1<-function(P){#P=#1=enfant, 2=instituteur, 3=direteur, 4=hazard
  pE=c(0.7,0.2,0.1)
  pI=c(0.5,0.3,0.2)
  pD=c(0.3,0.6,0.1)
  position<- P #1=enfant, 2=instituteur, 3=direteur
  if (position==4){
    position<-sample(x=c(1,2,3),size=1,replace=TRUE,prob=rep(1/3, 3))
  }
  E<-0
  I<-0
  D<-0
  
  for (i in 1:100){
    if (position==1){E<-E+1}
    else if (position==2){I<-I+1}
    else {D<-D+1}
    
    if (position==1){
      position<-sample(x=c(1,2,3),size=1,replace=TRUE,prob=pE)
    }
    else if (position==2){
      position<-sample(x=c(1,2,3),size=1,replace=TRUE,prob=pI)
    }
    else{#position=3
      position<-sample(x=c(1,2,3),size=1,replace=TRUE,prob=pD)
    }
  }
  return (c(E/100,I/100,D/100))
}

fc_tirage1(1) #P=1=enfant, 2=instituteur, 3=direteur, 4=hazard

#question 2
fc_tirage2<-function(P,n){#P=1=enfant, 2=instituteur, 3=direteur, 4=hazard
  pE=c(0.7,0.2,0.1)
  pI=c(0.5,0.3,0.2)
  pD=c(0.3,0.6,0.1)
  position<- P #1=enfant, 2=instituteur, 3=direteur
  if (position==4){
    position<-rep(sample(x=c(1,2,3),size=1,replace=TRUE,prob=rep(1/3, 3)),n)
  }
  for (i in 2:100){
    pou_sur_enfant<-position==1
    pou_sur_instituteur<-position==2
    pou_sur_directeur<-position==3
    
    position<-sample(x=c(1,2,3),size=n,replace=TRUE,prob=pE)*pou_sur_enfant
    position<-position+sample(x=c(1,2,3),size=n,replace=TRUE,prob=pI)*pou_sur_instituteur
    position<-position+sample(x=c(1,2,3),size=n,replace=TRUE,prob=pD)*pou_sur_directeur
  }
  return (position)
}

fc_comptage<-function(positions,n){
  E<-0
  I<-0
  D<-0
  for (i in positions){#1=enfant, 2=instituteur, 3=direteur
    if (i==1){E<-E+1}
    else if (i==2){I<-I+1}
    else {D<-D+1}
  }
  return(c(E/n,I/n,D/n))
}

fc_comptage(fc_tirage2(1,100),100)

#question 3
fc_tirage1(2)
fc_comptage(fc_tirage2(2,100),100)

#question 4
fc_tirage1(4)
fc_comptage(fc_tirage2(4,100),100)