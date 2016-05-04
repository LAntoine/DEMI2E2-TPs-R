rm(list=ls())
S<-c()
S[1]<-1
urne<-c()
urne[1]<-3 #3=red, 5=green
urne[2]<-5
S20j<-c()
for (a in 1:1000){
  for (i in 2:20){
    urne[i+1]<-sample(x=urne[1:i],prob=rep(1/(i+1),i), size=1)
    if(urne[i+1]==3)
      S[i]<-S[i-1]+1
    else
      S[i]<-S[i-1]
  }
  S20j[a]<-S[20]
}
plot(1:20,S, ylim = c(0,20),type='l')
abscise<-seq(from=0.5,to=20.5, length=21)
hist(S20j,abscise, freq = TRUE, plot = TRUE)