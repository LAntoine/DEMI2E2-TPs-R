rm(list=ls())
P_2_birthday<-function(n){
  if(n<2){
    return (0)
  }
  if(n>365){
    return (1)
  }
  return (1- (prod((365-n+1):365)) /365^n)
}

p<-1:365
for(i in p){
  p[i]<-P_2_birthday(i)
}

i<-2
while (i<=365 && p[i]<=0.5){i=i+1}

print(i)
plot(1:365,p,type='l')