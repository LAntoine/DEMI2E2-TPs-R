rm(list=ls())

#exemple de methode de rejet pour poisson
lambda=2 #on fixe une valeur arbitraire de lambda
t=1 #on fixe une valeur arbitraire de t
b=0
s=0
n=-1
while (b==0)
{
  n=n+1
  s=s-log(runif(1,0,1))/lambda
  if (s>t)
  {
    b=1
  }
}
print(n)