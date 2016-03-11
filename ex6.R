rm(list=ls())
l<-1000000000
x<-seq(from=-pi, to=pi, length=l)
x<-sin(10*x)
y<-x[x>0]
y<-y/l
sum(y)*2*pi
#Le resultat exact de l'integrale est 2 ce qui corobore nos observations.