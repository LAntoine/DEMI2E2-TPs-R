rm(list=ls())
tirage1<-matrix(rnorm(2*50), ncol = 2)
plot(tirage1[,1],tirage1[,2],ylim = c(-3,3), xlim = c(-3,3), main = "sigma1")


sigma2<-matrix(c(2,1,1,2),ncol=2)
A<-t(chol(sigma2))
tirage2<-t(A%*%t(tirage1))
plot(tirage2[,1],tirage2[,2],ylim = c(-3,3), xlim = c(-3,3), main = "sigma2")

fc<-function(epsilon){
sigma3<-matrix(c(4+epsilon,2,2,1+epsilon),ncol=2)
A<-t(chol(sigma3))
tirage3<-t(A%*%t(tirage1))
plot(tirage3[,1],tirage3[,2], ylim = c(-3,3), xlim = c(-3,3), main = epsilon)
}

fc(1)
fc(0.1)
fc(0.01)