rm(list=ls())
geometrique<-function(n){
  return(1-(1-1/pi)^n)
}
n<-1
geometrique(n)