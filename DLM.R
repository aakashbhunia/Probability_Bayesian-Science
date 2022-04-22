install.packages("dlm")
library(readr)
library(tidyverse)
library(dlm)

#Note the csv file has been converted into excel and then imported to R
data<- USGDP_1870_2021$`log Real GDP per capita (year 2012 dollars)`

#First Differencing

data.tmp<-diff(log(na.omit(data))*100,lag=1)
head(data.tmp)
data<-ts(data.tmp,start=c(1870,2))
plot.ts(data)
sapply(data,class)
data<-as.matrix(data)

build <- function(parm) {
  dlmModPoly(order = 1, dV = exp(parm[1]), dW =
               exp(parm[2]))}

#Maximum Likelihood to estimate V and W (Parameters)

fit<-dlmMLE(data,parm=c(0,0),build=build,hessian=TRUE)
(fit$convergence)


#Parameter statistics
dlmData<-build(fit$par)
drop(V(dlmData))
drop(W(dlmData))


#Kalman Filtering

filtered<-dlmFilter(data,dlmData)
plot(data,type='o',col="seagreen")
lines(dropFirst(filtered$m),type='o',pch=20,col="brown")

#Plotting results

attach(filtered)
v<-unlist(dlmSvd2var(U.C,D.C))
pl<-dropFirst(m)+qnorm(0.05,sd=sqrt(v[-1]))
pu<-dropFirst(m)+qnorm(0.95,sd=sqrt(v[-1]))
detach()
lines(pl,lty=2,col="brown")
lines(pu,lty=2,col="brown")

#Smoothing/States
smoothing<-dlmSmooth(filtered)
plot(data,type="o",col="seagreen")
attach(smoothing)

lines(dropFirst(s),type="o",pch=20,col="brown")
v<-unlist(dlmSvd2var(U.S,D.S))
pl<-dropFirst(s)+qnorm(0.05,sd=sqrt(v[-1]))
pu<-dropFirst(s)+qnorm(0.95,sd=sqrt(v[-1]))
detach()

lines(pl,lty=2,col="brown")
lines(pu,lty=2,col="brown")



