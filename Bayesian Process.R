library(LearnBayes)
library(ggplot2)
library(dplyr)
library(mcmc)
library(mosaicCalc)

#Create values
theta<-seq(0,by=0.1,1.0)

#Likelihood
likelihood<-dbinom(x=2,prob = theta,size = 5)
plot(theta,likelihood,type='l',xlab="P(success)",ylab="Density",col="red")
sum(likelihood)

#Prior
k=1/11
Prior = c(k,k,k,k,k,k,k,k,k,k,k)
sum(Prior)  #Sum of uniform prior probabilities

lines(theta,Prior,col="blue")

#Kernel
Kernel=likelihood*Prior
lines(theta,Kernel,col="black")
sum(Kernel)

#Posterior
Posterior<-Kernel/sum(Kernel)
sum(Posterior)

lines(theta,Posterior,col="purple")
legend("topleft",legend = c("likelihood","Kernel","Prior","Posterior"),text.col = 1:4,bty="n")

