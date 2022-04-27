library(dplyr)
library(readr)
library(tidyverse)
library(MSwM)

data<-realhomeprices$Index
data<-as.matrix(data)
     
#converting data into monthly percent change
data<-diff(data)/data[-1]
data<-data*100

hist(data)

ninf<-length(data)

y0<-data[2:ninf]
y1<-data[1:(ninf-1)]

#Creating a data frame with y and its lagged variable
df<-data.frame(y=y0,y1=y1)
mod<-lm(y~y1,df)
summary(mod)

#Find the number of states using histogram
hist(y0)

#creating a 2 state Markov switching model
k<-2

#2 variables and 1 element for volatility
mv<-3

#Fitting Hamilton Regime Switch Model
rsm=msmFit(mod,k=1,p=0,sw=rep(TRUE,mv),control=list(parallel=FALSE))
summary(rsm)
plotProb(rsm,which=2)
plotReg(rsm,exp="y1")

