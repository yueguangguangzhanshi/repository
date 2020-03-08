library(lattice)
library(MASS)
library(nnet)
library(mice)
library(VIM)
setwd("C://Users/LHB/Desktop/out/6")
dataname="test.txt"
inputfile=read.delim(dataname, row.names=1)

#mice
imp=mice(inputfile,m=50,seed = 20000)
fit=with(imp,lm(A1~B9,data=inputfile))
pooled=pool(fit)
summary(pooled)
result=complete(imp,action=1)
c=md.pattern(result)

#KNN(k-Nearest Neighbour Imputation)
datanew=kNN(inputfile, variable = c("NonD","Gest","Dream"))