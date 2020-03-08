setwd("C://Users/LHB/Desktop/out/9")
dataname="guolili1_ms.txt"
var=c(rep(1,10),rep(2,10))
datatest <- read.delim(dataname, row.names=1)
a=matrix(c(rep(1,nrow(datatest)*ncol(datatest))),nrow=nrow(datatest),ncol=ncol(datatest))
rownames(a)=rownames(datatest)
colnames(a)=colnames(datatest)
for (i in 1:nrow(datatest)) {
  for (j in 1:ncol(datatest)) {
    ifelse(datatest[i,j]=="",a[i,j]<-0,a[i,j]<-1)
    
  }
  
}

