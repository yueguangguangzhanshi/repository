#onewayanova多组比较&LSD多重组间比较
setwd("C://Users/LHB/Desktop/out/10")
library(agricolae)
library(xlsx)
data <- read.delim("aa.txt", row.names=1)
group=4
A=6
B=6
C=6
D=6
n=group*(group-1)/2+1
type<-factor(c(rep(1,A),rep(2,B),rep(3,C),rep(4,D)))
t=matrix(c(rep(0,nrow(data)*n)),nrow = nrow(data), ncol = n)
for (i in 1:nrow(data)) {
  for (j in 2:n) ifelse(sum(is.na(data[i,1:A]))<=A-2&sum(is.na(data[i,A+1:A+B]))<=B-2&sum(is.na(data[i,(A+B+1):(A+B+C)]))<=C-2&sum(is.na(data[i,(A+B+C+1):(A+B+C+D)]))<=D-2,{x<-aov(as.numeric(data[i,])~type)
  t[i,1]<-summary(x)[[1]][,5][1]
  z<-LSD.test(x,"type",DFerror, MSerror, alpha = 0.05,p.adj = "none",group=F,main = NULL,console=T)
  t[i,j]<-z$comparison[j-1,2]},{t[i,1]<-NA
  t[i,j]<-NA})
}
dimnames(t)=list(rownames(data),c("anova.pvalue",rownames(z$comparison)))
datanew=cbind(data,t)
write.xlsx(datanew,"mnew.xlsx")
                