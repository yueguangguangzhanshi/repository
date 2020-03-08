setwd("C://Users/LHB/Desktop/out/10")
data <- read.delim("2 groups.txt", row.names=1)
Pvalue<-c(rep(0,nrow(data)))
A=12
B=12
type<-factor(c(rep(1,A),rep(2,B)))
for(i in 1:nrow(data)){
  x<-wilcox.test(as.numeric(data[i,])~type)
  Pvalue[i]<-x$p.value
}
f<-function(x) mean(x,na.rm = TRUE)
Average_A=apply(data[,1:12],1,f)
Average_B=apply(data[,12:24],1,f)
FC<-c(rep(0,nrow(data)))
for (i in 1:nrow(data)) FC[i]<-Average_B[i]/Average_A[i]
data$p_value=Pvalue
data$mean_A=Average_A
data$mean_B=Average_B
data$FC_B_vs_A=FC
library(xlsx)
write.xlsx(data,"2 group.xlsx")
