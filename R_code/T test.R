setwd("C://Users/LHB/Desktop/out/7")
dataname="guolili-1.txt"
data <- read.delim(dataname, row.names=1)


a=c(rep(1,nrow(data)))
f<-function(x) sum(is.na(x))
for(i in (1:nrow(data))) {ifelse(apply(data,1,f)[i]<=(ncol(data)-3),
                                 a[i]<-shapiro.test(as.numeric(data[i,]))$p.value,
                                 a[i]<-NA)}


b=c(rep(1,nrow(data)))

group1=8
group2=7
g=c(rep(1,group1),rep(2,group2))
for(i in (1:nrow(data))) {ifelse(a[i]>=0.05&&sum(is.na(data[i,1:group1]))<(group1-1)&&sum(is.na(data[i,(group1+1):(group1+group2)]))<(group2-1),
                                 b[i]<-bartlett.test(as.numeric(data[i,]),g)$p.value,
                                 b[i]<-NA)}

#t-test
c=c(rep(1,nrow(data)))
for(i in (1:nrow(data)))  ifelse(b[i]>=0.05&!is.na(b[i]),
                                c[i]<-t.test(as.numeric(data[i,])~g,var.equal=TRUE)$p.value,
                                ifelse(is.na(b[i]),
                                       c[i]<-NA,
                                       c[i]<-t.test(as.numeric(data[i,])~g,var.equal=FALSE)$p.value))

#?ǲμ???
d=c(rep(1,nrow(data)))
for(i in (1:nrow(data)))  ifelse(is.na(c[i])&&sum(is.na(data[i,1:group1]))<(group1-1)&&sum(is.na(data[i,(group1+1):(group1+group2)]))<(group2-1),
                                  d[i]<-wilcox.test(as.numeric(data[i,])~g)$p.value,
                                  d[i]<-NA)


#???ݽ???
e=c(rep(1,nrow(data)))
{for(i in (1:nrow(data)))  ifelse(is.na(d[i]),e[i]<-c[i],e[i]<-d[i])
data$normal_pvalue<-a 
data$df_pvalue<-b
data$ttest_pvalue<-c
data$mann_pvalue<-d
data$pvalue<-e
}

#????????
library(xlsx)
write.xlsx(data,"guolili1.xlsx")
