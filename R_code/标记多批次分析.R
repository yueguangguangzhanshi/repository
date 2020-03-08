setwd("C://Users/LHB/Desktop/out/7")

#按照蛋白名整合数据
library(sqldf)
exp<-xyz
data1=sqldf("select ProteinGroupAccessions,sum(X113) from exp group by ProteinGroupAccessions")
data2=sqldf("select ProteinGroupAccessions,sum(X114) from exp group by ProteinGroupAccessions")
data3=sqldf("select ProteinGroupAccessions,sum(X115) from exp group by ProteinGroupAccessions")
data4=sqldf("select ProteinGroupAccessions,sum(X116) from exp group by ProteinGroupAccessions")
data5=sqldf("select ProteinGroupAccessions,sum(X117) from exp group by ProteinGroupAccessions")
data6=sqldf("select ProteinGroupAccessions,sum(X118) from exp group by ProteinGroupAccessions")
data7=sqldf("select ProteinGroupAccessions,sum(X119) from exp group by ProteinGroupAccessions")
B=cbind(data1,data2,data3,data4,data5,data6,data7)
B2=B[,!duplicated(t(B))]
write.table ( B2, file ="B.txt", sep ="\t", row.names =TRUE, col.names =TRUE)

#组间比较
library(edgeR)
data <-  read.xlsx("xyz2.xlsx",sheetIndex = 1)
row.names(data)=data$ProteinGroupAccessions
data=data[,-1]
group=factor(c(1,1,1,1,1,1,2,2,2,2,2,2))
y1 = DGEList(counts=data, group=group)
y2 <- calcNormFactors(y1)
plotMDS(y2)
y3 <- estimateCommonDisp(y2, verbose=TRUE)
y4 <- estimateTagwiseDisp(y3)
plotBCV(y4)

#精确二分类检验
et <- exactTest(y4)
top <- topTags(et)
detags <- rownames(y4)
plotSmear(lrt, de.tags=detags)
c<-log(2,2)
abline(h=c(-c, c), col="red")
write.table(et$table,file="B_vs_A_result.txt",sep = " ")

#广义线性模型+最大似然检验
design<-model.matrix(~group)
fit<-glmFit(y4, design)
lrt<-glmLRT(fit)
dt<-decideTestsDGE(lrt,p.value=0.00000000000000000000000000000000000001,lfc=0)
isDE<-as.logical(dt)
DEnames<-rownames(y4)[isDE]
