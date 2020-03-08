## quantile normal
library(reshape2)
data1<-read.xlsx("data.xlsx",1)
data2<-read.xlsx("data.xlsx",2)
data3<-read.xlsx("data.xlsx",3)
data4<-read.xlsx("data.xlsx",4)
data5<-read.xlsx("data.xlsx",5)
data6<-read.xlsx("data.xlsx",6)
reshape_data1<-dcast(data1,Name~Sample.ID,mean)
reshape_data2<-dcast(data2,Name~Sample.ID,mean)
reshape_data3<-dcast(data3,Name~Sample.ID,mean)
reshape_data4<-dcast(data4,Name~Sample.ID,mean)
reshape_data5<-dcast(data5,Name~Sample.ID,mean)
reshape_data6<-dcast(data6,Name~Sample.ID,mean)
xlsx::write.xlsx(reshape_data1,"reshape data.xlsx",sheetName="532")
xlsx::write.xlsx(reshape_data2,"reshape data.xlsx",append = T,sheetName="532-F")
xlsx::write.xlsx(reshape_data3,"reshape data.xlsx",append = T,sheetName="532-B")
xlsx::write.xlsx(reshape_data4,"reshape data.xlsx",append = T,sheetName="635")
xlsx::write.xlsx(reshape_data5,"reshape data.xlsx",append = T,sheetName="635-F")
xlsx::write.xlsx(reshape_data6,"reshape data.xlsx",append = T,sheetName="635-B")

lgG<-reshape_data1[-36]
lgM<-reshape_data4
lgG<-column_to_rownames(lgG,1)[-1,]
lgM<-column_to_rownames(lgM,1)[-1,]
lgG[lgG<0]<-NA
lgM[lgM<0]<-NA
rownames(lgG)<-lapply(rownames(lgG),FUN =function(x) {
  gsub("-","_",x)
})
rownames(lgM)<-lapply(rownames(lgM),FUN =function(x) {
  gsub("-","_",x)
})
Process = preProcess(t(lgG),method = c("bagImpute"))
pre_IgG = predict(Process, t(lgG))
Process = preProcess(lgM,method = c("medianImpute"))
pre_IgM = predict(Process, lgM)

nor_lgG<-normalizeQuantiles(t(pre_IgG))
nor_lgM<-normalizeQuantiles(pre_IgM)

write.xlsx(nor_lgG,"lgG.xlsx",rowNames=T)
write.xlsx(nor_lgM,"lgM.xlsx",rowNames=T)

log_lgG<-read.xlsx("lgG.xlsx",2,rowNames=T)
log_lgM<-read.xlsx("lgM.xlsx",2,rowNames=T)

type<-factor(c(rep('A',6),rep('B',6),rep('C',7),rep('D',10),rep('E',4),rep('F',14)))
design <- model.matrix(~0+type)
colnames(design) <- levels(type)
rownames(design) <- colnames(log_lgG)
cont.matrix=makeContrasts(B-A,C-A,D-A,E-A,F-A,levels = design)
## Linear Models fitting and Empirical Bayes test
fit <- lmFit(log_lgG, design)
fit2=contrasts.fit(fit,cont.matrix)
fit2 <- eBayes(fit2, trend=TRUE)
result_limma <- topTable(fit2,n=Inf)
