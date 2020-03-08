library(openxlsx)
library(psych)
library(pheatmap)
data1<-read.xlsx('correlation.xlsx',sheet=1,rowNames = TRUE)
data2<-data1
result<-corr.test(data1,data2,method = "pearson",adjust="none",alpha=.05)
rmatrix<-result$r
pmatrix<-result$p
pheatmap(rmatrix,
         cluster_rows=FALSE,cluster_cols=FALSE,
         scale = "none", 
         color =colorRampPalette(c("blue", "white","red"))(1000),
         show_rownames=TRUE,show_colnames=TRUE,
         cellwidth =30,cellheight=30,
         border_color =NA,fontsize =10,
         legend_breaks = NA,legend = TRUE,main="Correlation Heatmap-pearson")
write.xlsx(rmatrix,"pearson_correlation.xlsx",rowNames=TRUE)
write.xlsx(pmatrix,"pearson_correlation_p.xlsx",rowNames=TRUE)
