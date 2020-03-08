library("limma", lib.loc="~/R/win-library/3.4")
groups<-c(rep("A",63),rep("B",163),rep("C",11))
exprSet=newdata
design <- model.matrix(~0+factor(groups))
colnames(design)=levels(factor(groups))
rownames(design)=colnames(exprSet)
contrast.matrix<-makeContrasts(paste0(unique(groups),collapse = "-"),levels = design)
fit <- lmFit(exprSet,design)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)
tempOutput = topTable(fit2, coef=1, n=Inf)
nrDEG = na.omit(tempOutput)
head(nrDEG)
write.csv(nrDEG,file="result.csv")


#ÈÈÍ¼
pheatmap(`237sample.3subtype.surface.log2`[1:100,1:30],cluster_rows=T,cluster_cols=T,fontsize_row =7,fontsize_col = 7,main="Heatmap")
pheatmap(`237sample.3subtype.surface.log2`[1:100,1:30],color = colorRampPalette(c("blue", "white","red"))(1000),cluster_rows=T,cluster_cols=T,fontsize_row =7,fontsize_col = 7,main="Heatmap")
