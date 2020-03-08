#安装包
source("http://bioconductor.org/biocLite.R")
biocLite("clusterProfiler")


utils::setRepositories(ind=1:2)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("clusterProfiler")
BiocManager::install("org.Hs.eg.db")
BiocManager::install("pathview")

# 参数设置
organism<-"mmu"
p<-1
q<-1
OrgDb="org.Mm.eg.db"

library(OrgDb,character.only=T,quietly=T)
library(clusterProfiler)
library(pathview)
library(xlsx)

#ID转换
geneList<-xlsx::read.xlsx("protein.xlsx",sheetIndex = 1)
ID0 <- bitr(geneList[,1], fromType="UNIPROT", toType=c("SYMBOL","ENTREZID"), OrgDb=OrgDb)
ID <- bitr(geneList[,1], fromType="ENSEMBL", toType="ENTREZID", OrgDb=OrgDb)
ID$FC<-geneList[match(ID[,1],geneList[,1]),2]
FC<-as.matrix.data.frame(ID[,-1])
rownames(FC)<-ID[,2]
FC<-FC[,-1]
write.xlsx(ID0,"Gene.xlsx")

#GO分析
go_MF<-enrichGO(gene = geneList[,1],OrgDb = OrgDb,ont = "MF",pvalueCutoff = p, pAdjustMethod = "BH", qvalueCutoff = q,readable = F,keyType = "ENSEMBL",maxGSSize=20000)
go_CC<-enrichGO(gene = geneList[,1],OrgDb = OrgDb,ont = "CC",pvalueCutoff = p, pAdjustMethod = "BH", qvalueCutoff = q,readable = F,keyType = "ENSEMBL",maxGSSize=20000)
go_BP<-enrichGO(gene = geneList[,1],OrgDb = OrgDb,ont = "BP",pvalueCutoff = p, pAdjustMethod = "BH", qvalueCutoff = q,readable = F,keyType = "ENSEMBL",maxGSSize=20000)

go_filter_MF<-gofilter(go_MF, level = 2:4)
go_filter_CC<-gofilter(go_CC, level = 2:4)
go_filter_BP<-gofilter(go_BP, level = 2:4)

dropGO_MF<-dropGO(go_MF)
dropGO_CC<-dropGO(go_CC)
dropGO_BP<-dropGO(go_BP)

go_simplity_MF<-simplify(go_MF,cutoff = 0.9)
go_simplity_CC<-simplify(go_CC,cutoff = 0.9)
go_simplity_BP<-simplify(go_BP,cutoff = 0.9)


go<-enrichGO(gene = ID$ENTREZID,OrgDb = OrgDb,ont = "ALL",pvalueCutoff = p, pAdjustMethod = "BH", qvalueCutoff = q,readable = T,keyType = "ENTREZID")

## GO文件导出
write.xlsx(go_MF@result[go_MF@result['Count']>3,],sheetName = "GO_MF",file = "go.xlsx")
write.xlsx(go_CC@result[go_CC@result['Count']>3,],sheetName = "GO_CC",file = "go.xlsx",append = T)
write.xlsx(go_BP@result[go_BP@result['Count']>3,],sheetName = "GO_BP",file = "go.xlsx",append = T)

## GO_filter文件导出
write.xlsx(go_filter_MF,sheetName = "GO_MF",file = "go_level.xlsx")
write.xlsx(go_filter_CC,sheetName = "GO_CC",file = "go_level.xlsx",append = T)
write.xlsx(go_filter_BP,sheetName = "GO_BP",file = "go_level.xlsx",append = T)

## go_simplity文件导出
write.xlsx(go_simplity_MF,sheetName = "GO_MF",file = "go_simplity.xlsx")
write.xlsx(go_simplity_CC,sheetName = "GO_CC",file = "go_simplity.xlsx",append = T)
write.xlsx(go_simplity_BP,sheetName = "GO_BP",file = "go_simplity.xlsx",append = T)

## GO条形图
png(file="GO_MF.png",width = 1200, height = 600,res=100)
barplot(go_MF,title="Gene Ontology Molecular Function",showCategory = 15)
dev.off()
png(file="GO_CC.png",width = 1200, height = 600,res=100)
barplot(go_CC,title="Gene Ontology Cellular Component",showCategory = 15)
dev.off()
png(file="GO_BP.png",width = 1200, height = 600,res=100)
barplot(go_BP,title="Gene Ontology Biological Process",showCategory = 15)
dev.off()

## GO气泡图
png(file="GO_MF_气泡图.png",width = 1200, height = 800,res=100)
dotplot(go_MF,showCategory=20,title="Gene Ontology Molecular Function")
dev.off()
png(file="GO_CC_气泡图.png",width = 800, height = 800,res=100)
dotplot(go_CC,showCategory=20,title="Gene Ontology Cellular Component")
dev.off()
png(file="GO_BP_气泡图.png",width = 1000, height = 800,res=100)
dotplot(go_BP,showCategory=20,title="Gene Ontology Biological Process")
dev.off()

## GO网络图
pdf(file="GO_????ͼ.pdf",width=10)
cnetplot(go_MF,categorySize="pvalue",foldChange = FC,colorEdge=T)
cnetplot(go_MF,categorySize="pvalue",foldChange = FC,colorEdge=T,circular=T)
cnetplot(go_CC,categorySize="pvalue",foldChange = FC,colorEdge=T)
cnetplot(go_CC,categorySize="pvalue",foldChange = FC,colorEdge=T,circular=T)
cnetplot(go_BP,categorySize="pvalue",foldChange = FC,colorEdge=T)
cnetplot(go_BP,categorySize="pvalue",foldChange = FC,colorEdge=T,circular=T)
dev.off()

## GO字母集
pdf(file="GO_Tree_analysis.pdf",width=10)
plotGOgraph(go_CC)
plotGOgraph(go_MF)
plotGOgraph(go_BP)
goplot(go_CC)
goplot(go_MF)
goplot(go_BP)
dev.off()

# KEGG分析
kegg0<-enrichKEGG(gene = ID$ENTREZID, organism =organism, keyType = "kegg",pvalueCutoff = p,qvalueCutoff = q,use_internal_data = T,minGSSize=1,maxGSSize=5000)
kegg<-setReadable(kegg0, OrgDb, keyType="ENTREZID")

keggM<-enrichMKEGG(gene = ID$ENTREZID, organism =organism, keyType = "kegg",pvalueCutoff = p,qvalueCutoff = q)

## KEGG文件导出
write.xlsx(kegg,sheetName = "KEGG_Pathway",file = "kegg_pathway.xlsx")

## KEGG气泡图
png(file="KEGG_Pathway_气泡图.png",width = 800, height = 800,res=100)
dotplot(kegg,showCategory=20,title="KEGG_Pathway")
dev.off()

## KEGG网络图
pdf(file="KEGG_Pathway_网络图.pdf",width=10)
emapplot(kegg)
cnetplot(kegg,categorySize="pvalue",foldChange = FC,colorEdge=T,showCategory = 10)
cnetplot(kegg,categorySize="pvalue",foldChange = FC,colorEdge=T,circular=T,showCategory = 10)
dev.off()
