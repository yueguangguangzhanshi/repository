library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(org.Rn.eg.db)
library(clusterProfiler)
library(pathview)
library(xlsx)
library(dplyr)
library(tibble)
args=commandArgs(T)
parameter1 = args[1]
species<-args[2]
geneList<-read.xlsx(parameter1,1)
if (species=="hsa") {
  ID <- bitr(geneList[,1], fromType="UNIPROT", toType=c("SYMBOL","ENTREZID"), OrgDb="org.Hs.eg.db")
}
if (species=="mmu") {
  ID <- bitr(geneList[,1], fromType="UNIPROT", toType=c("SYMBOL","ENTREZID"), OrgDb="org.Mm.eg.db")
}
if (species=="rno") {
  ID <- bitr(geneList[,1], fromType="UNIPROT", toType=c("SYMBOL","ENTREZID"), OrgDb="org.Mm.eg.db")
}
kegg_name=read.xlsx("/home/fullmoon/R/material/kegg_name.xlsx",1,rowNames = TRUE)
kegg_pathway<-read.xlsx("/home/fullmoon/R/material/kegg_csp100_pathway.xlsx",1)

ID$Ratio<-geneList[match(ID[,1],geneList[,1]),2]
ID1<-(remove_rownames(ID)%>%column_to_rownames("ENTREZID"))
ID2<-ID1["Ratio"]

for(i in (1:nrow(kegg_pathway))){
  pv.out <- pathview(gene.data = ID2,pathway.id = kegg_pathway[i,1],
                     species = species, kegg.native = T,
                     sign.pos ="bottomleft",
                     mid = list(gene = "white", cpd = "white"),
                     bins=list(gene=20, cpd=10),
                     pdf.size=c(10,8),limit=list(gene=c(0,2), cpd=1),
                     out.suffix=kegg_pathway[i,2],map.null=F,same.layer = T)
}