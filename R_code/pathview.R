#????????
species<-"hsa"
library(org.Hs.eg.db)
library(clusterProfiler)
library(pathview)
library(xlsx)
#IDת??
geneList<-as.data.frame(geneList)
ID <- bitr(geneList[,1], fromType="UNIPROT", toType=c("SYMBOL","ENTREZID"), OrgDb="org.Hs.eg.db")
ncol<-ncol(geneList)
if(ncol==5){
  ID$FC1<-geneList[match(ID[,1],geneList[,1]),2]
  ID$FC2<-geneList[match(ID[,1],geneList[,1]),3]
  FC<-ID[,-(1:3)]
  rownames(FC)<-ID[,3]
  write.xlsx(ID,"Gene.xlsx")

#Pathway
  i<-1
  n<-nrow(na.omit(geneList))
  for(i in (1:n)){
    pv.out <- pathview(gene.data = FC[, 1:2], pathway.id = kegg_pathway[i,1],
                       species = species, kegg.native = F,
                       sign.pos ="bottomleft",
                       mid = list(gene = "white", cpd = "white"),
                       pdf.size=c(10,8),limit= list(gene=max(abs(FC)),cpd=1),
                       out.suffix=geneList[i,5],map.null=F,same.layer = F)

    pv.out <- pathview(gene.data = FC[, 1:2], pathway.id = kegg_pathway[i,1],
                       species = species, kegg.native = T,
                       sign.pos ="bottomleft",
                       mid = list(gene = "white", cpd = "white"),
                       pdf.size=c(10,8),limit= list(gene=max(abs(FC)), cpd=1),
                       out.suffix=geneList[i,5],map.null=F,same.layer = F)
    }
}else
  {
    ID$FC<-geneList[match(ID[,1],geneList[,1]),2]
    FC<-ID[,-(1:3)]
    FC<-as.matrix(FC)
    rownames(FC)<-ID[,3]
    write.xlsx(ID,"Gene.xlsx")
  
    #Pathway
    i<-1
    n<-nrow(na.omit(geneList))
    for(i in (1:n)){
      pv.out <- pathview(gene.data = FC[,1], pathway.id =  kegg_pathway[i,1],
                         species = species, kegg.native = F,
                         sign.pos ="bottomleft",
                         mid = list(gene = "white", cpd = "white"),
                         pdf.size=c(10,8),
                         limit= list(gene=max(abs(FC)), cpd=1),
                         out.suffix=geneList[i,4],map.null=F)
    
      pv.out <- pathview(gene.data = FC[,1], pathway.id = kegg_pathway[i,1],
                         species = species, kegg.native = T,
                         same.layer = F,
                         sign.pos ="bottomleft",
                         mid = list(gene = "white", cpd = "white"),
                         pdf.size=c(10,8),
                         limit= list(gene=max(abs(FC)), cpd=1),
                         out.suffix=geneList[i,4],map.null=F)
      }
  }
