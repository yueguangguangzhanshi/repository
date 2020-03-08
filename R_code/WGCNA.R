#???ݶ???
setwd("")
library(WGCNA)
library(reshape2)
library(stringr)
library(xlsx)
options(stringsAsFactors = FALSE)
enableWGCNAThreads()
type = "unsigned"
corType = "pearson"
exprMat="SampleGene.txt"
corFnc = ifelse(corType=="pearson", cor, bicor)
maxPOutliers = ifelse(corType=="pearson",1,0.05)
robustY = ifelse(corType=="pearson",T,F)
dataExprVar <- read.table(exprMat, sep='\t', row.names=1, header=T, 
                       quote="", comment="", check.names=F)


#=================================================================================



dataExpr <- as.data.frame(t(dataExprVar))
gsg = goodSamplesGenes(dataExpr, verbose = 3)
if (!gsg$allOK){
  # Optionally, print the gene and sample names that were removed:
  if (sum(!gsg$goodGenes)>0) 
    printFlush(paste("Removing genes:", 
                     paste(names(dataExpr)[!gsg$goodGenes], collapse = ",")));
  if (sum(!gsg$goodSamples)>0) 
    printFlush(paste("Removing samples:", 
                     paste(rownames(dataExpr)[!gsg$goodSamples], collapse = ",")));
  # Remove the offending genes and samples from the data:
  dataExpr = dataExpr[gsg$goodSamples, gsg$goodGenes]
}
nGenes = ncol(dataExpr)
nSamples = nrow(dataExpr)


#========================================================================


#????ֵɸѡ
sampleTree = hclust(dist(dataExpr), method = "average")
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="")
powers = c(c(1:10), seq(from = 12, to=30, by=2))
sft = pickSoftThreshold(dataExpr, powerVector=powers, 
                        networkType=type, verbose=5)
par(mfrow = c(1,2))
cex1 = 0.9
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",
     ylab="Scale Free Topology Model Fit,signed R^2",type="n",
     main = paste("Scale independence"))
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red")
abline(h=0.85,col="red")

plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, 
     cex=cex1, col="red")
power = sft$powerEstimate
power


#============================================================================


#???繹????One-step network construction and module detection
net = blockwiseModules(dataExpr, power = power, maxBlockSize = nGenes,
                       TOMType = type, minModuleSize = 20,
                       reassignThreshold = 0, mergeCutHeight = 0.15,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs= T , corType = corType, 
                       maxPOutliers=maxPOutliers, loadTOMs=TRUE,
                       saveTOMFileBase = paste0(exprMat, ".tom"),
                       verbose = 3)
table(net$colors)

#========================================================================

#?㼶??????չʾ????ģ??
moduleLabels = net$colors

moduleColors = labels2colors(moduleLabels)

plotDendroAndColors(net$dendrograms[[1]], moduleColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)

table(moduleColors)

##ѡ????????Module colors????ͼ
library(ggplot2)

library(ggthemes)

df2= read.table("mtcars2.txt",header = TRUE,sep='\t')

module_reorder=reorder(df2$module,df2$gene_number)

color1=c("lightcyan","midnightblue","cyan","salmon","tan","greenyellow","magenta","purple","pink","black","red","green","yellow","brown","blue","turquoise")##colorҪ????##

ggplot(df2, aes(x = module_reorder, y = gene_number,label=df2$gene_number),cex.axis=3,cex.lab=5)+geom_bar(fill=color1,stat = "identity",position="dodge")+coord_flip()+labs(title="Counts for each gene module", x="Gene Module", y="Gene count")+theme_minimal()+theme(
  plot.title = element_text(color = "red", size = 14, face = "bold.italic"),
  axis.title.x = element_text(color="blue", size = 14, face = "bold"),
  axis.title.y = element_text(color="#993333", size = 14, face = "bold")
)+geom_text(nudge_y = 10)


#========================================================================


#???ӻ????????? (TOM plot)
load(net$TOMFiles[1], verbose=T)
TOM <- as.matrix(TOM)
dissTOM = 1-TOM
plotTOM = dissTOM^7
diag(plotTOM) = NA
##???Ѵ?��ʱ?䣬??Ҫʱʡ??
TOMplot(plotTOM, net$dendrograms, moduleColors, 
        main = "Network heatmap plot, all genes")


#========================================================================

##????????ȫ?????򵼳?????????Cytoscape
probes = colnames(dataExpr)
dimnames(TOM) <- list(probes, probes)
cyt = exportNetworkToCytoscape(TOM,edgeFile = paste(exprMat, ".edges.txt", sep=""),nodeFile = paste(exprMat, ".nodes.txt", sep=""), weighted = TRUE, threshold = 0.5, nodeNames = probes, nodeAttr = moduleColors)


#========================================================================


#??��????????
trait <- "TraitsSample.txt"
MEs = net$MEs
MEs_col = net$MEs
colnames(MEs_col) = paste0("ME", labels2colors(
  as.numeric(str_replace_all(colnames(MEs),"ME",""))))
if(trait != "") {
  traitData <- read.table(file=trait, sep='\t', header = TRUE, row.names=1,check.names=FALSE, comment='',quote="")
  sampleName = rownames(dataExpr)
  traitData = traitData[match(sampleName, rownames(traitData)), ]
  }

##ѡ???Ե???ME????ֵ????
rownames(MEs_col)=rownames(traitData)
write.table(MEs_col,"Module.txt",sep="\t",qmethod = "double")

##ѡ????????Trait-Trait?????Ծ?????ͼ
library(pheatmap)
TraitCor=cor(traitData, traitData, use = "p")
TraitP=corPvalueStudent(TraitCor, nSamples)
pheatmap(TraitCor,cluster_rows=T,cluster_cols=T,fontsize_row =7,fontsize_col = 7,main="Correlation Heatmap-Trait")


#????Trait-Module????????ͼ
if (corType=="pearson") {
  modTraitCor = cor(MEs_col, traitData)
  modTraitP = corPvalueStudent(modTraitCor, nSamples)
} else {
  modTraitCorP = bicorAndPvalue(MEs_col, traitData, robustY=robustY)
  modTraitCor = modTraitCorP$bicor
  modTraitP   = modTraitCorP$p
}

textMatrix = paste(signif(modTraitCor, 2), "\n(", signif(modTraitP, 1), ")", sep = "")

dim(textMatrix) = dim(modTraitCor)
sizeGrWindow(7, 7)
labeledHeatmap(Matrix = modTraitCor, xLabels = colnames(traitData), yLabels = colnames(MEs_col),cex.lab = 0.8, ySymbols = colnames(MEs_col), colorLabels = FALSE,  colors = blueWhiteRed(50),  textMatrix = textMatrix, setStdMargins = TRUE,  cex.text = 0.55, zlim = c(-1,1), main = paste("Module-trait relationships"),yLabelsPosition="left")
write.xlsx(modTraitCor,"modTraitCor.xlsx")
write.xlsx(modTraitP,"modTraitP.xlsx")

##??ѡ??????????????ME-Trait??????ͼ
write.table(modTraitCor,"modTraitCor_s.txt",sep = "\t")
write.table(modTraitP,"modTraitP_s.txt",sep = "\t")

modTraitCor_s=as.matrix(read.delim("modTraitCor_s.txt",row.names = 1))
modTraitP_s=as.matrix(read.delim("modTraitP_s.txt",row.names = 1))
textMatrix_s = paste(signif(modTraitCor_s, 2), "\n(", signif(modTraitP_s, 1), ")", sep = "")
textMatrix_s[-c(1,47,48,49,129,185,199,200,219,220)]<-""
textMatrix_s[c(1,47,48,49,129,185,199,200,219,220)]<-"*"
dim(textMatrix_s) = dim(modTraitCor_s)
sizeGrWindow(7, 7)
labeledHeatmap(Matrix = modTraitCor_s, xLabels = colnames(modTraitCor_s), yLabels = rownames(modTraitCor_s),cex.lab = 1, ySymbols = rownames(modTraitCor_s), colorLabels = FALSE,  colors = blueWhiteRed(50),  textMatrix = textMatrix_s, setStdMargins = TRUE,  cex.text = 1.3, zlim = c(-1,1), main = paste("Module_chosen_trait relationships"),yLabelsPosition="left",cex.lab.y=0.8,cex.lab.x=0.7)


##????ģ??(+traits)֮??adjacency??????ͼ
Trait=as.data.frame(traitData[,10])
names(Trait) = "Trait_50"
MEs_Trait_col = orderMEs(cbind(MEs_col,Trait))
plotEigengeneNetworks(MEs_Trait_col, "Eigengene adjacency heatmap", marDendro = c(5,5,4,4),marHeatmap = c(6,6,4,4), plotDendrograms = T, xLabelsAngle = 90)

##????Trait??ME??????Barͼ
GS1=as.numeric(cor(Trait,dataExpr, use="p"))
GeneSignificance=abs(GS1)
sizeGrWindow(8,7)
par(mfrow = c(1,1))
plotModuleSignificance(GeneSignificance,moduleColors)


#========================================================================


#????ģ???????????????Ծ???
if (corType=="pearson") {
  geneModuleMembership = as.data.frame(cor(dataExpr, MEs_col, use = "p"))
  MMPvalue = as.data.frame(corPvalueStudent(
    as.matrix(geneModuleMembership), nSamples))
} else {
  geneModuleMembershipA = bicorAndPvalue(dataExpr, MEs_col, robustY=robustY)
  geneModuleMembership = geneModuleMembershipA$bicor
  MMPvalue   = geneModuleMembershipA$p
}

# ??????״?????????????Ծ???
# ֻ??��??????״???ܽ??м??㣬????????ɢ??��???ڹ?????Ʒ??ʱ??תΪ0-1??????
if (corType=="pearson") {
  geneTraitCor = as.data.frame(cor(dataExpr, traitData, use = "p"))
  geneTraitP = as.data.frame(corPvalueStudent(
    as.matrix(geneTraitCor), nSamples))
} else {
  geneTraitCorA = bicorAndPvalue(dataExpr, traitData, robustY=robustY)
  geneTraitCor = as.data.frame(geneTraitCorA$bicor)
  geneTraitP   = as.data.frame(geneTraitCorA$p)
}

##ѡ???Ե?????????????????????
as.data.frame(geneTraitCor)
write.table(geneTraitCor,file = "geneTraitCor.txt",sep = "\t")

# ??????��???????Ծ???��????��,ָ??????Ȥģ?????з???
pheno = "rectlike"
module = "magenta"
modNames = substring(colnames(MEs_col), 3)

# ??ȡ??ע????
module_column = match(module, modNames)
pheno_column = match(pheno,colnames(traitData))

# ??ȡģ???ڵĻ???
moduleGenes = moduleColors == module
probes = colnames(dataExpr)
modProbes = probes[moduleGenes]
write.table(modProbes,paste("modProbes_",module,".txt"),sep="\t",qmethod = "double")

# ????״?߶????صĻ?????Ҳ??????״???ص?ģ?͵Ĺؼ?????
sizeGrWindow(7, 7)
par(mfrow = c(1,1))
verboseScatterplot(abs(geneModuleMembership[moduleGenes, module_column]),
                   abs(geneTraitCor[moduleGenes, pheno_column]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = paste("Gene significance for", pheno),
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, cex.lab = 1.2, cex.axis = 1.2, col = "black",
                   abline=TRUE)
##??????Ҫ?????ı?ע??##
text(abs(geneModuleMembership[moduleGenes, module_column]),
     abs(geneTraitCor[moduleGenes, pheno_column]),modProbes,cex = 0.6,pos=3)

##??ѡ??????ģ????gene??Trait???????Ծ?????ͼ
library(pheatmap)
module = "blue"
moduleGenes = moduleColors == module
modProbes_module<-dataExpr[moduleGenes]
SampleGene_module<-modProbes_module[order(rownames(modProbes_module)),]
traitData<-traitData[order(rownames(traitData)),]
gene_col_TraitCor=cor(SampleGene_module, traitData, use = "p")
gene_col_TraitP=corPvalueStudent(gene_col_TraitCor, nSamples)
sizeGrWindow(7, 7)
pheatmap(gene_col_TraitCor,color = colorRampPalette(c("blue", "white","red"))(1000),fontsize_row =2,fontsize_col = 7, main=paste0("Correlation Heatmap_",module))->pheatmap_module
neworder_module<-gene_col_TraitCor[pheatmap_module$tree_row$order,pheatmap_module$tree_col$order]
write.xlsx(neworder_module,"neworder_blue.xlsx")

#????CYToscape
module = "magenta"
moduleGenes = moduleColors == module
modTOM = TOM[moduleGenes, moduleGenes]
probes = colnames(dataExpr)
modProbes = probes[moduleGenes]
dimnames(modTOM) = list(modProbes, modProbes)
cyt = exportNetworkToCytoscape(modTOM,
      edgeFile = paste('CytoscapeInput-edges-', paste(module, collapse='-'), '.txt', sep=''),
      nodeFile = paste('CytoscapeInput-nodes-', paste(module, collapse='-'), '.txt', sep=''),
      weighted = TRUE,threshold = 0.15,
      nodeNames = modProbes,
      nodeAttr = moduleColors[moduleGenes]
    )

