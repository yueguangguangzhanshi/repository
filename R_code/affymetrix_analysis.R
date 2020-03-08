celFiles <- list.celfiles('myCELs', full.names=TRUE)
rawData <- read.celfiles(celFiles)
geneCore <- rma(rawData)

#导入数据与质控
library(affy)
library(tcltk)
dir <- tk_choose.dir(caption = "Select folder")
cel.files <- list.files(path = dir, pattern = ".+\\.cel$", ignore.case = TRUE,
                        full.names = TRUE, recursive = TRUE)
basename(cel.files)
data.raw <- ReadAffy(filenames = cel.files)
pm.data <- pm(data.raw)
mm.data <- mm(data.raw)
library(simpleaffy)
qc.data <- qc(data.raw)
avbg.data <- as.data.frame(sort(avbg(qc.data)))
# 平均背景值，如果太大则表示可能有问题
sort(avbg(qc.data))
# 样品的scale factor,affy建议每个样品间的sf差异不能超过3倍
sfs.data <- sort(sfs(qc.data))
max(sfs.data)/min(sfs.data)
# 表达基因所占的比例，太小则表示有问题
as.data.frame(percent.present(qc.data))
# 内参基因的表达比例
ratios(qc.data)

#数据预处理
eset.mas <- expresso(data.raw, bgcorrect.method="mas", normalize.method="constant", pmcorrect.method="mas", summary.method="mas")
eset.rma <- rma(data.raw)
eset.mas5 <- mas5(data.raw)
#计算基因表达量
emat.rma.log2 <- exprs(eset.rma)
emat.mas5.nologs <- exprs(eset.mas5)
emat.rma.nologs <- 2^emat.rma.log2
emat.mas5.log2 <- log2(emat.mas5.nologs)
data.mas5calls <- mas5calls(data.raw)
eset.mas5calls <- exprs(data.mas5calls)
AP <- apply(eset.mas5calls, 1, function(x)any(x=="P"))
present.probes <- names(AP[AP])
paste(length(present.probes),"/",length(AP))
results.rma <- data.frame(emat.rma.log2)
results.present <- results.rma[present.probes,]
#每组仅1例样本
results.present$PLK0_vs_sh2<-results.present[,1]-results.present[,2]
sum(abs(results.present[,3])>=log2(2))
results.st <- results.present[abs(results.present$PLK0_vs_sh2)>=log2(2),]

#Moderated T statistic
library(limma)
pData(data.raw)$group <- c("HMEC_PLK0", "HMEC_sh2")
treatment <- factor(pData(data.raw)$group)
design <- model.matrix(~ 0 + treatment)
colnames(design) <- c("HMEC_PLK0", "HMEC_sh2")
contrast.matrix <- makeContrasts(HMEC_PLK0-HMEC_sh2,levels=design)
fit <- lmFit(data.frame(emat.rma.log2), design)
fit2 <- contrasts.fit(fit, contrast.matrix)
fit2 <- eBayes(fit2)
results.lim_1 <- topTable(fit2,adjust.method="fdr",p.value=0.05, lfc=1, number=30000,coef=1)
results.lim_2<- topTable(fit2,adjust.method="fdr",p.value=0.05, lfc=1, number=30000,coef=2)

#注释
library(annotate)
platformDB <- paste(eset.mas5@annotation, ".db",sep="")
EGID <- t(data.frame(lookUp(results.st, platformDB, "SYMBOL")))
HMEC$Symbol<-EGID
library(xlsx)
write.xlsx(HMEC,"HMEC.xlsx")
