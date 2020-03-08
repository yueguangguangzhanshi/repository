library(VennDiagram)
venn.diagram(list(KO=kowt$ko,WT=kowt$wt[1:1863]),col=c(rgb(66,133,244,maxColorValue = 255),rgb(234,67,53,maxColorValue = 255)),filename = "VennDiagram.tif")
