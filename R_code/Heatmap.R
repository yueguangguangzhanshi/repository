library(ComplexHeatmap)
library(pheatmap)
library(openxlsx)
options(stringsAsFactors=FALSE)
data<- read.delim("heatmap.txt", row.names=1, stringsAsFactors=FALSE)
data<-read.xlsx("GO_Bp.xlsx",1,rowNames = T)

## heatmap
pheatmap(data,
         cluster_rows =F,
         cluster_cols=F,
         na_col="grey60",
         color=colorRampPalette(rev(c("red","yellow","white")))(20000),
         show_rownames=TRUE,show_colnames=TRUE,
         cellwidth =30,cellheight=10,
         border_color ="black",
         fontsize =10,
         legend_breaks = NA,
         legend = TRUE,
         main="GO_BP Heatmap")

pheatmap(data,
         cluster_rows = T,
         cluster_cols=F,
         scale="row",
         color=colorRampPalette(rev(c("red","white","blue")))(20000),
         show_rownames=TRUE,show_colnames=TRUE,
         cellwidth =30,cellheight=15,
         border_color ="black",
         fontsize =10,
         legend_breaks = NA,
         legend = TRUE,
         main="bochong IgG Heatmap")

## kmeans1
set.seed(1234)
Heatmap(data,
        col = colorRampPalette(rev(c("red","yellow","white")))(20000),
        k=4,cluster_columns = F,
        row_names_max_width = unit(10,"cm"),
        row_names_gp = gpar(fontsize =2),
        name = "FC_1.1",clustering_method_rows ="median", 
        width = unit(50,"mm"),height = unit(250,"mm"),
        row_dend_width = unit(30, "mm"))

pdf("GO_BP W1,W2 Heatmap.pdf",width=15,height=20)
Heatmap(data45,
        col = colorRampPalette(rev(c("red","yellow","white")))(20000),
        na_col="grey60",
        border=T,
        rect_gp = gpar(col = "black"),
        cluster_rows = F,
        cluster_columns = F,
        row_names_side = "left",
        row_names_gp = gpar(fontsize =9),
        column_names_side="top",
        column_names_gp = gpar(fontsize =9),
        column_names_rot = 45,
        row_names_rot = 15,
        heatmap_legend_param = list(title = "-LogP",
                                    title_gp = gpar(fontsize = 9),
                                    title_position = "topcenter",
                                    grid_height = unit(6, "mm"),
                                    grid_width = unit(6, "mm")),
        name="GO_BP Heatmap",
        column_title ="GO_BP Heatmap",
        column_title_gp = gpar(fontsize = 14),
        width = unit(50,"mm"),
        height = unit(400,"mm"))
dev.off()
fileName

## kmeans2
set.seed(1234)
pheatmap(as.matrix(NMF),color = colorRampPalette(c("blue", "white","red"))(1000),
         cluster_rows=T,cluster_cols=T,fontsize_row =7,fontsize_col = 7,
         main="heatmap",kmeans_k = 4)

xx=pheatmap(as.matrix(NMF),color = colorRampPalette(c("blue", "white","red"))(1000),cluster_rows=T,cluster_cols=T,fontsize_row =7,fontsize_col = 7,main="heatmap",kmeans_k = 4)

moduleGenes<-xx[["kmeans"]]$cluster==2
cluster<-rownames(NMF)[moduleGenes]
write.table(cluster,"clusterGenes.txt",sep = "\t")

## heatmap+group
annotation_col<-read.delim("group2.txt", row.names=1, stringsAsFactors=FALSE)
annotation_colors<-list(testgroup = c("Control" = "lightblue", "Model" = "orange", "HJF-1" = "red"))

pheatmap(t(data),
         cluster_rows = T,cluster_cols=F,
         scale="row",
         annotation_col = annotation_col,
         annotation_row = NA,
         annotation_colors = annotation_colors,
         color=colorRampPalette(rev(c("red","white","blue")))(20000),
         show_rownames=TRUE,show_colnames=TRUE,
         cellwidth =15,cellheight=15,
         border_color =NA,
         fontsize =10,
         legend_breaks = NA,
         legend = TRUE,
         main="Cluster")
