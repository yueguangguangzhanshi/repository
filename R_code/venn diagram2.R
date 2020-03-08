library(VennDiagram)
library(xlsx)
workbook<-"data.xlsx"

data1<-t(read.xlsx(workbook,2))
data2<-t(read.xlsx(workbook,3))
venn<-venn.diagram(x=list(A=data1,B=data2),
                   resolution = 600, 
                   imagetype = "png", 
                   filename = "venn_2groups.png",
                   fill = c("red","yellow"),
                   label.col="black",
                   scale=F,
                   cat.pos=6,
                   cat.cex=1)

data1<-t(read.xlsx(workbook,1))
data2<-t(read.xlsx(workbook,2))
data3<-t(read.xlsx(workbook,3))
venn<-venn.diagram(x=list(A=data1,B=data2,C=data3),
                   resolution = 600, 
                   imagetype = "png", 
                   filename = "venn_3groups.png",
                   fill = c("red","yellow","blue"),
                   label.col="black",
                   cat.cex=1)

data1<-t(read.xlsx(workbook,1))
data2<-t(read.xlsx(workbook,2))
data3<-t(read.xlsx(workbook,3))
data4<-t(read.xlsx(workbook,4))
venn<-venn.diagram(x=list(A=data1,B=data2,C=data3,D=data4),
                  resolution = 600, 
                  imagetype = "png", 
                  filename = "venn_4groups.png",
                  fill = c("red","yellow","blue","green"),
                  label.col="black",
                  cat.cex=1)

data1<-t(read.xlsx(workbook,1))
data2<-t(read.xlsx(workbook,2))
data3<-t(read.xlsx(workbook,3))
data4<-t(read.xlsx(workbook,4))
data5<-t(read.xlsx(workbook,5))
venn<-venn.diagram(x=list(A=data1,B=data2,C=data3,D=data4,E=data5),
                   resolution = 600, 
                   imagetype = "png", 
                   filename = "venn_5groups.png",
                   fill = c("red","yellow","blue","green","orange"),
                   label.col="black",
                   cat.cex=1)
