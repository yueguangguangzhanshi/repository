library(pca3d)
library(ggplot2)
library(ggord)
library(openxlsx)
library(ggbiplot)
data<-read.xlsx("data.xlsx",sheet=3,rowNames = TRUE)
pc.cr<-prcomp(t(data))
pca_group=factor(c(rep('HC',6),rep('LTBI',6),rep('TB',21)))

p1 <- ggord(pc.cr,pca_group,obslab=T,arrow=NULL,txt=NULL,ellipse =T,alpha=1,poly=F,coord_fix=F,facet = F)
p1+scale_shape_manual('Groups', values = c(1,2))+ theme(plot.title = element_text(hjust = 0.5,size=15))+labs(title="PCA Plot")
p1 + theme_classic()

pca2d(pc.cr,show.labels = T ,show.ellipses = F,radius = 1.5,show.plane = T,group=pca_group,palette = c("red","blue"))
pca3d(pc.cr,show.labels = T ,show.ellipses =  T,radius = 1,show.plane = T)

ggbiplot(pc.cr,  groups = pca_group,obs.scale = 1.5, ellipse = F, labels =rownames(t(data)),var.axes = F,ellipse.prob=0.95,circle = T, circle.prob = 0.5) +scale_color_discrete(name = 'groups') + theme( legend.position = 'right',plot.title = element_text(hjust =0.5,size=15))+labs(title="华盈 IgG PCA Plot")

