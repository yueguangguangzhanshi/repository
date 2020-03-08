library(corrplot)
setwd("~/out/20191127-corrplot")
data<-read.delim("data.txt",row.names=1,sep="\t", header=T)
cor_data<-cor(data)
cor_data_p <- cor.mtest(data)$p

corrplot(cor_data)

corrplot.mixed(cor_data,tl.pos="lt")

corrplot(cor_data,method ="pie")

corrplot(cor_data,method ="color")

corrplot(cor_data,method ="number")


corrplot(cor_data, type = "upper", order = "hclust",cl.length = 11)

corrplot(cor_data, p.mat = cor_data_p, insig = "label_sig",sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white")

corrplot(cor_data, p.mat =cor_data_p, method = "color", type = "upper",sig.level = c(.001, .01, .05), pch.cex = .9,insig = "label_sig", pch.col = "white", order = "AOE")

corrplot(cor_data, method = "color", type = "upper", order = "AOE", number.cex = 1,addCoef.col = "black",tl.col = "black", tl.srt = 90,p.mat = cor_data_p, sig.level = 0.01, insig = "blank",diag = FALSE)
