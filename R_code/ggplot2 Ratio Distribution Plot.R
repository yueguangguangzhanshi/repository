library(ggplot2)
library(grid)
library(xlsx)

bplot=read.xlsx("bplot.xlsx",sheetIndex = 1)
bplot2=read.xlsx("bplot.xlsx",sheetIndex = 2)
bplot3=read.xlsx("bplot.xlsx",sheetIndex = 3)
bplot4=read.xlsx("bplot.xlsx",sheetIndex = 4)

tiff(filename = "Protein Combination vs Zey.tiff",width = 4508 ,height = 3000,res=700)
ggplot(data=bplot4,aes(x=x,y=y,fill=color))  + geom_col(width=1) + scale_fill_manual(values=c('ratio<0.833'="#3399FF", '0.833<ratio<1.2'="grey", 'ratio>1.2'="red"))  + geom_hline(linetype='dashed',yintercept = c(0.833,1.2))  + geom_hline(linetype='solid',yintercept = 0)  + labs(x = "Protein numbers",y="Ratio(Combination vs Zey)",title="Ratio Distribution Plot",fill="Ratio")  + theme_classic() + theme(plot.title = element_text(hjust = 0.5,size=20),axis.title=element_text(size=12),legend.text=element_text(size=12),legend.title=element_text(size=12),legend.position = "top")
dev.off()