#volcano plot
p=0.05
Fold=log2(1.5)
data$threshold = as.factor(ifelse(data[20] < 0.05 & data[19]> Fold, 'Up', ifelse(data[20] < 0.05 &data[19]<= (-Fold), 'Down', 'Not')))

ggplot(data, aes(x=FC3, y=-log10(ttest3)))+ xlim(c(-2, 2)) + ylim(c(0,3))+xlab("log2(fold change)") + ylab("-log10(p-value)")+labs(title="WT+CVF volcano plot",hjust=0.5)+geom_hline(yintercept=1.3,linetype=3)+geom_vline(xintercept=c(-log2(1/1.5),log2(1/1.5)),linetype=3)+geom_point(aes(color =threshold,fill=threshold,size=1))+scale_color_manual(values =  c("green","grey","red"))+theme_bw(base_size = 12, base_family = "Times")+theme(plot.title = element_text(hjust = 0.5))+guides(size=FALSE)
