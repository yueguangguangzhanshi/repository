p_data<-read.xlsx("p_data.xlsx",sheetIndex = 1)
p_data<-column_to_rownames(p_data,"NA.")
p_value<-c()
group<-c(rep(0,12),rep(1,15))
i<-1
for (i in 1:nrow(p_data)) {
  p_value[i]<-wilcox.test(as.numeric(p_data[i,])~group,alternative="two.sided",exact = TRUE)$p.value
}
p_value<-unlist(p_value)
p_adjust<-p.adjust(p_value, method = "holm")
pp<-cbind(p_data,p_value,p_adjust)
