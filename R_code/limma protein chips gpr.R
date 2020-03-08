data<-list()
fileName = dir("~/out/20191106/",pattern = '*.gpr')
data <- lapply(fileName, function(x){
  read.delim(x,skip=31)})
data_mutate<-list()
for (i in 1:48) {
  data_mutate[[i]]<-transmute(data[[i]],Name=Name,FB635=F635.Median-B635.Median,FB532=F532.Median-B532.Median)%>%add_column(Sample=i)
}
rbind_data<-list()
rbind_data[[1]]<-data_mutate[[1]]
for (i in 2:48) {
  rbind_data[[i]]<-rbind(rbind_data[[i-1]],data_mutate[[i]])
}
data_IgG<-rbind_data[[48]][,c(4,1,3)]
data_IgM<-rbind_data[[48]][,c(4,1,2)]
reshape_data_IgG<-dcast(data_IgG,Name~Sample,mean)
reshape_data_IgM<-dcast(data_IgM,Name~Sample,mean)
write.xlsx(reshape_data_IgG,"reshape_IgG.xlsx")
write.xlsx(reshape_data_IgM,"reshape_IgM.xlsx")
IgG_wayen<-read.xlsx("reshape data.xlsx",1)
IgG_bochong<-read.xlsx("reshape data.xlsx",3)
IgM_wayen<-read.xlsx("reshape data.xlsx",2)
IgM_bochong<-read.xlsx("reshape data.xlsx",4)

data_IgG<-left_join(IgG_bochong,IgG_wayen,by="Name",suffix = c("_bochong", "_wayen"))
data_IgG<-column_to_rownames(data_IgG,'Name')
data_IgG[data_IgG<0]<-NA
Process = preProcess(data_IgG,method = c("medianImpute"))
pre_IgG = predict(Process, data_IgG)
nor_lgG<-normalizeQuantiles(pre_IgG)
nor_lgG<-log2(nor_lgG)

data_IgM<-left_join(IgM_bochong,IgM_wayen,by="Name",suffix = c("_bochong", "_wayen"))
data_IgM<-column_to_rownames(data_IgM,'Name')
data_IgM[data_IgM<0]<-NA
Process = preProcess(data_IgM,method = c("medianImpute"))
pre_IgM = predict(Process, data_IgM)
nor_lgM<-normalizeQuantiles(pre_IgM)
nor_lgM<-log2(nor_lgM)

write.xlsx(nor_lgG,"nor_lgG.xlsx",rowNames=T)
write.xlsx(nor_lgM,"nor_lgM.xlsx",rowNames=T)
