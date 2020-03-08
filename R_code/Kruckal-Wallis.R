data=Ratio.Data
anova.Pvalue<-c(rep(0,nrow(data)))
#预生成2个长度与输入文件行数相同的全为0的向量，将用于存储pvalue
type<-factor(c(rep("组1",3),rep("组2",3),rep("组3",3),rep("组4",3)))
#分组信息
for(i in 1:nrow(data)){
      if(sum(data[i,1:3])==0&&sum(data[i,4:6])==0&&sum(data[i,7:9])==0&&sum(data[i,10:12])==0){
            anova.Pvalue[i] <- "NA"
            t.Pvalue[i] <- "NA"
        }else{
              x<-kruskal.test(as.numeric(data[i,1:12])~type)
              anova.Pvalue[i]<-x$p.value
          }
  }
#2~4列是一组,5~7列为二组；
#将使用循环对每一行分别进行两独立样本非参检验；
#两组表达量都是0的基因，不检验；
out<-cbind(data,anova.Pvalue)
#每一行计算得到p value将被加入原文件的最后一列；
write.table(out,file="out.txt",quote=FALSE,sep="\t",row.names=FALSE)
#在原文件后面加入pvalue，输出文件格式txt；

