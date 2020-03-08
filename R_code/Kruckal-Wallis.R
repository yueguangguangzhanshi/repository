data=Ratio.Data
anova.Pvalue<-c(rep(0,nrow(data)))
#Ԥ����2�������������ļ�������ͬ��ȫΪ0�������������ڴ洢pvalue
type<-factor(c(rep("��1",3),rep("��2",3),rep("��3",3),rep("��4",3)))
#������Ϣ
for(i in 1:nrow(data)){
      if(sum(data[i,1:3])==0&&sum(data[i,4:6])==0&&sum(data[i,7:9])==0&&sum(data[i,10:12])==0){
            anova.Pvalue[i] <- "NA"
            t.Pvalue[i] <- "NA"
        }else{
              x<-kruskal.test(as.numeric(data[i,1:12])~type)
              anova.Pvalue[i]<-x$p.value
          }
  }
#2~4����һ��,5~7��Ϊ���飻
#��ʹ��ѭ����ÿһ�зֱ���������������ǲμ��飻
#�������������0�Ļ��򣬲����飻
out<-cbind(data,anova.Pvalue)
#ÿһ�м���õ�p value��������ԭ�ļ������һ�У�
write.table(out,file="out.txt",quote=FALSE,sep="\t",row.names=FALSE)
#��ԭ�ļ��������pvalue������ļ���ʽtxt��
