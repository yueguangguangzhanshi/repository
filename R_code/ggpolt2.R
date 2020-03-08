setwd("C://Users/LHB/Desktop/out/")
df= read.table("mtcars.txt",header = TRUE,sep='\t')
df$cyl <- as.factor(df$cyl)
library(ggplot2)
library(plyr)
mu <- ddply(df, "cyl", summarise, grp.mean=mean(mpg))


#========================================================================ɢ??ͼ

#ɢ??ͼ
qplot(x=mpg, y=wt, data=df, geom = "point")


#ɢ??ƽ??????ͼ
qplot(x=mpg, y=wt, data = df, geom = c("point", "smooth"))

#ɢ????ɫ??״ͼ
qplot(x=mpg, y=wt, data = df, colour=cyl, shape=cyl)

ggplot(data=df, aes(x=mpg, y=wt))+geom_point(aes(color=cyl, shape=cyl))

#?ݶ???ɫɢ??ͼ
  ##?ӻع???
ggplot(df, aes(x=wt, y=mpg))+geom_point(aes(color = weight_g),position = "jitter")+scale_color_gradient(low="blue", high="red")+geom_abline(color="blue")

 ##???ı?
ggplot(df, aes(x=wt, y=mpg))+geom_point(aes(color=cyl),position = "jitter")+geom_text(aes(label = ID, color = cyl), vjust = -1,size=2)

 ##????ɫ?ݶ?
ggplot(data = df, aes(x=wt, y=mpg))+geom_point(aes(color = wt))+scale_color_gradient2(midpoint=480, low="blue", mid="white",high="red", space = "Lab" )


#========================================================================????ͼ

#????ͼ
qplot(cyl, mpg, data = df, geom = "boxplot", fill=cyl)

ggplot(data = df, aes(x=cyl, y=mpg))+geom_boxplot(notch = FALSE,aes(fill=cyl))+ scale_fill_brewer(palette="Set2")+facet_grid(type~.)


#========================================================================С????ͼ

#С????ͼ
qplot(cyl, mpg, data = df, geom = "violin")

ggplot(data = df, aes(x=cyl, y=mpg))+geom_violin(trim = FALSE,aes(fill=cyl))

#========================================================================??ͼ

#??ͼ
ggplot(df, aes(x=cyl,y=mpg))+geom_dotplot()

#????ɫͼ
qplot(cyl, mpg, data = df, geom = "dotplot", stackdir="center", binaxis="y", dotsize=0.5, color=cyl)

ggplot(df, aes(x=cyl,y=mpg))+geom_dotplot(aes(fill=cyl,color=cyl),binaxis = "y", stackdir = "center")


#========================================================================ֱ??ͼ

df2= read.table("mtcars2.txt",header = TRUE,sep='\t')
df2$len <- as.factor(df2$len)

#????ͼ
ggplot(df2, aes(x = module, y = gene_number))+geom_bar(aes(fill=color),stat = "identity",position="dodge")+coord_flip()

#ֱ??ͼ
ggplot(df2, aes(x = time, y = bill, fill=sex))+geom_histogram( position=position_dodge(width = 1),stat = "identity")+theme_classic()

#??????ֱ??ͼ
ggplot(df3, aes(x = dose,y = len,ymin = len-sd, ymax = len+sd))+geom_bar(aes(color=dose,fill=dose), stat = "identity")+geom_errorbar(color="black", width=0.1)+geom_line(aes(group=1))


#========================================================================?ܶ?ͼ

#?ܶ?ͼ
qplot(mpg, data = df, geom = "density", color=cyl, linetype=cyl)

ggplot(df, aes(x=mpg))+stat_density()

#?ܶ???ɫͼ
ggplot(df, aes(x=mpg))+geom_density(aes(color=cyl))

#?ܶ???ɫ͸??ͼ
ggplot(df, aes(x=mpg))+geom_density(aes(fill=cyl), alpha=0.4)

#?ܶȾ?????ɫͼ
ggplot(df, aes(x=mpg))+geom_density(aes(color=cyl)) + geom_vline(data=mu, aes(xintercept=grp.mean, color=cyl), linetype="dashed") + scale_color_manual(values = c("red", "blue","Brown","green"))

i<-ggplot(data,aes(ratio))
i + geom_density(kernel = "gaussian",color="blue",size=0.8,adjust = 2)
i + stat_count(color="blue")
i + stat_density(adjust = 2, kernel = "gaussian")
#========================================================================????ͼ

#????ͼ
ggplot(df, aes(x=mpg))+geom_area(stat = "bin")

#??????ɫͼ
ggplot(df, aes(x=mpg))+geom_area(aes(fill=cyl), stat = "bin", alpha=0.6)+theme_classic()

#?????ܶ?ͼ
ggplot(df, aes(x=mpg))+geom_area(aes(y=..density..), stat = "bin")


#========================================================================Ƶ?ʶ???ͼ

#Ƶ??ͼ
ggplot(df, aes(x=mpg))+geom_freqpoly()

#Ƶ???ܶ?ͼ
ggplot(df, aes(x=mpg))+geom_freqpoly(aes(y=..density..))+theme_minimal()

#Ƶ????ɫͼ
ggplot(df, aes(x=mpg))+geom_freqpoly(aes(color=cyl, linetype=cyl))+theme_minimal()


#========================================================================????ͼ

df2=read.table("mtcars2.txt",header = TRUE,sep='\t')
ggplot(df2, aes(x=time, y=bill, group=sex)) +  geom_line(aes(linetype = sex, color = sex,size=1))+  geom_point(aes(color=sex,shape=sex,size=2))+  theme(legend.position="top")+scale_shape_manual(values=c(12, 14))+scale_linetype_manual(values=c("twodash", "dotted"))
