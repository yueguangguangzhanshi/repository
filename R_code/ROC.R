library(pROC)
library(openxlsx)
data<-read.xlsx("IgG IgM AUC.xlsx",sheet = 1,rowNames = T)
rocdata$category<-as.factor(rocdata$category)
fit.full<-glm(category~.,data=rocdata,family = binomial())
summary(fit.full)
pre=predict(fit.full,type='response')

pred2<-as.numeric(pred2)
modelroc=roc(training$grade,pred2,ci=T)
modelroc=roc(training$grade,pred2,percent=TRUE,print.thres=TRUE,plot=T,print.auc=T)
modelroc

plot(modelroc,print.auc=TRUE,auc.polygon=TRUE,grid.col=c("grey"),max.auc.polygon=TRUE,print.thres=T,auc.polygon.col="skyblue")

plot.roc(modelroc,auc.polygon=T,ci=T,identity=TRUE,ci.type="bars",print.auc=TRUE)

plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, 
     partial.auc.focus="se", grid=c(0.1, 0.2), 
      print.thres=TRUE,
     reuse.auc=FALSE)
