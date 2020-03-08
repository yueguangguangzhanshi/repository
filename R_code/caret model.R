library(caret)
library(parallel)
library(doParallel)
library(xlsx)
library(tibble)
library(rattle)
library(rpart.plot)
library(pROC)
# 线程设置
cluster_Set <- makeCluster(4)
registerDoParallel(cluster_Set)

# 数据预处理
data<-read.xlsx("data.xlsx",sheetIndex = 1)
Process = preProcess(data,method = c("knnImpute","center","scale","corr","zv"))
newdata1 = predict(Process, data)

# 特征选择
subsets = seq(10,150,10)
ctrl= rfeControl(functions = treebagFuncs, method = "cv")
Profile = rfe(newdata1[,-1], newdata1$grade, sizes = subsets, rfeControl = ctrl)
newdata2<-cbind(newdata1$grade,newdata1[Profile$optVariables])
colnames(newdata2)[1]<-"grade"

# 模型建立
fitControl <- trainControl(method = "cv",summaryFunction=twoClassSummary,classProbs = TRUE)
repeat{
  inTrain <- createDataPartition(y = newdata2$grade,p = .75,list = FALSE)
  training <- newdata2[ inTrain,]
  testing  <- newdata2[-inTrain,]
  Fit<- train(grade ~ ., data = training, method = "rpart", trControl = fitControl,metric="ROC")
  Fit$results
  if(max(Fit$results[,2])>0.8){
    print(Fit)
    # 测试集预测
    pred<-predict(Fit$finalModel,testing,type = "class")
    k<-testing[,"grade"]
    x<-table(pred,k)
    print(x)
    correct<-(x[1]+x[4])/sum(x)
    print(correct)
    break
  }
}
asRules(Fit$finalModel)
fancyRpartPlot(Fit$finalModel,type=0)
rpart.plot(Fit$finalModel,type=1)
rpart.plot(Fit$finalModel,branch=0, extra=8, under=TRUE, faclen=0, main="决策树",type=0)
table(pred,k)
write.xlsx(training,"result1.xlsx",sheetName = "training")
write.xlsx(testing,"result1.xlsx",sheetName = "testing",append = T)

# 模型评价
confusionMatrix(pred,testing$grade)

# ROC
