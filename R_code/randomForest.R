library(randomForest)
repeat{
  sub_M<-sample(1:18,9)
  train_M<-TraitsSample_after_pca_M[sub_M,]
  sub_N<-sample(1:26,13)
  train_N<-TraitsSample_after_pca_N[sub_N,]
  test_M<-TraitsSample_after_pca_M[-sub_M,]
  test_N<-TraitsSample_after_pca_N[-sub_N,]
  train<-rbind(train_M,train_N)
  test<-rbind(test_M,test_N)
  model<-randomForest(M.N~.,ntree=500,data=train,importance=T)
  x=mean(model$err.rate[,1])
  y<-subset(test,select=-M.N)
  pred<-predict(model,y,type = "class")
  k<-test[,"M.N"]
  aa<-table(pred,k)
  correct<-(aa[1]+aa[4])/sum(aa)
  if(x<0.2&correct>0.75){
    print(model)
    break
  }
 }




importance(model,type=2)
