library(rpart)
library(rpart.plot)
library(caret)
library(rattle)
library(C50)
library(party)
library(pROC)

set.seed(16)
inTrain <- createDataPartition(y = filterdata$grade,p = .7,list = FALSE)
training <- filterdata[ inTrain,]
testing  <- filterdata[-inTrain,]

tc <- rpart.control(minsplit = 3,minbucket=3,xval = 10)
tc <- rpart.control(minsplit = 3,xval = 10)
rpart.model <- rpart(grade ~ ., data = training, control = tc)
rpart.model <- prune(rpart.model,cp = rpart.model$cptable[which.min(rpart.model$cptable[,"xerror"]),"CP"])
prp(rpart.model,type=2,box.palette = "auto",faclen = 0,extra=2,branch=1)

pred<-predict(rpart.model,testing,type = "class")
confusionMatrix(pred,testing$grade)

pred2<-predict(rpart.model,training,type = "class")
confusionMatrix(pred2,training$grade)

write.xlsx(training,"result_data.xlsx",sheetName = "training")
write.xlsx(testing,"result_data.xlsx",sheetName = "testing",append = T)
