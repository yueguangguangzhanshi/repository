require(ISLR)
require(boot)
require(Daim)
Module_ROC<-read.xlsx("ROC.xlsx",sheetIndex = 1)
M<-roc(Module_ROC[,2:59],Module_ROC[,60],"1")
plot(M)
roc.area(M)
summary(M)

#????ѵ��????????logistic/glm?ع?
train<-sample(49,25)
lm.fit<-lm(category~.,data = lm_raw_data,subset = train,family = binomial())
summary(lm.fit)
plot(lm.fit)
require(gvlma)
gvmodel<-gvlma(lm.fit)
summary(gvmodel)


#???±?��????ROC????
N<-roc(ROC_3model_data[,2],ROC_3model_data[,1],"1")
roc.area(N)
plot(N)
summary(N)

#????Corss-Validation??֤????
lm.fit2<-glm(category~model,data = ROC_3model_data)
cv.error<-cv.glm(ROC_3model_data, lm.fit2)$delta[1]
cv.error
match_data<-lm_raw_data[train,]
require(xlsx)
write.xlsx(match_data,"3model_data_train.xlsx")
