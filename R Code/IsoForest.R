library(Amelia)
library(logistf)
library(corrplot)
library(pscl)
library(caret)
library(ROSE)
library(rms)
library(ada)
library(isofor)

setwd("~/Documents/GitHub/Parable")

logistic<-readRDS('./RDS2/regressiondata.rds')
traindata<-readRDS("./RDS2/trainingdata.rds")
testdata<-readRDS("./RDS2/testingdata.rds")
rawpredict<-readRDS("./RDS2/rawpredictdata.rds")
predictdata<-readRDS("./RDS2/predictdata.rds")

traindata$Bought<-factor(traindata$Bought)
testdata$Bought<-factor(testdata$Bought)
predictdata$Bought<-factor(predictdata$Bought)

#----------------------------------------------------------------------
#General Model

isomodel<-iForest( X=traindata,nt=1000,phi=128)
fitpreds = predict(isomodel,newdata=testdata,type="response")
fitpred = prediction(fitpreds,testdata$Bought)

fitperf = performance(fitpred,"tpr","fpr")
repperf = performance(fitpred,"prec","rec")
auc.tmp <- performance(fitpred,"auc"); 
print(as.numeric(auc.tmp@y.values)*100)
f <- approxfun(data.frame(repperf@x.values , repperf@y.values) ) 
auc <- integrate(f, 0, 1)$value
print(auc-0.01815110344)

#train 0.0138297431, test  0.01815110344
plot(fitperf,col="green",lwd=2,main="ROC Curve for Logistic Regression")
abline(a=0,b=1,lwd=2,lty=2,col="gray")
plot(repperf,col="blue",lwd=2,main="Precision-Recall Curve for Logistic REg")

fitpreds = predict(isomodel,newdata=predictdata,type="response")

rawpredict$predicted<-round(fitpreds)
ouput<-data.frame(rawpredict$`Full Ticker`,rawpredict$Dates,rawpredict$predicted)
ouput<-ouput[ouput$rawpredict.predicted==1,]
tickers<-ouput[!duplicated(ouput$rawpredict..Full.Ticker.), ]
finaltickers<-tickers[tickers$rawpredict.Dates>as.Date('2017-06-30','%Y-%m-%d'),]

#--------------------------------------------------------------------------------
#Industry model

i<-1
rlist<-list()
for(industry in industries)
{
  rlist[[i]]<-iForest(X=traindata[traindata$ICBSecNo==industry,],nt=1000,phi=128)
  i<-i+1
  print(i)
}

i<-1
predts<-list()

for(industry in industries)
{
  predts[[i]]<-predict(rlist[[i]],newdata=testdata[testdata$ICBSecNo==industry,],type='response')
  i<-i+1
  print(i)
}

i<-1
fitpred<-list()

for(industry in industries)
{
  print(i)
  testing <- testdata[testdata$ICBSecNo==industry,]$Bought
  fitpred<-prediction(predts[i],testing)
  fitperf = performance(fitpred,"tpr","fpr")
  auc.tmp <- performance(fitpred,"auc"); 
  print(as.numeric(auc.tmp@y.values)*100)
  i<-i+1
}

j=15
print(industries[j])
testing <- testdata[testdata$ICBSecNo==industries[j],]$Bought
fitpred<-prediction(predts[j],testing)
fitperf = performance(fitpred,"tpr","fpr")
repperf = performance(fitpred,"prec","rec")
auc.tmp <- performance(fitpred,"auc"); 
print(as.numeric(auc.tmp@y.values)*100)
f <- approxfun(data.frame(repperf@x.values , repperf@y.values) ) 
auc <- integrate(f, 0.3, 0.9)$value
print(auc-0.01815110344)


plot(fitperf,col="green",lwd=2,main="ROC Curve for Isolation Forest: Life Insurance")
abline(a=0,b=1,lwd=2,lty=2,col="gray")
plot(repperf,col="blue",lwd=2,main="Precision-Recall Curve for Isolation Forest: Life Insurance")

#-----------------------------------------------------------------------------
#Helper Functions

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}

isomodel<-iForest( X=traindata,nt=1000,phi=128)
fitpreds = predict(isomodel,newdata=testdata,type="response")
fitpred = prediction(fitpreds,testdata$Bought)
fitperf = performance(fitpred,"tpr","fpr")

print(opt.cut(fitperf, fitpred))
