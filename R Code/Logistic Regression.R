library(Amelia)
library(logistf)
library(corrplot)
library(pscl)
library(caret)
library(ROSE)
library(rms)
library(ada)

setwd("~/Documents/GitHub/Parable")
#-------------------------------------------------------------------------------
#If using pre-made .rds files, uncomment lines 60,61 and start execution at line 60
#
#-------------------------------------------------------------------------------
dealdata<-readRDS("./RDS/dealdata.rds")
factordata<-readRDS("./RDS/factordatamerged1.rds")
factordata2<-readRDS("./RDS/newRawData.rds")

colnames(factordata2)[4] <- colnames(factordata)[5]
factordata<-rbind(factordata,factordata2)


colnames(dealdata)[1]<-'Ticker'
dealdata$modTicker<-gsub('\\s+.*','',dealdata$Ticker)
factordata$modTicker<-gsub('\\s+.*','',factordata$Ticker)

data<-merge(x=factordata,y=dealdata[,c(1,2,4,8)],by='modTicker',all.x=TRUE)

truedata<-data[!duplicated(data),]
truedata$`Announce Date`<-as.Date(truedata$`Announce Date`)

isbought<-c(!is.na(truedata$`Announce Date`) & (truedata$`Announce Date` - truedata$Dates)<92 & (truedata$`Announce Date` - truedata$Dates)>0)

truedata$date_diff<-truedata$`Announce Date` - truedata$Dates
truedata$bought<-isbought

y<-data.frame(cbind(truedata$bought,isbought))


logistic<-truedata[,c(3:41,46)]
logistic$bought[logistic$bought==T]<-1
logistic$bought[logistic$bought==F]<-0

truedata$bought[truedata$bought==T]<-1
truedata$bought[truedata$bought==F]<-0

colnames(logistic)<-c('Dates','Price','DebtPS','Debt2Eq','CurrentR','FCFPS','PE','PS','PB','EV2EBITDA',
                      'PM','EBITDA','Cash','MinorityInt','MkCap','Leverage','RevGrowth','ROIC',
                      'CFO','CAPEX','DilEPS','R3MEQI','BICSL1Name','BICSL1Code','ICBSupSecNo',
                      'ICBIndNo','ICBSubSecNo','ICBSecNo','CUSIP','NDebt2CF','NI2Profit','AcRec1yGr',
                      'InvDays','TDebt1yGr','d200Pxchange','BV5yGr','DoInc','CEOGender','CEOTenure',
                      'Bought')

colnames(truedata)<-c('ModTicker','Full Ticker','Dates','Price','DebtPS','Debt2Eq','CurrentR','FCFPS','PE','PS','PB','EV2EBITDA',
                      'PM','EBITDA','Cash','MinorityInt','MkCap','Leverage','RevGrowth','ROIC',
                      'CFO','CAPEX','DilEPS','R3MEQI','BICSL1Name','BICSL1Code','ICBSupSecNo',
                      'ICBIndNo','ICBSubSecNo','ICBSecNo','CUSIP','NDebt2CF','NI2Profit','AcRec1yGr',
                      'InvDays','TDebt1yGr','d200Pxchange','BV5yGr','DoInc','CEOGender','CEOTenure',
                      'Full Ticker','Announced Data','Sector','DateDiff','Bought')

logistic<-logistic[ , !(names(logistic) %in% c('CEOGender','DoInc','ICBSubSecNo','BICSL1Code','BICSL1Name','CAPEX'))]
logistic$Price<-as.numeric(logistic$Price)

traindata<-logistic[logistic$Dates<as.Date('2011-01-01','%Y-%m-%d'),]
testdata<-logistic[logistic$Dates>=as.Date('2011-01-01','%Y-%m-%d') & logistic$Dates<as.Date('2016-09-30','%Y-%m-%d'),]
predictdata<-logistic[logistic$Dates>=as.Date('2016-09-30','%Y-%m-%d'),]
rawpredict<-truedata[truedata$Dates>=as.Date('2016-09-30','%Y-%m-%d'),]

traindata<-traindata[,!(names(traindata) %in% c('Dates'))]
testdata<-testdata[,!(names(testdata) %in% c('Dates'))]
predictdata<-predictdata[,!(names(predictdata) %in% c('Dates'))]

for(i in names(traindata))
{
  traindata[[i]][!is.finite(traindata[[i]])]<-mean(traindata[[i]][is.finite(traindata[[i]])])
}
for(i in names(testdata))
{
  testdata[[i]][!is.finite(testdata[[i]])]<-mean(testdata[[i]][is.finite(testdata[[i]])])
}
for(i in names(predictdata))
{
  predictdata[[i]][!is.finite(predictdata[[i]])]<-mean(predictdata[[i]][is.finite(predictdata[[i]])])
}

traindata<-traindata[!(traindata$ICBSecNo %in% c(2790,1750,2720,2730,3760,570,3720,1770,3530,3350,2750,2710,2350,7570,3780,8980)),]
testdata<-testdata[!(testdata$ICBSecNo %in% c(2790,1750,2720,2730,3760,570,3720,1770,3530,3350,2750,2710,2350,7570,3780,8980)),]
predictdata<-predictdata[!(predictdata$ICBSecNo %in% c(2790,1750,2720,2730,3760,570,3720,1770,3530,3350,2750,2710,2350,7570,3780,8980)),]
rawpredict<-rawpredict[!(rawpredict$ICBSecNo %in% c(2790,1750,2720,2730,3760,570,3720,1770,3530,3350,2750,2710,2350,7570,3780,8980)),]


traindata<-traindata[traindata$MkCap<1e10,]
testdata<-testdata[testdata$MkCap<1e10,]
predictdata<-predictdata[predictdata$MkCap<1e10,]
rawpredict<-rawpredict[rawpredict$MkCap <1e10,]

#traindata<-traindata[(traindata$ICBSecNo %in% c(5550,6570,580,9530,3570,8990,6530,8630,5750,5330)),]
#testdata<-testdata[(testdata$ICBSecNo %in% c(5550,6570,580,9530,3570,8990,6530,8630,5750,5330)),]


saveRDS(logistic,'./RDS2/regressiondata.rds')
saveRDS(testdata,'./RDS2/testingdata.rds')
saveRDS(traindata,'./RDS2/trainingdata.rds')
saveRDS(rawpredict,'./RDS2/rawpredictdata.rds')
saveRDS(predictdata,'./RDS2/predictdata.rds')

#------------------------------------------------------------------------------------------
logistic<-readRDS('./RDS2/regressiondata.rds')
traindata<-readRDS("./RDS2/trainingdata.rds")
testdata<-readRDS("./RDS2/testingdata.rds")
rawpredict<-readRDS("./RDS2/rawpredictdata.rds")
predictdata<-readRDS("./RDS2/predictdata.rds")

traindata$Bought<-factor(traindata$Bought)
testdata$Bought<-factor(testdata$Bought)
predictdata$Bought<-factor(predictdata$Bought)
traindataOver<-ovun.sample(Bought~.,data=traindata,method='over',N=300000)$data

#missmap(traindata, main = "Missing values vs observed")

model<-logistf(
  formula = Bought~Price+DebtPS+Debt2Eq+CurrentR+FCFPS+PE+PS+PB+EV2EBITDA+PM+EBITDA+Cash+MinorityInt+MkCap+Leverage+RevGrowth+ROIC+CFO+DilEPS
  ,data=traindataOver)

modellogit<-glm(
  formula = Bought~.
  ,data=traindata, family=binomial(logit))

modellogit3<-ada(
  formula = Bought~.
  ,data=traindataOver, loss="logistic",iter=20)

check<-data.frame(round(fitted(modellogit)),traindataOver$Bought)
#-----------------------------------------------------------------------
library(randomForest)
industries<-unique(traindata$ICBSecNo)
rlist<-readRDS('./RDS/indovrforest.rds')

rmodel<-randomForest(Bought~., data=traindata,classwt=c(1,20))

i<-1
rlist<-list()
for(industry in industries)
{
  rlist[[i]]<-randomForest(Bought~.,data=traindata[traindata$ICBSecNo==industry,],ntree=1000,mtry=9,importance=TRUE)
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

#---------------------------------------------------------------------

backtest<-predict(modellogit,newdata=testdata,type='response')
backtest<-round(backtest)

betas <- coef(model)
X<-model.matrix(model, data=traindata) 
backtest<-round(1 / (1 + exp(-X %*% betas)))

compare<-cbind(bought,training)
#-----------------------------------------------------------------------
library(isofor)
isomodel<-iForest( X=traindata,nt=1000,phi=128)
fitpreds = predict(isomodel,newdata=traindata,type="response")
fitpred = prediction(fitpreds,testdata$Bought)

#saveRDS(fitpreds,'./RDS2/isoForTrain.rds')
#saveRDS(fitpreds,'./RDS2/isoForTest.rds')

fitperf = performance(fitpred,"tpr","fpr")
repperf = performance(fitpred,"prec","rec")
auc.tmp <- performance(fitpred,"auc"); 
print(as.numeric(auc.tmp@y.values)*100)
f <- approxfun(data.frame(repperf@x.values , repperf@y.values) ) 
auc <- integrate(f, 0, 1)$value
print(auc-0.01815110344)


#train 0.0138297431, test 
plot(fitperf,col="green",lwd=2,main="ROC Curve for Logistic Regression")
abline(a=0,b=1,lwd=2,lty=2,col="gray")
plot(repperf,col="blue",lwd=2,main="Precision-Recall Curve for Logistic REg")

#-----------------------------------------------------------------------
library(ROCR)
fitpreds = predict(isomodel,newdata=predictdata,type="response")

rawpredict$predicted<-round(fitpreds)
ouput<-data.frame(rawpredict$`Full Ticker`,rawpredict$Dates,rawpredict$predicted)
ouput<-ouput[ouput$rawpredict.predicted==1,]
tickers<-ouput[!duplicated(ouput$rawpredict..Full.Ticker.), ]
finaltickers<-tickers[tickers$rawpredict.Dates>as.Date('2017-06-30','%Y-%m-%d'),]

fitpred = prediction(fitpreds,testdata$Bought)
fitperf = performance(fitpred,"tpr","fpr")
plot(fitperf,col="green",lwd=2,main="ROC Curve for Logistic:  Bought")
abline(a=0,b=1,lwd=2,lty=2,col="gray")