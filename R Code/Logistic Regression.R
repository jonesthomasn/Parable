library(Amelia)
library(logistf)
library(corrplot)
library(pscl)
library(caret)
library(ROSE)

setwd("~/Documents/GitHub/Parable")
#-------------------------------------------------------------------------------
#If using pre-made .rds files, uncomment lines 60,61 and start execution at line 60
#
#-------------------------------------------------------------------------------
dealdata<-readRDS("./RDS/dealdata.rds")
factordata<-readRDS("./RDS/factordatamerged1.rds")

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

colnames(logistic)<-c('Dates','Price','DebtPS','Debt2Eq','CurrentR','FCFPS','PE','PS','PB','EV2EBITDA',
                      'PM','EBITDA','Cash','MinorityInt','MkCap','Leverage','RevGrowth','ROIC',
                      'CFO','CAPEX','DilEPS','R3MEQI','BICSL1Name','BICSL1Code','ICBSupSecNo',
                      'ICBIndNo','ICBSubSecNo','ICBSecNo','CUSIP','NDebt2CF','NI2Profit','AcRec1yGr',
                      'InvDays','TDebt1yGr','d200Pxchange','BV5yGr','DoInc','CEOGender','CEOTenure',
                      'Bought')

logistic<-logistic[ , !(names(logistic) %in% c('CEOGender','DoInc','ICBSubSectorNo','BICSL1Code','BICSL1Name','CAPEX'))]
traindata<-logistic[logistic$Dates<as.Date('2011-01-01','%Y-%m-%d'),]
testdata<-logistic[logistic$Dates>=as.Date('2011-01-01','%Y-%m-%d'),]

traindata<-traindata[,!(names(traindata) %in% c('Dates'))]
testdata<-testdata[,!(names(testdata) %in% c('Dates'))]

for(i in names(traindata))
{
  traindata[[i]][!is.finite(traindata[[i]])]<-mean(traindata[[i]][is.finite(traindata[[i]])])
}
for(i in names(testdata))
{
  testdata[[i]][!is.finite(testdata[[i]])]<-mean(testdata[[i]][is.finite(testdata[[i]])])
}

#------------------------------------------------------------------------------------------
#traindata<-readRDS("./RDS/trainingdata.rds")
#testdata<-readRDS("./RDS/testingdata.rds")

traindataOver<-ovun.sample(Bought~.,data=traindata,method='over',N=260000)$data

#missmap(traindata, main = "Missing values vs observed")

model<-logistf(
  formula = Bought~Price+DebtPS+Debt2Eq+CurrentR+FCFPS+PE+PS+PB+EV2EBITDA+PM+EBITDA+Cash+MinorityInt+MkCap+Leverage+RevGrowth+ROIC+CFO+DilEPS
  ,data=traindata)

modellogit<-glm(
  formula = Bought~Price+DebtPS+Debt2Eq+CurrentR+FCFPS+PE+PS+PB+EV2EBITDA+PM+EBITDA+Cash+MinorityInt+MkCap+Leverage+RevGrowth+ROIC+CFO+DilEPS
  ,data=traindata, family=binomial)


check<-data.frame(round(fitted(modellogit)),traindata$Bought)
#-----------------------------------------------------------------------

backtest<-predict(modellogit,newdata=testdata,type='response')
backtest<-round(backtest)

betas <- coef(model)
X<-model.matrix(model, data=traindata) 
backtest<-round(1 / (1 + exp(-X %*% betas)))

compare<-cbind(bought,training)