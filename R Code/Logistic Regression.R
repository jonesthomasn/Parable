library(Amelia)
library(logistf)
library(corrplot)
library(pscl)

setwd("~/Documents/GitHub/Parable")
dealdata<-readRDS("./RDS/dealdata.rds")
factordata<-readRDS("./RDS/factordata.rds")

colnames(dealdata)[1]<-'Ticker'
dealdata$modTicker<-gsub('\\s+.*','',dealdata$Ticker)
factordata$modTicker<-gsub('\\s+.*','',factordata$Ticker)

data<-merge(x=factordata,y=dealdata[,c(1,2,4,8)],by='modTicker',all.x=TRUE)

truedata<-data[!duplicated(data),]
truedata$`Announce Date`<-as.Date(truedata$`Announce Date`)

isbought<-c(!is.na(truedata$`Announce Date`) & (truedata$`Announce Date` - truedata$Dates)<92 & (truedata$`Announce Date` - truedata$Dates)>0)

truedata$date_diff<-truedata$`Announce Date` - truedata$Dates
truedata$bought<-isbought

logistic<-truedata[,c(3:23,28)]
logistic$bought[logistic$bought==T]<-1
logistic$bought[logistic$bought==F]<-0

colnames(logistic)<-c('Price','DebtPS','Debt2Eq','CurrentR','FCFPS','PE','PS','PB','EV2EBITDA','PM','EBITDA','Cash','MinorityInt','MkCap','Leverage','RevGrowth','ROIC','CFO','CAPEX','DilEPS','Dates','Bought')

traindata<-logistic[logistic$Dates<as.Date('2011-01-01','%Y-%m-%d'),]
testdata<-logistic[logistic$Dates>=as.Date('2011-01-01','%Y-%m-%d'),]

traindata<-traindata[,c(1:20,22)]
testdata<-testdata[,c(1:20,22)]

for(i in names(traindata))
{
  traindata[[i]][!is.finite(traindata[[i]])]<-mean(traindata[[i]][is.finite(traindata[[i]])])
}
for(i in names(testdata))
{
  testdata[[i]][!is.finite(testdata[[i]])]<-mean(testdata[[i]][is.finite(testdata[[i]])])
}
testdata<-testdata[,c(1:18,20)]

#missmap(traindata, main = "Missing values vs observed")
model<-logistf(
  formula = Bought~Price+DebtPS+Debt2Eq+CurrentR+FCFPS+PE+PS+PB+EV2EBITDA+PM+EBITDA+Cash+MinorityInt+MkCap+Leverage+RevGrowth+ROIC+CFO+DilEPS
  ,data=traindata)

modellogit<-glm(
  formula = Bought~Price+DebtPS+Debt2Eq+CurrentR+FCFPS+PE+PS+PB+EV2EBITDA+PM+EBITDA+Cash+MinorityInt+MkCap+Leverage+RevGrowth+ROIC+CFO+DilEPS
  ,data=traindata, family=binomial)

corrplot(cor(traindata), type = 'lower',method='ellipse')

pR2(modellogit)

#-----------------------------------------------------------------------

backtest<-predict(modellogit,newdata=testdata,type='response')
backtest<-round(backtest)
bought<-logistic[logistic$Dates>=as.Date('2011-01-01','%Y-%m-%d'),22]

betas <- coef(model)
X<-model.matrix(model, data=testdata) 
backtest<-round(1 / (1 + exp(-X %*% betas)))

compare<-cbind(bought,backtest)