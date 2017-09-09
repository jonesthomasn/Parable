library(Amelia)
library(brglm)

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

traindata<-logistic[logistic$Dates<as.Date('2011-01-01','%Y-%m-%d'),]
testdata<-logistic[logistic$Dates>=as.Date('2011-01-01','%Y-%m-%d'),]

traindata<-traindata[,c(1:20,22)]
testdata<-testdata[,c(1:20,22)]

for(i in names(traindata))
{
  traindata[[i]][is.na(traindata[[i]])]<-mean(traindata[[i]],na.rm = T)
}
for(i in names(traindata))
{
  traindata[[i]][is.na(testdata[[i]])]<-mean(testdata[[i]],na.rm = T)
}

trainset <- data.matrix(traindata[,1:20])
trainoutcomes<-data.matrix(traindata[,21])

model<-brglmFit(x=trainset,y=trainoutcomes,family=binomial)