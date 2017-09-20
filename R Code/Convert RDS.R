library(readxl)
library(dplyr)

setwd("~/Documents/GitHub/Parable/Bloomberg/Bloomberg Raw New")
file.list <- list.files(pattern='*.xlsx')
dflist <-lapply(file.list, read_excel)

dflist2<-cbind(dflist)

for(i in 1:length(dflist))
{
  dflist2[[i]]<-lapply(
    dflist[[i]],
    function(x) {
    gsub('--','',x)
    })
}

datelist<-lapply(file.list,function(x) {gsub('RAY as of ','',x)})
datelist<-lapply(datelist,function(x) {gsub('[0-9]\\.xlsx','',x)})
datelist<-lapply(datelist,function(x) {gsub(' ','-',x)})
datelist<-lapply(datelist,function(x) {as.Date(x,'%b-%d-%Y')})
datelist<-do.call(c,datelist)

dflist3<-cbind(dflist2)

x<-length(dflist[[1]])

for(i in 1:length(dflist2))
{
  dflist3[[i]]$Dates<-c(rep(datelist[i],length(dflist2[[i]]$Ticker)))
}

dfmerged<-do.call(bind_rows,dflist3)
dat<-cbind(dfmerged)
dat[,2:x] <- sapply(dfmerged[,2:x], as.numeric)

data<-cbind(dat[,0:x+1])