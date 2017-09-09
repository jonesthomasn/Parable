library(readxl)
library(dplyr)

setwd("~/Documents/GitHub/Parable/Deal Raw")
file.list <- list.files(pattern='*.xlsx')
dflist <-lapply(file.list, read_excel)

dflist2<-cbind(dflist)
dflist3<-cbind(dflist2)

dflist2<-bind_rows(dflist)

dflist3$`Target Industry Sector`<-factor(dflist2$`Target Industry Sector`)
dflist3$`Deal Type`<-factor(dflist2$`Deal Type`)
dflist3$`Payment Type`<-factor(dflist2$`Payment Type`)
dflist3$`Deal Status`<-factor(dflist2$`Deal Status`)

dataD <-cbind(dflist3)
saveRDS(dataD,'dealdata.rds')
