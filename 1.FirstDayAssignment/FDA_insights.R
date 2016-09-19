rm(list=ls())
### Import Data
library(dplyr)
AdData <- tbl_df(read.csv("1_FDA_AdData.csv")) %>%
  mutate(Sales = Sales * 1000,AdSpending = rowSums(.[1:3]),row_number=1:n())

###Sales Budget, in thousands of dollars: $2.8 billion
sum(AdData$Sales)
### number of stores: 200
nrow(AdData)
### number of stores by region
table(AdData$Market)

### Sales vary by market
library(ggplot2)
p1 <- ggplot(AdData,aes(x=Sales,fill=Market)) + geom_histogram(position="dodge") + facet_grid(Market~.) + theme(legend.position="none")

### ad spending varies by market:
p2 <- ggplot(AdData,aes(x=AdSpending,fill=Market)) + geom_histogram(position="dodge") + facet_grid(Market~.) + theme(legend.position="none")

library(gridExtra)
grid.arrange(p1,p2,ncol=2)

### plot sales by value, grouped by store, faceted by market and geographic type
library(reshape2)
AdData.melt <- melt(AdData2,id=c("row_number","Market","Sales"));tbl_df(AdData.melt)
ggplot(AdData.melt,aes(x=value,y=Sales,color=Market)) + geom_point() + stat_smooth() + facet_grid(Market~variable,scales="free") 
## plot 
ggplot(AdData.melt,aes(x=value,fill=Market)) + geom_histogram(position="dodge") + facet_grid(Market~variable)

### percent of advertising allocated within geographic types
# AdData %>%
#   group_by(Market) %>%
#   summarise_all(funs(sum=sum)) %>%
#   mutate(AdSum=TV_sum+Radio_sum+Newspaper_sum) %>%
#   transmute(Market=Market,TV_percent=TV_sum/AdSum,Radio_percent=Radio_sum/AdSum,Newspaper_percent=Newspaper_sum/AdSum)

###--- Most efficient type of advertising per market
All.mod <- lm(Sales~.,data=AdData[,])
Rural.mod <- lm(Sales~poly(TV,2)+Radio+Newspaper,data=AdData[AdData$Market=="Rural",-5])
Urban.mod <- lm(Sales~.,data=AdData[AdData$Market=="Urban",-5])
SubUrban.mod <- lm(Sales~.,data=AdData[AdData$Market=="SubUrban",-5])
interaction.mod <- lm(Sales~ TV:Market + Radio:Market + Newspaper:Market,data=AdData)

library(stargazer)
stargazer(Rural.mod,Urban.mod,SubUrban.mod,All.mod,interaction.mod,type="text",column.labels = c("Rural","Urban","SubUrban","All","Interaction"))

stargazer(All.mod,interaction.mod,type="text")
library(corrplot)
AdMatrix<-model.matrix(Sales~.,AdData)[,-1]
corrplot(cor(cbind(AdData$Sales,mutate(data.frame(AdMatrix),MarketRural=ifelse(MarketSubUrban==1 | MarketUrban==1,0,1)))),
         type="lower",diag=FALSE,tl.srt=25,tl.col="black")
