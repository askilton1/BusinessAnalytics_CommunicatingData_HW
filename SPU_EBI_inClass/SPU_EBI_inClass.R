#Kyler Adams and Antonio Skilton

library(dplyr)

data <- read.csv("2_EBI_Data.csv")

library(reshape2)
data.melt <- melt(data,id=names(data)[1:10])

data.melt <- na.omit(data.melt)
library(ggplot2)
ggplot(data.melt,aes(x=value,fill=variable)) + 
  geom_bar(stat="count",position="dodge",width=.75) + 
  facet_grid(Dwelling~.,scales="free")
