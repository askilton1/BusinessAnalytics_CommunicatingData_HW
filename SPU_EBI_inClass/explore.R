library(dplyr)
library(tidyr)
EBI <- tbl_df(read.csv("2_EBI_Data.csv"))
hist(unique(EBI$Age))

library(ggplot2)
EBI.plot <- EBI %>%
  gather(key=response,value=rating,Electronics,Percrip_Med,Drn_Clnr,Dsps_Food) 

ggplot(EBI.plot,aes(x=Age)) + 
  geom_density(aes(fill=response),position="dodge",alpha=0.5) + 
  #geom_density(aes(color=rating)) + 
  facet_grid(response~rating,scales = "free")

for(var in names(EBI)[which(sapply(EBI,is.factor))]){
  table <- EBI %>%
      gather(key=response,value=rating,Electronics,Percrip_Med,Drn_Clnr,Dsps_Food) %>%
      group_by_(var,"response") %>%
      na.omit() %>%
      summarise(mean=mean(rating)) 
      p <- ggplot(table,aes_string(x=var,y="mean",fill=var)) + 
        geom_bar(stat="identity") +
        facet_grid(~response,scales = "free")
      test <- readline("Press enter to print next plot:")
      print(p)
}
p1 <- ls()
dodge <- position_dodge(width=0.9)
#factorNames <- names(EBI)[which(sapply(EBI,is.factor))]
factorNames <- c("Age_Cat","Education","Dwelling")
plist<-lapply(factorNames,function(x){
  EBI %>%
    gather(key=response,value=rating,Electronics,Percrip_Med,Drn_Clnr,Dsps_Food) %>%
    group_by_(x,"response") %>%
    na.omit() %>%
    summarise(mean=mean(rating),se=sd(rating)) %>%
    ggplot(aes_string(x=x,y="mean",fill=x)) + 
      geom_bar(stat="identity") +
      geom_errorbar(aes(ymax = mean + se, ymin = mean - se), position=position_dodge(width=0.9),width=0.25) + 
      facet_grid(~response,scales = "free") +
      theme_minimal() + 
      theme(axis.text.x=element_blank())}
)
library(gridExtra)
n <- length(plist)
nCol <- floor(sqrt(n))
m1 <- do.call("grid.arrange", c(plist, ncol=1))
ggsave("multiplot.pdf",m1)
