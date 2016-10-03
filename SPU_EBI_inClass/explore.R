library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
EBI <- tbl_df(read.csv("2_EBI_Data.csv"))
hist(unique(EBI$Age))


EBI %>%
  gather(key=response,value=rating,Electronics,Percrip_Med,Drn_Clnr,Dsps_Food) %>%
  ggplot(aes(x=Age)) + #custom plot for the only contiuous var: Age
    geom_density(aes(fill=response),position="dodge",alpha=0.5) + 
    #geom_density(aes(color=rating)) + 
    facet_grid(response~rating,scales = "free")

factorNames <- c("Age_Cat","Education","Dwelling") #names of interesting factor vars
plist<-lapply(factorNames,function(x){
  p <- EBI %>%
    gather(key=response, #transform customer rating vars so kind of pollution
           value=rating, #is in one column, and rating is in another
           Electronics,
           Percrip_Med,
           Drn_Clnr,
           Dsps_Food) %>%
    group_by_(x,"response") %>%
    na.omit() %>% #placing this after group_by_() reduces number of observations omitted
    summarise(mean=mean(rating),se=sd(rating)) %>%
    ggplot(aes(x=response,y=mean,fill=response)) + 
      geom_bar(stat="identity") + 
      geom_errorbar(aes(ymax = mean + se, ymin = mean - se), #only a 68% CI interval
                    position=position_dodge(width=0.9),width=0.25) + 
      facet_grid(reformulate(x),scales = "free") +
      theme_minimal() + 
      theme(
            legend.position="bottom")
  if(x!=factorNames[length(factorNames)]){ #if variable is not the last variable
    p <- p + theme(axis.text.x=element_blank(),
                   legend.position="none")} else { #do not include legend, or else if last var,
                     p <- p + theme(axis.text.x=element_blank(),
                                    legend.position="bottom") #put legend on the bottom
                   }
  }
)
n <- length(plist)
nCol <- floor(sqrt(n))
m1 <- do.call("grid.arrange", c(plist, ncol=1))
ggsave("multiplot.pdf",m1)
