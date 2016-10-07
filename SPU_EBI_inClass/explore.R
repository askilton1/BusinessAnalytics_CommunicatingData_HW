library(dplyr);library(tidyr);library(ggplot2);library(gridExtra)
source("functions/clean.R")

EBI <- clean(read.csv("2_EBI_Data.csv"),gather=TRUE)$completeData

EBI %>%
  ggplot(aes(x=Age)) + #custom plot for the only contiuous var: Age
    geom_density(aes(fill=response),position="dodge",alpha=0.5) + 
    facet_grid(response~rating,scales = "free")

factorNames <- names(select_if(EBI,is.factor)) #names of interesting factor vars
plist<-lapply(factorNames,function(x){
  p <- EBI %>%
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
  })

m1 <- do.call("grid.arrange", c(plist, ncol=1))
ggsave("multiplot.pdf",m1)
