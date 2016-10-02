clean <- function(df,plot=FALSE,LASSO=TRUE,ALL=TRUE){
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(psych))
  source("my_aggr.R")
  
  ##MISSING VARIABLES
  if(plot==TRUE) my_aggr(raw)#plot shows patterns of missing data in observations
  df<- raw %>%
    select(-Inc_Cat) %>%
    gather(key=response,value=rating,Electronics,Percrip_Med,Drn_Clnr,Dsps_Food) 
  if(plot==TRUE) my_aggr(df)
  
  ##COLINEARITY
  if(plot==TRUE) pairs.panels(raw)#plot shows correlation between variables
  data.ls <- list()
  data.ls$df <- df %>%
    select(-Age_Cat)#colinear with Age variable
  
  if(plot==TRUE) pairs.panels(select(df,-response))#in second plot, response var has 0 correlation
  
  if(LASSO==TRUE){
    data.ls$electronicsLASSO <- model.matrix(~.,(select(filter(df,response=="Electronics"),-response)))
    data.ls$percrip_medLASSO <- model.matrix(~.,(select(filter(df,response=="Percrip_Med"),-response)))
    data.ls$drn_clnrLASSO <- model.matrix(~.,(select(filter(df,response=="Drn_Clnr"),-response)))
    data.ls$dsps_foodLASSO <- model.matrix(~.,(select(filter(df,response=="Dsps_Food"),-response)))
  }
  return(data.ls)
}