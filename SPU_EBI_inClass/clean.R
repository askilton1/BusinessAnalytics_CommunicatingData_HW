clean <- function(raw_data,plot=FALSE,LASSO=TRUE,ALL=TRUE){
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(psych))
  source("functions/my_aggr.R")
  
  raw_data <- tbl_df(raw_data)
  
  ##MISSING VARIABLES
  if(plot==TRUE) my_aggr(raw_data)#plot shows patterns of missing data in observations
  
  df <- raw_data %>%
    select(-Inc_Cat) %>% #missing too many observations!
    gather(key=response,value=rating,Electronics,Percrip_Med,Drn_Clnr,Dsps_Food) %>%
    mutate(Education = factor(Education, levels = c("Non-high school graduate","High school diploma / GED", 
                                                    "Some college", "Associates Degree", "Bachelor's Degree", 
                                                    "Some graduate work", "Graduate degree(s)", 
                                                    "Missing / Prefer not to answer")))
  if(plot==TRUE) my_aggr(df)
  
  ##COLINEARITY
  if(plot==TRUE) pairs.panels(raw_data)#plot shows correlation between variables
  
  data.ls <- list()
  
  df <- df %>%
    select(-Age_Cat) #colinear with Age variable
    
  
  data.ls$completeData <- na.omit(df)
    
  if(plot==TRUE) pairs.panels(select(df,-response))#in second plot, response var has 0 correlation
  
  if(LASSO==TRUE){
    data.ls$electronics <- model.matrix(~.,select(filter(df,response=="Electronics"),-response,-Age_Cat))
    data.ls$percrip_med <- model.matrix(~.,select(filter(df,response=="Percrip_Med"),-response,-Age_Cat))
    data.ls$drn_clnr <- model.matrix(~.,select(filter(df,response=="Drn_Clnr"),-response,-Age_Cat))
    data.ls$dsps_food <- model.matrix(~.,select(filter(df,response=="Dsps_Food"),-response,-Age_Cat))
  }
  return(data.ls)
}