clean <- function(raw_data,gather=FALSE,cleanPlots=FALSE,LASSO=TRUE,ALL=TRUE){
  suppressMessages(library(dplyr))
  suppressMessages(library(tidyr))
  suppressMessages(library(psych))
  source("functions/my_aggr.R")
  
  raw_data <- tbl_df(raw_data)
  
  ##MISSING VARIABLES
  if(cleanPlots==TRUE) my_aggr(raw_data)#plot shows patterns of missing data in observations
  
  if(gather==TRUE){
    df <- raw_data %>%
      select(-Inc_Cat) %>% #missing too many observations!
      gather(key=response,value=rating,Electronics,Percrip_Med,Drn_Clnr,Dsps_Food) %>%
      mutate(response <- as.factor(response)) %>%
      mutate(Education = factor(Education, levels = c("Non-high school graduate","High school diploma / GED", 
                                                      "Some college", "Associates Degree", "Bachelor's Degree", 
                                                      "Some graduate work", "Graduate degree(s)", 
                                                      "Missing / Prefer not to answer")))
  } else {
    df <- raw_data %>%
      select(-Inc_Cat) %>%
      mutate(Education = factor(Education, levels = c("Non-high school graduate","High school diploma / GED", 
                                                      "Some college", "Associates Degree", "Bachelor's Degree", 
                                                      "Some graduate work", "Graduate degree(s)", 
                                                      "Missing / Prefer not to answer")))
  }
  
  if(cleanPlots==TRUE) my_aggr(df)
  
  ##COLINEARITY
  if(cleanPlots==TRUE) pairs.panels(raw_data)#plot shows correlation between variables
  
  data.ls <- list()
  
  df <- df %>%
    select(-Age_Cat) #colinear with Age variable
  
  data.ls$completeData <- na.omit(df)
    
  if(cleanPlots==TRUE) pairs.panels(select(df,-response))#in second plot, response var has 0 correlation
  
  if(LASSO==TRUE){
    if(gather==FALSE) df <- df %>% gather(key=response,value=rating,Electronics,Percrip_Med,Drn_Clnr,Dsps_Food) %>% mutate(response <- as.factor(response))
    data.ls$completeData <- na.omit(df)
      data.ls$electronics <- model.matrix(~.,select(filter(df,response=="Electronics"),-response))
      data.ls$percrip_med <- model.matrix(~.,select(filter(df,response=="Percrip_Med"),-response))
      data.ls$drn_clnr <- model.matrix(~.,select(filter(df,response=="Drn_Clnr"),-response))
      data.ls$dsps_food <- model.matrix(~.,select(filter(df,response=="Dsps_Food"),-response))
  }
  return(data.ls)
}