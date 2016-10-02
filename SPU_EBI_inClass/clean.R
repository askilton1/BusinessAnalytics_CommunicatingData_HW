clean <- function(df,plot=FALSE){
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

df <- df %>%
  select(-Age_Cat)#colinear with Age variable

if(plot==TRUE) pairs.panels(select(df,-response))#in second plot, response var has 0 correlation

return(df)
}