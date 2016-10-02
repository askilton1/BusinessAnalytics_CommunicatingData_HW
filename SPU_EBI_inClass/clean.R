library(dplyr)
library(tidyr)
source("my_aggr.R")

data2 <- tbl_df(read.csv("2_EBI_Data.csv"))
##MISSING VARIABLES
data3<- data2 %>%
  select(-Inc_Cat) %>%
  gather(key=response,value=rating,Electronics,Percrip_Med,Drn_Clnr,Dsps_Food) 


##COLINEARITY
library(psych)
pairs.panels(data3)
