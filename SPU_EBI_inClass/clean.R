library(dplyr)
source("my_aggr.R")

data <- tbl_df(read.csv("2_EBI_Data.csv"))

#for each measure of customer environmental behavior:
#1. filter for observations where measure is not missing
#2. remove Inc_Cat; >10% of ovservations are missing only that variable
#3. remove all remaining observations that are missing variables
my_aggr(Dsps_Food.data)

##### Dsps_Food
data.list <- list("Electronics"=NULL,"Percrip_Med"=NULL,"Drn_Clnr"=NULL,"Dsps_Food"=NULL)
i <- 1
for(var in names(data)[11:14]){
  data.list[[i]] <- data %>%
    filter_(!is.na(var)) %>%
    select(-Inc_Cat) %>% #10.9% of observations missing only Inc_Cat
    na.omit
  i <- i+1
}