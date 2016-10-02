library(dplyr)
source("my_aggr.R")

data <- tbl_df(read.csv("2_EBI_Data.csv"))

#data cleaned to maxize size of pure data set
#1. select one of four measures
#2. filter out observations that have missing value for selected measure
#3. check dimensions of data with dim() 
#4. plot my_aggr() to find patterns in missing data
#5. remove variables that have many observations are missing, if necessary
#6. remove all observations that still have missing variables

##### Dsps_Food
Dsps_Food.data <- data %>%
  select(-Electronics,-Percrip_Med,-Drn_Clnr) %>%
  filter(!is.na(Dsps_Food)) 

dim(Dsps_Food.data)# 2858 rows, 11 columns after other measures removed
my_aggr(Dsps_Food.data)

Dsps_Food.data <- Dsps_Food.data %>%
  select(-Inc_Cat) %>% #10.9% of observations missing only Inc_Cat
  na.omit

dim(Dsps_Food.data)# 2588 observations, 10 variables in pure dataset

##### Electronics
Electronics.data <- data %>%
  select(-Dsps_Food,-Percrip_Med,-Drn_Clnr) %>%
  filter(!is.na(Electronics)) 

dim(Electronics.data)# 2700 rows, 11 columns after other measures removed
my_aggr(Electronics.data)

Electronics.data <- Electronics.data %>%
  select(-Inc_Cat) %>% #10.9% of observations missing only Inc_Cat
  na.omit

dim(Electronics.data)# 2415 observations, 10 variables in pure dataset

##### Percrip_Med
Percrip_Med.data <- data %>%
  select(-Electronics,-Dsps_Food,-Drn_Clnr) %>%
  filter(!is.na(Percrip_Med)) 

dim(Percrip_Med.data)# 2111 rows, 11 columns after other measures removed
my_aggr(Percrip_Med.data)

Percrip_Med.data <- Percrip_Med.data %>%
  select(-Inc_Cat) %>% #10.9% of observations missing only Inc_Cat
  na.omit

dim(Percrip_Med.data)# 1883 observations, 10 variables in pure dataset

##### Drn_Clnr
Drn_Clnr.data <- data %>%
  select(-Electronics,-Percrip_Med,-Dsps_Food) %>%
  filter(!is.na(Drn_Clnr)) 

dim(Drn_Clnr.data)# 2367 rows, 11 columns after other measures removed
my_aggr(Drn_Clnr.data)

Drn_Clnr.data <- Drn_Clnr.data %>%
  select(-Inc_Cat) %>% #10.9% of observations missing only Inc_Cat
  na.omit

dim(Drn_Clnr.data)# 2151 observations, 10 variables in pure dataset

