source("clean.R")
df <- clean(read.csv("2_EBI_Data.csv"))
library(stargazer)
stargazer(lm(rating~.,data=select(filter(df,response=="Electronics"),-response)),
          lm(rating~.,data=select(filter(df,response=="Percrip_Med"),-response)),
          lm(rating~.,data=select(filter(df,response=="Drn_Clnr"),-response)),
          lm(rating~.,data=select(filter(df,response=="Dsps_Food"),-response)),
          type="text",no.space=TRUE)
?stargazer
