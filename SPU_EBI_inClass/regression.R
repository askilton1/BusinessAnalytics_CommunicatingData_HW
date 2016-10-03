source("clean.R")
library(stargazer)

EBI <- clean(read.csv("2_EBI_Data.csv"))
attach(EBI)

stargazer(lm(rating~.,data=data.frame(electronics)),
          lm(rating~.,data=data.frame(percrip_med)),
          lm(rating~.,data.frame(drn_clnr)),
          lm(rating~.,data.frame(dsps_food)),
          lm(rating~.,data=data.frame(model.matrix(~.,completeData))),
          type="text",no.space=TRUE,
          column.labels=c("Electronics",
                          "Rx",
                          "Drain Cleaner",
                          "Food Disposal",
                          "All"),
          omit.stat = c("f","ser","rsq","adj.rsq")
          )

##LASSO variable selection
source("lasso_fun.R")
lassoCoefficients <- data.frame(
  "Electronics" = as.numeric(lasso.fun(electronics)$coefficients),
  "Percrip_Med" = as.numeric(lasso.fun(percrip_med)$coefficients),
  "Drn_Clnr" = as.numeric(lasso.fun(drn_clnr)$coefficients),
  "Dsps_Food" = as.numeric(lasso.fun(dsps_food)$coefficients))
lassoCoefficients <- lassoCoefficients[-2,]
row.names(lassoCoefficients) <-rownames((lasso.fun(electronics)$coefficients))[-2]
lassoCoefficients <- round(lassoCoefficients,3)
rownames((lasso.fun(electronics)$coefficients))
lassoCoefficients[lassoCoefficients == 0] <- ""
RMSE <- c(
  lasso.fun(electronics)$RMSE,
  lasso.fun(percrip_med)$RMSE,
  lasso.fun(drn_clnr)$RMSE,
  lasso.fun(dsps_food)$RMSE)
rbind(lassoCoefficients,"RMSE" = signif(RMSE,3))
