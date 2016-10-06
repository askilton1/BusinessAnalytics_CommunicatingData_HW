#remove all objects; detach all packages; detach EBI data
###########################
rm(list = ls(all = TRUE))
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""), detach, character.only = TRUE, unload = TRUE)
while(1==1){detach(EBI)}
###########################

source("functions/clean.R")
source("functions/rmse_fun2.R")
library(stargazer)

EBI <- clean(read.csv("2_EBI_Data.csv"))
attach(EBI)

mod1 <- lm(rating~.,data=data.frame(electronics))
mod2 <- lm(rating~.,data=data.frame(percrip_med))
mod3 <- lm(rating~.,data=data.frame(drn_clnr))
mod4 <- lm(rating~.,data=data.frame(dsps_food))
modAll <- lm(rating~.,data=data.frame(model.matrix(~.,completeData)))

rmse.out <- rmse_fun(electronics,percrip_med,drn_clnr,dsps_food,model.matrix(~.,completeData))
stargazer(mod1, mod2, mod3, mod4,modAll,
          type="text",no.space=TRUE,
          column.labels=c("Electronics",
                          "Rx",
                          "Drain Cleaner",
                          "Food Disposal",
                          "All"),
          omit.stat = c("f","ser","rsq","adj.rsq"),
          add.lines = list(c("CV RMSE",rmse.out$rmse),
                            c("CV RMSE SE",rmse.out$rmseSE))
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
