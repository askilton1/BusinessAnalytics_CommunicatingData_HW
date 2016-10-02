source("clean.R")
EBI <- clean(read.csv("2_EBI_Data.csv"))
library(stargazer)
stargazer(lm(rating~.,data=data.frame(EBI$electronicsLASSO)),
          lm(rating~.,data=data.frame(EBI$percrip_medLASSO)),
          lm(rating~.,data.frame(EBI$drn_clnrLASSO)),
          lm(rating~.,data.frame(EBI$dsps_foodLASSO)),
          lm(rating~.,data=data.frame(model.matrix(~.,EBI$df))),
          type="text",no.space=TRUE)

library(glmnet)
##LASSO variable selection
lasso.fun(EBI$electronicsLASSO)$coefficients
lasso.fun(EBI$electronicsLASSO)$RMSE


electronics.mod <- glmnet(EBI$electronicsLASSO[,-24],EBI$electronicsLASSO[,24],alpha=1,lambda=EBI$grid)
    set.seed(1)
    cv.out <- cv.glmnet(EBI$electronicsLASSO[,-24],EBI$electronicsLASSO[,24],alpha=1)
    predict(electronics.mod,type="coefficients",s=cv.out$lambda.min)
    lasso.pred<-predict(electronics.mod ,s=cv.out$lambda.min,newx=EBI$electronicsLASSO[,-24])
    ((mean(lasso.pred-EBI$electronicsLASSO[,24]))^2)^.5   
percrip_med.mod <- glmnet(EBI$electronicsLASSO[,-24],EBI$electronicsLASSO[,24],alpha=1,lambda=EBI$grid)
    set.seed(1)
    cv.out <- cv.glmnet(EBI$electronicsLASSO[,-24],EBI$electronicsLASSO[,24],alpha=1)
    predict(percrip_med.mod,type="coefficients",s=cv.out$lambda.min)
    lasso.pred<-predict(percrip_med.mod ,s=cv.out$lambda.min,newx=EBI$electronicsLASSO[,-24])
    ((mean(lasso.pred-EBI$electronicsLASSO[,24]))^2)^.5   