lasso.fun <- function(mx){
  library(glmnet)
  lasso.ls <- ls()
  mod <- glmnet(mx[,-24],mx[,24],alpha=1,lambda=10^seq(10,-2, length=100))
  set.seed(1)
  lasso.ls <- cv.glmnet(mx[,-24],mx[,24],alpha=1)
  lasso.ls$coefficients <- predict(mod,type="coefficients",s=cv.out$lambda.min)
  lasso.pred<-predict(mod ,s=cv.out$lambda.min,newx=mx[,-24])
  lasso.ls$RMSE <- ((mean(lasso.pred-mx[,24]))^2)^.5  
  return(lasso.ls)
  }