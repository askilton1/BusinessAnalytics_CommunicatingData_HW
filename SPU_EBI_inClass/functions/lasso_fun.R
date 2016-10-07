lasso.fun <- function(mx){
  library(glmnet)
  lasso.ls <- ls()
  mod <- glmnet(mx[, -dim(mx)[2]], mx[, dim(mx)[2]], alpha=1, lambda=10^seq( 10, -2, length=100))
  set.seed(1)
  cv.out <- cv.glmnet(mx[,-dim(mx)[2]],mx[,dim(mx)[2]],alpha=1)
  lasso.ls$coefficients <- predict(mod,type="coefficients",s=cv.out$lambda.min)
  lasso.pred<-predict(mod ,s=cv.out$lambda.min,newx=mx[,-dim(mx)[2]])
  lasso.ls$RMSE <- ((mean(lasso.pred-mx[,dim(mx)[2]]))^2)^.5  
  return(lasso.ls)
  }