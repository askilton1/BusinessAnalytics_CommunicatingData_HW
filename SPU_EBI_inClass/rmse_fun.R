rmse_fun <- function(x){
library(caret)
trainlist<-train(x[,-which(names(x)=="rating")],x[,"rating"],method="lm")
returnlist <- ls()
returnlist <- list("RMSE" = trainlist$results[2],"RMSESD" = trainlist$results[4])
return(returnlist)
}

