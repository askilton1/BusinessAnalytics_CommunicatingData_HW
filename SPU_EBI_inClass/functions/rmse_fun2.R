rmse_fun <- function(...){
  library(caret)
  datalist <- list(...)
  rmselist <- list("rmse"=c(rep(NA,length(datalist))),"rmseSE"=c(rep(NA,length(datalist))))
  for(i in 1:length(datalist)){
    trainlist<-train(datalist[[i]][,-which(names(datalist[[i]])=="rating")],datalist[[i]][,"rating"],method="lm")
    rmselist$rmse[i] <- signif(as.numeric(trainlist$results[2]),digits=3)
    rmselist$rmseSE[i] <- signif(as.numeric(trainlist$results[4]),digits=3)
  }
  return(rmselist)
}
