library(forecast)
library(rminer)

call_arima <- function(dataset){
  dataset <- dataset[,1]
  L <- length(dataset)
  K = 7
  Test = K
  S = 1
  Runs = 7
  
  W = (L-Test)-(Runs-1)*S
  
  timelags = c(1, 14, 21, 28)
  D = CasesSeries(dataset, timelags)
  W2 = W-max(timelags)
  
  for(i in 1:Runs){
    H = holdout(dataset, ratio = Test, mode = "rolling", iter = i, window = W, increment = S)
    trinit= H$tr[4]
  }
  
  return(trinit)
}