library(vars)
library(rminer)
library(forecast)

source("multi-utils.R")

call_mlpe <- function(Num_Customers, timelags){
  CasesSeriesObject <- CasesSeries(Num_Customers, timelags)
  K <- 7
  StepJump <- round(K/3)
  Runs <- 7
  WindowSize <- (nrow(CasesSeriesObject) - K) - (Runs - 1) * StepJump - max(timelags)
  
  predictions <- vector("list", Runs)
  actuals <- vector("list", Runs)
  
  for(i in 1:Runs){
    HoldoutObject <- holdout(CasesSeriesObject$y,
                             ratio = K,
                             mode = "incremental",
                             iter = i,
                             window = WindowSize,
                             increment = StepJump)
    
    #Debugging check!
    cat("nrow(HoldoutObject$ts)")
    MLPE <- fit(y~.,CasesSeriesObject[HoldoutObject$tr,],model="mlpe",search="heuristic")
    Output <- lforecast(MLPE, CasesSeriesObject, start=(length(HoldoutObject$tr) + 1), horizon=K)
    predictions[[i]] <- if(is.list(Output)) Output$pred else Output
    actuals[[i]] <- CasesSeriesObject$y[HoldoutObject$ts]
  }
  
  return(data.frame(
    Actual = unlist(actuals),
    Prediction = unlist(predictions)))
}