library(vars)
library(rminer)
library(forecast)

source("multi-utils.R")

call_arimax <- function(Num_Customers, timelags, Sales, Num_Employees, Pct_On_Sale, TouristEvent){
  CasesSeriesObject <- CasesSeries(Num_Customers, timelags)
  K <- 7
  StepJump <- round(K/3)
  Runs <- 7
  WindowSize <- (nrow(CasesSeriesObject) - K) - (Runs - 1) * StepJump - max(timelags)
  
  predictions <- vector("list", Runs)
  actuals <- vector("list", Runs)
  
  Endogenous <- ts(cbind(Num_Customers, Sales), K)
  Exogenous <- cbind(Num_Employees, Pct_On_Sale, TouristEvent)
  
  for(i in 1:Runs){
    HoldoutObject <- holdout(Endogenous[, 1],
                             ratio = K,
                             mode = "incremental",
                             iter = i,
                             window = WindowSize,
                             increment = StepJump)
    
    #Debugging check!
    cat("nrow(HoldoutObject$ts)")
    # ARIMAX <- autoARIMAX(Endogenous[HoldoutObject$tr,], frequency = K, exogen = Exogenous[HoldoutObject$tr,])
    # Output <- forecastARIMAX(ARIMAX, h = K, exogen = Exogenous[HoldoutObject$ts,])
    # predictions[[i]] <- if(is.list(Output)) Output$pred else Output
    # actuals[[i]] <- Endogenous[HoldoutObject$ts, 1]
    
    tr <- HoldoutObject$tr
    ts <- HoldoutObject$ts
    
    mtr <- ts(Endogenous[tr, ], frequency = K)
    exotr <- Exogenous[tr, ]
    exots <- Exogenous[ts, ]
    
    ARIMAX_model <- autoARIMAX(mtr, frequency = K, exogen = exotr)
    FA           <- forecastARIMAX(ARIMAX_model, h = K, exogen = exots)
    
    predictions[[i]] <- FA[[1]]
    actuals[[i]] <- Endogenous[ts, 1]
  }
  
  return(data.frame(
    Actual = unlist(actuals),
    Prediction = unlist(predictions)))
}