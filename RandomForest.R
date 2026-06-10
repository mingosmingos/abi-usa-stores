library(vars)
library(rminer)
library(forecast)

source("multi-utils.R")

# data <- read.table("baltimore.csv", header = TRUE, sep = ",")
# data$TouristEvent <- ifelse(data$TouristEvent == "Yes", 1, 0)
# data <- na.omit(data)
# 
# Num_Customers <- data[, "Num_Customers"]
# 
# timelags <- c(1:3, 7, 27)
# D <- CasesSeries(Num_Customers, timelags)
# NPRED <- 7
# 
# N <- nrow(D)
# NTR <- N - NPRED
# # Manual holdout calculation.
# TR <- 1:NTR
# TS <- (NTR+1):N
# 
# RF <- fit(y~.,D[TR,],model="randomForest",search="heuristic")
# LTS <- length(TS)
# START=nrow(D)-LTS+1
# 
# PRF <- lforecast(RF,D,start=START,horizon=LTS)
# # Y <- D[TS,]$y
# 

call_rf <- function(Num_Customers, timelags){
  CasesSeriesObject <- CasesSeries(Num_Customers, timelags)
  K <- 7
  StepJump <- round(K/3)
  Runs <- 7
  WindowSize <- (nrow(CasesSeriesObject) - K) - (Runs - 1) * StepJump - max(timelags)
  
  predictions <- vector("list", Runs)
  actuals <- vector("list", Runs)
  
  for(i in 1:Runs){
    # Debugging checks!
    cat("Length of CasesSeriesObject:", nrow(CasesSeriesObject), "\n")
    cat("WindowSize:", WindowSize, "\n")
    cat("StepSize:", StepJump, "\n")
    cat("Runs:", Runs, "\n")
    
    HoldoutObject <- holdout(CasesSeriesObject$y,
                             ratio = K,
                             mode = "incremental",
                             iter = i,
                             window = WindowSize,
                             increment = StepJump)
    
    #Debugging check!
    cat("nrow(HoldoutObject$ts)")
    RF <- fit(y~.,CasesSeriesObject[HoldoutObject$tr,],model="randomForest",search="heuristic")
    Output <- lforecast(RF, CasesSeriesObject, start=(length(HoldoutObject$tr) + 1), horizon=K)
    predictions[[i]] <- if(is.list(Output)) Output$pred else Output
    actuals[[i]] <- CasesSeriesObject$y[HoldoutObject$ts]
  }
  
  return(data.frame(
    Actual = unlist(actuals),
    Prediction = unlist(predictions)))
}