library(vars)
library(rminer)
library(forecast)

source("multi-utils.R")

data <- read.table("baltimore.csv", header = TRUE, sep = ",")
data$TouristEvent <- ifelse(data$TouristEvent == "Yes", 1, 0)
data <- na.omit(data)

Num_Customers <- data[, "Num_Customers"]

timelags <- c(1:3, 7, 27)
D <- CasesSeries(Num_Customers, timelags)
NPRED <- 7

N <- nrow(D)
NTR <- N - NPRED
# Manual holdout calculation.
TR <- 1:NTR
TS <- (NTR+1):N

RF <- fit(y~.,D[TR,],model="randomForest",search="heuristic")
LTS <- length(TS)
START=nrow(D)-LTS+1

PRF <- lforecast(RF,D,start=START,horizon=LTS)
# Y <- D[TS,]$y

predictions <- vector("list", Runs)
actuals <- vector("list", Runs)

call_rf <- function(LeadTimeSteps, StepSize, CasesSeriesObject, RowNumber, WindowSize, Runs){
  
  for(i in 1:Runs){
    HoldoutObject <- holdout(CasesSeriesObject,
                             ratio = LeadTimeSteps,
                             mode = "incremental",
                             iter = i,
                             window = WindowSize,
                             increment = StepSize)
    Y <- HoldoutObject$ts
    RF <- fit(y~.,CasesSeriesObject[HoldoutObject$tr,],model="randomForest",search="heuristic")
    
    START <- min(HoldoutObject$ts)
    
    predictions[[i]] <- lforecast(RF,D,start=START,horizon=LeadTimeSteps)
  }
  
  return(predictions)
}