library(vars)
library(rminer)
library(forecast)

source("multi-utils.R")

data <- read.table("baltimore.csv", header = TRUE, sep = ",")
data$TouristEvent <- ifelse(data$TouristEvent == "Yes", 1, 0)
data <- na.omit(data)

K <- 7
LTS <- K #Lead Time Steps
S <- round(K/3)
Runs <- 8

Num_Customers <- data[, "Num_Customers"]
Sales <- data[, "Sales"]
Num_Employees <- data[, "Num_Employees"]
Pct_On_Sale <- data[, "Pct_On_Sale"]
TouristEvent <- data[, "TouristEvent"] 

L <- length(Num_Customers)
W <- (L - LTS) - (Runs - 1)*S

cdata <- cbind(Num_Customers, Sales)
exogen <- cbind(Num_Employees, Pct_On_Sale, TouristEvent)

predictions <- vector("list", Runs)

for(i in 1:Runs)
{
  HG <- holdout(Num_Customers, ratio = LTS, mode = "incremental", iter = i, window = W, increment = S)
  mtr2 <- ts(cdata[HG$tr, ], frequency = K)
  Y <- cdata[HG$ts, ]
  exotr <- exogen[HG$tr, ]
  exots <- exogen[HG$ts, ]
  
  model <- autoVAR(mtr2, season = K, exogen = exotr)
  predictions[[i]] <- forecastVAR(model, h = LTS, exogen = exots)
  
  PRED1 <- predictions[[i]][[1]]
  PRED2 <- predictions[[i]][[2]]
}