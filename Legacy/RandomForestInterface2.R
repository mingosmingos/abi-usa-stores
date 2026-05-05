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

Num_Customers <- data[, "Num_Customers"]
Sales <- data[, "Sales"]
Num_Employees <- data[, "Num_Employees"]
Pct_On_Sale <- data[, "Pct_On_Sale"]
TouristEvent <- data[, "TouristEvent"] 

L <- length(Num_Customers)
W <- round(L * 0.7)
Runs <- floor((L - W - LTS) / S) + 1

cdata <- cbind(Num_Customers, Sales)
exogen <- cbind(Num_Employees, Pct_On_Sale, TouristEvent)

predictions <- vector("list", Runs)
actuals <- vector("list", Runs)

D <- CasesSeries(Num_Customers, c(1:3, 7, 27))
N <- nrow(D)
all_PRF <- vector("list", N)

for(i in 1:N){
  H <- holdout(Num_Customers, ratio = LTS, mode = "rolling", iter = i, window = 8, increment = 1)
  # tr <- ts(H$tr, frequency = 7)
  # ts <- ts(H$ts, frequency = 7)
  tr <- D[H$tr, ]
  ts <- D[H$ts, ]
  RF <- fit(y ~ ., tr, model = "randomForest", search = "heuristic")
  PRF  <- lforecast(RF, D, start = nrow(tr), horizon = nrow(ts))
  
  all_PRF[[i]] <- PRF
}