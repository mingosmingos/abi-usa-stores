library(vars)
library(rminer)
library(forecast)

source("multi-utils.R")

data <- read.table("baltimore.csv", header = TRUE, sep = ",")
data$TouristEvent <- ifelse(data$TouristEvent == "Yes", 1, 0)

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

H <- holdout(Num_Customers, ratio = LTS, mode = "order")
# VS
HG <- holdout(Num_Customers, ratio = LTS, mode = "incremental", iter = 1, window = W, increment = S)
cdata <- cbind(Num_Customers, Sales)
mtr2 <- ts(cdata[HG$tr, ], frequency = K)
Y <- cdata[HG$ts, ]

exogen <- cbind(Num_Employees, Pct_On_Sale, TouristEvent)
exotr <- exogen[HG$tr, ]
exots <- exogen[HG$ts, ]

TRAINING <- ts(Num_Customers[H$ts], frequency = K)