library(forecast)
library(rminer)
library(RSNNS)

data <- read.csv("baltimore.csv")
Num_Employees <- data[, "Num_Employees"]
Num_Customers <- data[, "Num_Customers"]
Pct_On_Sale <- data[, "Pct_On_Sale"]
data$TouristEvent <- ifelse(data$TouristEvent == "Yes", 1, 0)
TouristEvent <- data[, "TouristEvent"]
Sales <- data[, "Sales"]

for(i in 1:7){
  H <- holdout(Num_Employees, 0.2, mode = "rolling", iter = i, window = 7, increment = 1)
  print(H$ts)
  print(H$tr)
}


dtr=ts(H$tr,frequency=2)
cat(dtr)

D <- CasesSeries(Num_Customers, c(1:3, 7, 27))

N <- nrow(D)