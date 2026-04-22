source("multi-utils.R")

ba <- read.table("baltimore.csv", header = TRUE, sep = ",")
ba$Date <- as.Date(ba$Date)
ba <- ba[order(ba$Date), ]
ba$TouristEvent <- ifelse(ba$TouristEvent == "Yes", 1, 0)
ba <- na.omit(ba)

ENDO <- ts(ba[, c("Num_Customers", "Sales")], start = c(2012, 1), frequency = 7)
EXO <- ts(ba[, c("Num_Employees", "Pct_On_Sale", "TouristEvent")], start = c(2012, 1), frequency = 7)

model <- autoVAR(ENDO, lag.max = 14, type = "const", exogen = EXO)
summary(model)
