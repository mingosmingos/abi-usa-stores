library(vars)

ba <- read.table("baltimore.csv", header = TRUE, sep = ",")

ba$Date <- as.Date(ba$Date)
ba <- ba[order(ba$Date), ]
ba$TouristEvent <- ifelse(ba$TouristEvent == "Yes", 1, 0)
ba <- na.omit(ba)

ENDO <- ts(
  ba[, c("Num_Customers", "Sales")],
  start     = c(2012, 1),
  frequency = 7
)

EXO <- ts(
  ba[, c("Num_Employees", "Pct_On_Sale", "TouristEvent")],
  start     = c(2012, 1),
  frequency = 7
)

# VARselect Plot

var_select <- VARselect(ENDO, lag.max = 14, type = "const", exogen = EXO)
criteria <- t(var_select$criteria)

matplot(1:nrow(criteria), criteria,
  type  = "l",
  lty   = 1,
  lwd   = 2,
  col   = c("blue", "red", "green", "purple"),
  xlab  = "Number of Lags",
  ylab  = "Criterion Value",
  main  = "VAR Lag Selection Criteria"
)

legend("topright",
  legend = colnames(criteria),
  col    = c("blue", "red", "green", "purple"),
  lty    = 1,
  lwd    = 2
)

abline(v = which.min(criteria[, "AIC(n)"]), col = "blue", lty = 2)
abline(v = which.min(criteria[, "HQ(n)"]), col = "red", lty = 2)
abline(v = which.min(criteria[, "SC(n)"]), col = "green", lty = 2)
abline(v = which.min(criteria[, "FPE(n)"]), col = "purple", lty = 2)

# Fitting model

model <- VAR(ENDO, p = 7, type = "const", exogen = EXO)
summary(model)
