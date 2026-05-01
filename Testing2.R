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

for(i in 1:Runs)
{
  HG <- holdout(Num_Customers, ratio = LTS, mode = "incremental", iter = i, window = W, increment = S)
  mtr2 <- ts(cdata[HG$tr, ], frequency = K)
  Y <- cdata[HG$ts, ]
  exotr <- exogen[HG$tr, ]
  exots <- exogen[HG$ts, ]
  
  model <- autoVAR(mtr2, season = K, exogen = exotr)
  predictions[[i]] <- forecastVAR(model, h = LTS, exogen = exots)
  actuals[[i]] <- Y
}

predictions_df <- data.frame(
  Num_Customers = unlist(lapply(predictions, `[[`, 1)),
  Sales         = unlist(lapply(predictions, `[[`, 2))
)

actual_df <- data.frame(
  Run           = rep(1:Runs, each = LTS),
  Step          = rep(1:LTS, times = Runs),
  Num_Customers = unlist(lapply(actuals, function(y) y[, "Num_Customers"])),
  Sales         = unlist(lapply(actuals, function(y) y[, "Sales"]))
)

predictions_df <- data.frame(
  Run           = rep(1:Runs, each = LTS),
  Step          = rep(1:LTS, times = Runs),
  Num_Customers = unlist(lapply(predictions, `[[`, 1)),
  Sales         = unlist(lapply(predictions, `[[`, 2))
)

# Plot using base R
par(mfrow = c(1, 2))

plot(actual_df$Num_Customers, type = "l", col = "black",
     main = "Num_Customers: Actual vs Predicted",
     ylab = "Num_Customers", xlab = "Time Step")
lines(predictions_df$Num_Customers, col = "blue", lty = 2)
legend("topleft", legend = c("Actual", "Predicted"),
       col = c("black", "blue"), lty = c(1, 2))

plot(actual_df$Sales, type = "l", col = "black",
     main = "Sales: Actual vs Predicted",
     ylab = "Sales", xlab = "Time Step")
lines(predictions_df$Sales, col = "red", lty = 2)
legend("topleft", legend = c("Actual", "Predicted"),
       col = c("black", "red"), lty = c(1, 2))