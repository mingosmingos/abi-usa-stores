library(e1071)

call_svr <- function(dataset) {
  dataset$TouristEvent <- ifelse(dataset$TouristEvent == "Yes", 1, 0)
  dataset <- na.omit(dataset)

  dataset$lag1 <- c(NA, head(dataset$Num_Customers, -1))
  dataset$lag7 <- c(rep(NA, 7), head(dataset$Num_Customers, -7))
  dataset <- na.omit(dataset)

  N        <- nrow(dataset)
  TRAINING <- dataset[1:floor(N * 0.8), ]
  TEST     <- dataset[(floor(N * 0.8) + 1):N, ]

  FEATURES <- c("Num_Employees", "Pct_On_Sale", "TouristEvent", "lag1", "lag7")

  model <- svm(
    x      = TRAINING[, FEATURES],
    y      = TRAINING$Num_Customers,
    type   = "eps-regression",
    kernel = "radial",
    scale  = TRUE
  )

  all_customers <- dataset$Num_Customers
  train_end     <- floor(N * 0.8)
  PREDICTVALUES <- numeric(nrow(TEST))

  for (i in 1:nrow(TEST)) {
    idx  <- train_end + i
    new_row <- data.frame(
      Num_Employees = TEST$Num_Employees[i],
      Pct_On_Sale   = TEST$Pct_On_Sale[i],
      TouristEvent  = TEST$TouristEvent[i],
      lag1          = all_customers[idx - 1],
      lag7          = all_customers[idx - 7]
    )
    PREDICTVALUES[i] <- predict(model, newdata = new_row)
  }

  return(list(predictions = PREDICTVALUES, actuals = TEST$Num_Customers))
}
