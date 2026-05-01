library(ranger)

call_rf <- function(dataset) {
  dataset$TouristEvent <- ifelse(dataset$TouristEvent == "Yes", 1, 0)
  dataset <- na.omit(dataset)

  # Lag features to give the model temporal context
  dataset$lag1 <- c(NA, head(dataset$Num_Customers, -1))
  dataset$lag7 <- c(rep(NA, 7), head(dataset$Num_Customers, -7))
  dataset <- na.omit(dataset)

  N        <- nrow(dataset)
  TRAINING <- dataset[1:floor(N * 0.8), ]
  TEST     <- dataset[(floor(N * 0.8) + 1):N, ]

  FEATURES <- c("Num_Employees", "Pct_On_Sale", "TouristEvent", "lag1", "lag7")

  model <- ranger(
    formula   = Num_Customers ~ .,
    data      = TRAINING[, c("Num_Customers", FEATURES)],
    num.trees = 500,
    seed      = 42
  )

  # Walk-forward: rebuild lag features from the growing window
  all_customers <- dataset$Num_Customers
  train_end     <- floor(N * 0.8)
  PREDICTVALUES <- numeric(nrow(TEST))

  for (i in 1:nrow(TEST)) {
    idx      <- train_end + i          # absolute row in dataset
    lag1_val <- all_customers[idx - 1]
    lag7_val <- all_customers[idx - 7]

    new_row <- data.frame(
      Num_Employees = TEST$Num_Employees[i],
      Pct_On_Sale   = TEST$Pct_On_Sale[i],
      TouristEvent  = TEST$TouristEvent[i],
      lag1          = lag1_val,
      lag7          = lag7_val
    )

    PREDICTVALUES[i] <- predict(model, data = new_row)$predictions
  }

  return(list(predictions = PREDICTVALUES, actuals = TEST$Num_Customers))
}
