library(xgboost)

call_xgb <- function(dataset) {
  dataset$TouristEvent <- ifelse(dataset$TouristEvent == "Yes", 1, 0)
  dataset <- na.omit(dataset)

  dataset$lag1 <- c(NA, head(dataset$Num_Customers, -1))
  dataset$lag7 <- c(rep(NA, 7), head(dataset$Num_Customers, -7))
  dataset <- na.omit(dataset)

  N        <- nrow(dataset)
  TRAINING <- dataset[1:floor(N * 0.8), ]
  TEST     <- dataset[(floor(N * 0.8) + 1):N, ]

  FEATURES <- c("Num_Employees", "Pct_On_Sale", "TouristEvent", "lag1", "lag7")

  dtrain <- xgb.DMatrix(
    data  = as.matrix(TRAINING[, FEATURES]),
    label = TRAINING$Num_Customers
  )

  model <- xgb.train(
    params  = list(
      objective = "reg:squarederror",
      eta       = 0.05,
      max_depth = 6,
      subsample = 0.8
    ),
    data    = dtrain,
    nrounds = 300,
    verbose = 0
  )

  all_customers <- dataset$Num_Customers
  train_end     <- floor(N * 0.8)
  PREDICTVALUES <- numeric(nrow(TEST))

  for (i in 1:nrow(TEST)) {
    idx  <- train_end + i
    new_mat <- matrix(
      c(TEST$Num_Employees[i],
        TEST$Pct_On_Sale[i],
        TEST$TouristEvent[i],
        all_customers[idx - 1],
        all_customers[idx - 7]),
      nrow = 1,
      dimnames = list(NULL, FEATURES)
    )
    PREDICTVALUES[i] <- predict(model, xgb.DMatrix(new_mat))
  }

  return(list(predictions = PREDICTVALUES, actuals = TEST$Num_Customers))
}
