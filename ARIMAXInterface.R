library(forecast)

call_arimax <- function(dataset){
  dataset$TouristEvent <- ifelse(dataset$TouristEvent == "Yes", 1, 0)
  dataset <- na.omit(dataset)
  
  N <- nrow(dataset)
  TRAINING <- dataset[1:floor(N * 0.8), ]
  TEST <- dataset[(floor(N * 0.8) + 1):N, ]
  
  TRAININGTS <- ts(TRAINING$Num_Customers, frequency = 7)
  
  XREG_TRAIN <- as.matrix(TRAINING[, c("Num_Employees", "Pct_On_Sale", "TouristEvent")])
  XREG_TEST <- as.matrix(TEST[, c("Num_Employees", "Pct_On_Sale", "TouristEvent")])
  
  model <- auto.arima(TRAININGTS,
                      seasonal      = TRUE,
                      stepwise      = FALSE,
                      approximation = FALSE,
                      xreg          = XREG_TRAIN
  )
  
  PREDICTVALUES <- numeric(nrow(TEST))
  for (i in 1:nrow(TEST)) {
    train_i <- ts(c(TRAINING$Num_Customers, TEST$Num_Customers[seq_len(i - 1)]), frequency = 7)
    xreg_i <- rbind(XREG_TRAIN, XREG_TEST[seq_len(i - 1), , drop = FALSE])
    fit_i <- Arima(train_i, model = model, xreg = xreg_i)
    PREDICTVALUES[i] <- forecast(fit_i, h = 1, xreg = XREG_TEST[i, , drop = FALSE])$mean
  }
  
  REALVALUES <- TEST$Num_Customers
  
  return(list(predictions = PREDICTVALUES, actuals = REALVALUES))
}