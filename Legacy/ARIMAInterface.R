library(forecast)

call_arima <- function(dataset){
  N <- nrow(dataset)
  TRAINING <- dataset[1:floor(N * 0.8), ]
  TEST <- dataset[(floor(N * 0.8) + 1):N, ]
  
  TRAININGTS <- ts(TRAINING$Num_Customers, frequency = 7)
  model <- auto.arima(TRAININGTS,
                      seasonal = TRUE,
                      stepwise = FALSE,
                      approximation = FALSE
  )
  
  PREDICTVALUES <- numeric(nrow(TEST))
  for (i in 1:nrow(TEST)) {
    train_i <- ts(c(TRAINING$Num_Customers, TEST$Num_Customers[seq_len(i-1)]), frequency = 7)
    fit_i <- Arima(train_i, model = model)
    PREDICTVALUES[i] <- forecast(fit_i, h = 1)$mean
  }
  
  REALVALUES <- TEST$Num_Customers
  
  return(list(predictions = PREDICTVALUES, actuals = REALVALUES))
}