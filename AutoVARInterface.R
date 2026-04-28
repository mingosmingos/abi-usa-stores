library(vars)
source("multi-utils.R")

call_autovar <- function(dataset) {
  # 1. Preprocess
  dataset$TouristEvent <- ifelse(dataset$TouristEvent == "Yes", 1, 0)
  dataset <- na.omit(dataset)
  N <- nrow(dataset)
  dataset <- ts(dataset, frequency = 7)
  
  # 2. Holdout split (80/20)
  TRAINING <- dataset[1:floor(N * 0.8), ]
  TEST     <- dataset[(floor(N * 0.8) + 1):N, ]
  
  TRAININGTS <- ts(TRAINING[, c("Num_Customers", "Sales")], frequency = 7)
  XREG_TRAIN <- as.matrix(TRAINING[, c("Num_Employees", "Pct_On_Sale", "TouristEvent")])
  XREG_TEST  <- as.matrix(TEST[, c("Num_Employees", "Pct_On_Sale", "TouristEvent")])
  
  PREDICTVALUES <- numeric(nrow(TEST))
  REALVALUES    <- TEST$Num_Customers
  
  # 3. Recursive 1-step-ahead backtesting loop
  for (i in 1:nrow(TEST)) {
    # Update training window with actuals observed so far (matches ARIMAXInterface.R)
    train_i <- ts(cbind(
      c(TRAINING$Num_Customers, TEST$Num_Customers[seq_len(i - 1)]),
      c(TRAINING$Sales, TEST$Sales[seq_len(i - 1)])
    ), frequency = 7)
    xreg_i  <- rbind(XREG_TRAIN, XREG_TEST[seq_len(i - 1), , drop = FALSE])
    
    # Fit VAR model on the dynamically updated window
    model <- autoVAR(train_i, lag.max = 14, type = "const", exogen = xreg_i)
    
    exogen_i <- ts(XREG_TEST[i, , drop = FALSE], frequency = 7)
    colnames(exogen_i) <- colnames(XREG_TEST)
    
    fc <- forecastVAR(model, h = 1, exogen = exogen_i)
    
    # Extract prediction for the single endogenous variable (Num_Customers)
    PREDICTVALUES[i] <- as.numeric(fc[[1]])
  }
  
  return(list(predictions = PREDICTVALUES, actuals = REALVALUES))
}