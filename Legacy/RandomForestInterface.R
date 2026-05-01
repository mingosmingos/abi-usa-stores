call_rf <- function(dataset){
  series <- dataset[[2]]
  L <- length(series)
  K = 7
  Test = K
  S = 1
  Runs = 7
  
  W = (L - Test) - (Runs - 1) * S
  
  # rminer
  timelags = c(1:3, 7, 27)
  all_preds <- vector("list", Runs)
  
  for (i in 1:Runs){
    H = holdout(series, ratio = Test, mode = "rolling", iter = i, window = W, increment = S)
    
    window_series <- series[min(H$tr):max(H$ts)]
    D <- CasesSeries(window_series, timelags)
    
    n_test <- length(H$ts)
    dtr <- D[1:(nrow(D) - n_test), ]
    TS  <- D[(nrow(D) - n_test + 1):nrow(D), ]
    
    # TR = Training
    RF <- fit(y ~ ., dtr, model = "randomForest", search = "heuristic")
    
    START <- nrow(D) - n_test + 1
    
    PRF <- lforecast(RF, D, start = START, horizon = n_test)
    
    all_preds[[i]] <- PRF
  }
  
  return(all_preds)
}