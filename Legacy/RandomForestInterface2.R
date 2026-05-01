call_rf <- function(Num_Customers){
  D <- CasesSeries(Num_Customers, c(1:3, 7, 27))
  N <- nrow(D)
  all_PRF <- numeric(N)
  
  for(i in 1:N){
    H <- holdout(Num_Customers, ratio = 0.2, mode = "rolling", iter = i, window = 8, increment = 1)
    # tr <- ts(H$tr, frequency = 7)
    # ts <- ts(H$ts, frequency = 7)
    tr <- D[H$tr, ]
    ts <- D[H$ts, ]
    RF <- fit(y ~ ., tr, model = "randomForest", search = "heuristic")
    PRF  <- lforecast(RF, D, start = max(tr), horizon = length(ts))
    
    all_PRF[[i]] <- PRF
  }
  return(all_PRF)
}