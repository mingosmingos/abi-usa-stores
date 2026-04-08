library(forecast)
library(rminer)
library(RSNNS)

csv_files <- list.files(path = "dados/", pattern = "\\.csv$", full.names = TRUE)
cat("Found datasets:", paste(csv_files, collapse = ", "), "\n")

NPRED <- 10

rec_lforecast <- function(RN, tinputs, horizon) {
  Pred <- vector(length = horizon)
  for (i in 1:horizon) {
    Pred[i] <- as.numeric(predict(RN, tinputs[i, ]))
    if (i < horizon) tinputs[i + 1, 1] <- Pred[i]
  }
  return(Pred)
}

# results dataframe
results <- data.frame(
  store = character(),
  NN_MAE = numeric(), NN_NMAE = numeric(), NN_RMSE = numeric(), NN_RRSE = numeric(), NN_R2 = numeric(),
  RF_MAE = numeric(), RF_NMAE = numeric(), RF_RMSE = numeric(), RF_RRSE = numeric(), RF_R2 = numeric(),
  NN_NMAE_multi = numeric(),
  NN1_NMAE_multi = numeric(),
  stringsAsFactors = FALSE
)

# Main Loop
for (file in csv_files) {
  cat(file)

  # based on first time series example.
  df <- read.table(file, header = TRUE, sep = ",")
  store <- tools::file_path_sans_ext(basename(file))

  series <- df[[2]]
  srange <- diff(range(series))

  # 25/03/2026 we were advised to test additional lags other than the weekly one
  D <- CasesSeries(series, c(1:3, 7, 27))
  N <- nrow(D)
  NTR <- N - NPRED
  TR <- 1:NTR
  TS <- (NTR + 1):N

  NN <- fit(y ~ ., D[TR, ], model = "mlpe", search = "heuristic") # multilayer perceptron
  RF <- fit(y ~ ., D[TR, ], model = "randomForest", search = "heuristic")

  START <- nrow(D) - length(TS) + 1
  PNN <- lforecast(NN, D, start = START, horizon = length(TS))
  PRF <- lforecast(RF, D, start = START, horizon = length(TS))
  Y <- D[TS, ]$y

  cat("NN (MLP)\n")
  cat("MAE:", mmetric(Y, PNN, metric = "MAE"), "\n")
  cat("NMAE:", mmetric(Y, PNN, metric = "NMAE", val = srange), "\n")
  cat("RMSE:", mmetric(Y, PNN, metric = "RMSE"), "\n")
  cat("RRSE:", mmetric(Y, PNN, metric = "RRSE"), "\n")
  cat("R2:", mmetric(Y, PNN, metric = "R22"), "\n")

  cat("RF)\n")
  cat("MAE:", mmetric(Y, PRF, metric = "MAE"), "\n")
  cat("NMAE:", mmetric(Y, PRF, metric = "NMAE", val = srange), "\n")
  cat("RMSE:", mmetric(Y, PRF, metric = "RMSE"), "\n")
  cat("RRSE:", mmetric(Y, PRF, metric = "RRSE"), "\n")
  cat("R2:", mmetric(Y, PRF, metric = "R22"), "\n")

  # based on second time series example.
  lags <- 6 # Could I do with more lags?!

  MAX <- max(series)
  MIN <- min(series)
  RANGE <- MAX - MIN
  Snorm <- (series - MIN) / RANGE

  H <- holdout(series, ratio = NPRED, mode = "order")
  Ds <- CasesSeries(Snorm, c(1:lags)) # Ds stands for D scaled
  # What does D stand for?
  HD <- holdout(Ds$y, ratio = NPRED, mode = "order")

  inputs <- Ds[, 1:lags]
  output <- Ds[, (lags + 1)]

  EL <- elman(inputs[HD$tr, "lag1"], output[HD$tr],
    size = c(4, 2), learnFuncParams = c(0.1), maxit = 300
  )

  Y_orig <- series[H$ts]
  NTS <- length(HD$ts)
  vector1 <- vector(length = NTS)
  vector1[1] <- inputs[TS[1], "lag1"]
  tinputs <- data.frame(lag1 = vector1)
  PEL <- rec_lforecast(EL, tinputs, horizon = NTS)
  PEL <- (PEL * RANGE) + MIN

  D2 <- CasesSeries(series, c(1:lags))
  NN <- rminer::fit(y ~ ., D2[HD$tr, ], model = "mlpe", search = "heuristic")
  NN1 <- rminer::fit(y ~ ., D2[HD$tr, c("lag1", "y")], model = "mlpe", search = "heuristic")
  D1 <- D2[, c("lag1", "y")]
  START2 <- nrow(D2) - NTS + 1
  PNN <- lforecast(NN, D2, start = START2, horizon = NTS)
  PNN1 <- lforecast(NN1, D1, start = START2, horizon = NTS)

  # appending metrics
  results <- rbind(results, data.frame(
    store = store,
    NN_MAE = mmetric(Y, PNN, metric = "MAE"),
    NN_NMAE = mmetric(Y, PNN, metric = "NMAE", val = srange),
    NN_RMSE = mmetric(Y, PNN, metric = "RMSE"),
    NN_RRSE = mmetric(Y, PNN, metric = "RRSE"),
    NN_R2 = mmetric(Y, PNN, metric = "R22"),
    RF_MAE = mmetric(Y, PRF, metric = "MAE"),
    RF_NMAE = mmetric(Y, PRF, metric = "NMAE", val = srange),
    RF_RMSE = mmetric(Y, PRF, metric = "RMSE"),
    RF_RRSE = mmetric(Y, PRF, metric = "RRSE"),
    RF_R2 = mmetric(Y, PRF, metric = "R22"),
    NN_NMAE_multi = mmetric(Y_orig, PNN, metric = "NMAE", val = srange),
    NN1_NMAE_multi = mmetric(Y_orig, PNN1, metric = "NMAE", val = srange),
    stringsAsFactors = FALSE
  ))

  # Plots
  plot(1:NTS, Y_orig,
    ylim = c(min(PEL, PNN, PNN1, PRF, Y_orig), max(PEL, PNN, PNN1, PRF, Y_orig)),
    type = "b", col = "black", main = paste(store, "- Multi-step forecasts")
  )
  lines(PEL, type = "b", col = "blue", pch = 2)
  lines(PNN, type = "b", col = "red", pch = 3)
  lines(PNN1, type = "b", col = "green", pch = 3)
  lines(PRF, type = "b", col = "purple", pch = 4)
  legend("topright", c("Original", "EL", "NN", "NN1", "RF"),
    pch = c(1, 2, 3, 3, 4), col = c("black", "blue", "red", "green", "purple")
  )
}
