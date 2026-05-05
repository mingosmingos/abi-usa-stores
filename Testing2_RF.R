library(rminer)
source("multi-utils.R")

# 1. Get all CSV files in the working directory (adjust path if needed)
csv_files <- list.files(path = ".", pattern = "\\.csv$", full.names = TRUE)

# Initialize storage for all stores
all_actuals      <- list()
all_predictions  <- list()

for (f in csv_files) {
  # Extract store name from filename (e.g., "baltimore" from "baltimore.csv")
  store_name <- tools::file_path_sans_ext(basename(f))
  cat(sprintf("\n🔄 Processing: %s\n", store_name))
  
  # Load & clean data
  data <- read.table(f, header = TRUE, sep = ",")
  data <- na.omit(data)
  
  # Safety check: ensure required column exists
  if (!"Num_Customers" %in% colnames(data)) {
    warning(sprintf("Skipping %s: 'Num_Customers' column not found.", f))
    next
  }
  
  Num_Customers <- data[, "Num_Customers"]
  L <- length(Num_Customers)
  W <- round(L * 0.7)
  K <- 7
  LTS <- K
  S <- round(K/3)
  Runs <- floor((L - W - LTS) / S) + 1
  
  # Skip if not enough data for at least one run
  if (Runs <= 0) {
    warning(sprintf("Skipping %s: insufficient data length for forecasting.", f))
    next
  }
  
  LAGS <- 1:K
  predictions <- vector("list", Runs)
  actuals     <- vector("list", Runs)
  
  # Original forecasting loop
  for (i in 1:Runs) {
    HG <- holdout(Num_Customers, ratio = LTS, mode = "incremental",
                  iter = i, window = W, increment = S)
    D <- CasesSeries(Num_Customers, LAGS)
    offset <- max(LAGS)
    tr_idx <- HG$tr[HG$tr > offset] - offset
    ts_idx <- HG$ts[HG$ts > offset] - offset
    
    RF <- fit(y ~ ., D[tr_idx, ], model = "randomForest", search = "heuristic")
    START <- min(ts_idx)
    pred <- lforecast(RF, D, start = START, horizon = LTS)
    
    predictions[[i]] <- pred
    actuals[[i]]     <- D[ts_idx, ]$y
  }
  
  # Flatten & add Store column
  pred_vec   <- unlist(predictions)
  actual_vec <- unlist(actuals)
  
  actual_df <- data.frame(
    Run           = rep(1:Runs, each = LTS),
    Step          = rep(1:LTS,  times = Runs),
    Num_Customers = actual_vec,
    Store         = store_name
  )
  
  predictions_df <- data.frame(
    Run           = rep(1:Runs, each = LTS),
    Step          = rep(1:LTS,  times = Runs),
    Num_Customers = pred_vec,
    Store         = store_name
  )
  
  all_actuals[[store_name]]     <- actual_df
  all_predictions[[store_name]] <- predictions_df
}

# 2. Combine results across all stores
final_actuals     <- do.call(rbind, all_actuals)
final_predictions <- do.call(rbind, all_predictions)

# 3. Global metrics (across all stores combined)
cat("\n📊 Global Metrics:\n")
print(round(mmetric(y = final_actuals$Num_Customers,
                    x = final_predictions$Num_Customers,
                    metric = "ALL")))

# 4. Per-store metrics (optional but recommended)
cat("\n📊 Per-Store Metrics:\n")
store_metrics <- by(final_actuals, final_actuals$Store, function(sub) {
  mmetric(y = sub$Num_Customers, 
          x = final_predictions[final_predictions$Store == unique(sub$Store), ]$Num_Customers,
          metric = "ALL")
})
print(store_metrics)

write.csv(final_predictions, file = "all_store_predictions.csv", row.names = FALSE)