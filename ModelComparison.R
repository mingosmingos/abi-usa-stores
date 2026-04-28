library(rminer)

source("ARIMAInterface.R")
source("ARIMAXInterface.R")
source("AutoVARInterface.R")

csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
cat("Found datasets:", paste(csv_files, collapse = ", "), "\n")

all_results <- list()

for (filepath in csv_files) {
  dataset_name <- tools::file_path_sans_ext(basename(filepath))
  cat("\n========== Dataset:", dataset_name, "==========\n")
  
  dataset <- read.csv(filepath)
  
  arima_result  <- call_arima(dataset)
  arimax_result <- call_arimax(dataset)
  var_result <- call_autovar(dataset)
  
  Y      <- arima_result$actuals
  srange <- max(Y) - min(Y)
  
  models <- list(ARIMA = arima_result$predictions,
                 ARIMAX = arimax_result$predictions,
                 AutoVAR = var_result$predictions)
  
  for (model_name in names(models)) {
    PNN <- models[[model_name]]
    cat("\n---", model_name, "---\n")
    cat("MAE:",  mmetric(Y, PNN, metric = "MAE"),              "\n")
    cat("NMAE:", mmetric(Y, PNN, metric = "NMAE", val = srange), "\n")
    cat("RMSE:", mmetric(Y, PNN, metric = "RMSE"),             "\n")
    cat("RRSE:", mmetric(Y, PNN, metric = "RRSE"),             "\n")
    cat("R2:",   mmetric(Y, PNN, metric = "R2"),               "\n")
  }
  
  all_results[[dataset_name]] <- list(
    actuals = Y,
    ARIMA   = arima_result$predictions,
    ARIMAX  = arimax_result$predictions,
    AutoVAR = var_result$predictions
  )
}