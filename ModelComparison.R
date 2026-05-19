library(rminer)

source("RandomForest.R")
source("MLPE.R")
source("ARIMAX.R")

csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
cat("Found datasets:", paste(csv_files, collapse = ", "), "\n")

results_list <- list()

for (filepath in csv_files){
  dataset_name <- tools::file_path_sans_ext(basename(filepath))
  cat("\n========== Dataset:", dataset_name, "==========\n")
  
  data <- read.csv(filepath)
  data$TouristEvent <- ifelse(data$TouristEvent == "Yes", 1, 0)
  data <- na.omit(data)
  
  Num_Customers <- data[, "Num_Customers"]
  
  Sales <- data[, "Sales"]
  
  Num_Employees <- data[, "Num_Employees"]
  Pct_On_Sale <- data[, "Pct_On_Sale"]
  TouristEvent <- data[, "TouristEvent"]
  
  timelags <- c(1:3, 7, 27)
  
  # CasesSeriesObject <- CasesSeries(Num_Customers, timelags)
  # K <- 7
  # LeadTimeSteps <- K
  # StepSize <- round(K/3)
  # 
  # LengthObject <- length(CasesSeriesObject)
  # RowNumber <- nrow(CasesSeriesObject)
  # WindowSize <- round(LengthObject * 0.1)
  # Runs <- floor((LengthObject - WindowSize - LeadTimeSteps) / StepSize) + 1
  
  # RandomForestResult <- call_rf(Num_Customers, timelags)
  # 
  # DataFrame <- as.data.frame(do.call(rbind, RandomForestResult))
  # colnames(DataFrame) <- paste0("Step_", 1:ncol(DataFrame))
  # rownames(DataFrame) <- paste0("Run_", 1:nrow(DataFrame))
  # 
  # all_results[[dataset_name]] <- DataFrame
  
  RandomForest <- call_rf(Num_Customers, timelags)
  MultiLayerPerceptron <- call_mlpe(Num_Customers, timelags)
  ARIMAX <- call_arimax(Num_Customers, timelags, Sales, Num_Employees, Pct_On_Sale, TouristEvent)
  
  colnames(RandomForest) <- c(paste0(dataset_name, "_Actual"), paste0(dataset_name, "_RF_Prediction"))
  colnames(MultiLayerPerceptron) <- c(paste0(dataset_name, "_Actual"), paste0(dataset_name, "_MLPE_Prediction"))
  colnames(ARIMAX) <- c(paste0(dataset_name, "_Actual"), paste0(dataset_name, "_ARIMAX_Prediction"))
  
  combined <- cbind(
    RandomForest, 
    MultiLayerPerceptron = MultiLayerPerceptron[, 2],
    ARIMAX_Prediction = ARIMAX[, 2])
  
  results_list[[dataset_name]] <- combined
}

final_predictions <- do.call(cbind, results_list)

# HoldoutObject <- holdout(Num_Customers,
#                          ratio = LeadTimeSteps,
#                          mode = "incremental",
#                          iter = 2,
#                          window = 7,
#                          increment = 5)
# 

# Endogenous <- ts(cbind(Num_Customers, Sales), 7)
# 
# HoldoutObject <- holdout(Endogenous,
#                          ratio = 9,
#                          mode = "incremental",
#                          iter = 7,
#                          window = 3,
#                          increment = 5)