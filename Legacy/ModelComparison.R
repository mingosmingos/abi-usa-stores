library(forecast)
library(rminer)
library(RSNNS)

source("RandomForestInterface.R")

csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
cat("Found datasets:", paste(csv_files, collapse = ", "), "\n")

for (filepath in csv_files) {
  dataset_name <- tools::file_path_sans_ext(basename(filepath))
  cat("\n========== Dataset:", dataset_name, "==========\n")
  
  dataset <- read.csv(filepath)
  dataset$TouristEvent <- ifelse(dataset$TouristEvent == "Yes", 1, 0)
  dataset <- na.omit(dataset)
  
  Num_Employees <- dataset[, "Num_Employees"]
  Num_Customers <- dataset[, "Num_Customers"]
  Pct_On_Sale <- dataset[, "Pct_On_Sale"]
  dataset$TouristEvent <- ifelse(dataset$TouristEvent == "Yes", 1, 0)
  TouristEvent <- dataset[, "TouristEvent"]
  Sales <- dataset[, "Sales"]
  
  results <- call_rf(Num_Customers)
  print(results)
}