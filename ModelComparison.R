library(rminer)

source("ARIMAInterface.R")
source("ARIMAXInterface.R")
source("AutoVARInterface.R")

csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
cat("Found datasets:", paste(csv_files, collapse = ", "), "\n")

for (filepath in csv_files) {
  dataset_name <- tools::file_path_sans_ext(basename(filepath))
  cat("\n========== Dataset:", dataset_name, "==========\n")
  
  dataset <- read.csv(filepath)
  dataset$TouristEvent <- ifelse(dataset$TouristEvent == "Yes", 1, 0)
  dataset <- na.omit(dataset)
  dataset <- ts(dataset, frequency = 7)
  
  cat(call_arima(dataset))
}