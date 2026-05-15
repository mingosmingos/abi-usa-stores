library(rminer)

source("RandomForest.R")

csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
cat("Found datasets:", paste(csv_files, collapse = ", "), "\n")

for (filepath in csv_files){
  dataset_name <- tools::file_path_sans_ext(basename(filepath))
  cat("\n========== Dataset:", dataset_name, "==========\n")
  
  data <- read.csv(filepath)
  data$TouristEvent <- ifelse(data$TouristEvent == "Yes", 1, 0)
  data <- na.omit(data)
  
  Num_Customers <- data[, "Num_Customers"]
  
  timelags <- c(1:3, 7, 27)
  CasesSeriesObject <- CasesSeries(Num_Customers, timelags)
  K <- 7
  LeadTimeSteps <- K
  StepSize <- round(K/3)
  
  LengthObject <- length(Num_Customers)
  RowNumber <- nrow(CasesSeriesObject)
  WindowSize <- round(LengthObject * 0.7)
  Runs <- floor((LengthObject - WindowSize - LeadTimeSteps) / StepSize) + 1
  
  # RandomForestResult <- call_rf(LeadTimeSteps, StepSize, CasesSeriesObject, RowNumber, WindowSize, Runs)
  
  HoldoutObject <- holdout(CasesSeriesObject,
                           ratio = LeadTimeSteps,
                           mode = "incremental",
                           iter = 7,
                           window = WindowSize,
                           increment = 5)
}

HoldoutObject <- holdout(Num_Customers,
                         ratio = LeadTimeSteps,
                         mode = "incremental",
                         iter = 1,
                         window = 7,
                         increment = 5)