library(rminer)

source("ARIMAInterface.R")
source("ARIMAXInterface.R")
source("RFInterface.R")
source("SVRInterface.R")
source("XGBInterface.R")

csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)
cat("Found datasets:", paste(csv_files, collapse = ", "), "\n")

all_results <- list()

for (filepath in csv_files) {
  dataset_name <- tools::file_path_sans_ext(basename(filepath))
  cat("\n========== Dataset:", dataset_name, "==========\n")
  
  dataset <- read.csv(filepath)
  
  arima_result  <- call_arima(dataset)
  arimax_result <- call_arimax(dataset)
  rf_result     <- call_rf(dataset)
  svr_result    <- call_svr(dataset)
  xgb_result    <- call_xgb(dataset)
  
  # Use ARIMA actuals as the reference (same 80/20 split, no lag trimming)
  # ML models drop leading NA rows from lag features, so their test windows
  # are slightly shorter. Align on the common tail.
  Y_arima <- arima_result$actuals
  Y_ml    <- rf_result$actuals   # RF / SVR / XGB share the same trimmed split
  
  srange_arima <- max(Y_arima) - min(Y_arima)
  srange_ml    <- max(Y_ml)    - min(Y_ml)
  
  arima_models <- list(
    ARIMA  = list(preds = arima_result$predictions,  Y = Y_arima, srange = srange_arima),
    ARIMAX = list(preds = arimax_result$predictions, Y = Y_arima, srange = srange_arima)
  )
  
  ml_models <- list(
    RF  = list(preds = rf_result$predictions,  Y = Y_ml, srange = srange_ml),
    SVR = list(preds = svr_result$predictions, Y = Y_ml, srange = srange_ml),
    XGB = list(preds = xgb_result$predictions, Y = Y_ml, srange = srange_ml)
  )
  
  print_metrics <- function(model_name, preds, Y, srange) {
    cat("\n---", model_name, "---\n")
    cat("MAE:",  mmetric(Y, preds, metric = "MAE"),               "\n")
    cat("NMAE:", mmetric(Y, preds, metric = "NMAE", val = srange), "\n")
    cat("RMSE:", mmetric(Y, preds, metric = "RMSE"),              "\n")
    cat("RRSE:", mmetric(Y, preds, metric = "RRSE"),              "\n")
    cat("R2:",   mmetric(Y, preds, metric = "R2"),                "\n")
  }
  
  for (m in names(arima_models)) {
    print_metrics(m, arima_models[[m]]$preds,
                  arima_models[[m]]$Y,
                  arima_models[[m]]$srange)
  }
  for (m in names(ml_models)) {
    print_metrics(m, ml_models[[m]]$preds,
                  ml_models[[m]]$Y,
                  ml_models[[m]]$srange)
  }
  
  all_results[[dataset_name]] <- list(
    actuals_arima = Y_arima,
    actuals_ml    = Y_ml,
    ARIMA         = arima_result$predictions,
    ARIMAX        = arimax_result$predictions,
    RF            = rf_result$predictions,
    SVR           = svr_result$predictions,
    XGB           = xgb_result$predictions
  )
}

# Function to plot predicted vs actual for a single dataset
# Replace the plot_predictions function with this version:
plot_predictions <- function(results, dataset_name) {
  library(ggplot2)
  library(reshape2)
  
  # Get ML actuals as reference (shorter but common to all ML models)
  actuals_ml <- results$actuals_ml
  n_ml <- length(actuals_ml)
  time_idx_ml <- 1:n_ml
  
  # For ARIMA/ARIMAX, use the tail to match ML length
  # (ARIMA predictions are longer, so we take the last n_ml values)
  arima_preds <- tail(results$ARIMA, n_ml)
  arimax_preds <- tail(results$ARIMAX, n_ml)
  
  # Create long-format dataframe for plotting
  plot_df <- data.frame(
    Time = rep(time_idx_ml, 5),
    Actual = rep(actuals_ml, 5),
    Model = rep(c("ARIMA", "ARIMAX", "RF", "SVR", "XGB"), each = n_ml),
    Predicted = c(arima_preds, arimax_preds, results$RF, results$SVR, results$XGB)
  )
  
  # Main time series plot
  p1 <- ggplot(plot_df, aes(x = Time)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Predicted, color = Model), alpha = 0.8) +
    scale_color_manual(values = c(
      "Actual" = "black", "ARIMA" = "#E69F00", "ARIMAX" = "#56B4E9",
      "RF" = "#009E73", "SVR" = "#F0E442", "XGB" = "#D55E00"
    )) +
    labs(title = paste("Predictions vs Actuals -", dataset_name),
         x = "Time Step", y = "Num_Customers", color = "Model") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Scatter plot: Predicted vs Actual
  p2 <- ggplot(plot_df, aes(x = Actual, y = Predicted, color = Model)) +
    geom_point(alpha = 0.6, size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = c(
      "ARIMA" = "#E69F00", "ARIMAX" = "#56B4E9",
      "RF" = "#009E73", "SVR" = "#F0E442", "XGB" = "#D55E00"
    )) +
    labs(title = paste("Predicted vs Actual -", dataset_name),
         x = "Actual Values", y = "Predicted Values") +
    coord_equal() +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(timeseries = p1, scatter = p2))
}

# Usage example (inside your dataset loop, after storing results):
# plots <- plot_predictions(all_results[[dataset_name]], dataset_name)
# print(plots$timeseries)
# print(plots$scatter)
# ggsave(paste0("plots/", dataset_name, "_predictions.png"), plots$timeseries, width=10, height=5)

# Function to compute and plot Pareto frontier
plot_pareto <- function(all_results, metric_x = "RMSE", metric_y = "R2", lower_is_better_x = TRUE) {
  library(ggplot2)
  library(dplyr)
  
  models <- c("ARIMA", "ARIMAX", "RF", "SVR", "XGB")
  metrics_df <- data.frame()
  
  # Extract metrics for each dataset/model combination
  for (ds_name in names(all_results)) {
    res <- all_results[[ds_name]]
    
    # Use ML actuals as the common reference
    Y_ml <- res$actuals_ml
    n_ml <- length(Y_ml)
    srange_ml <- max(Y_ml) - min(Y_ml)
    
    # Align ARIMA/ARIMAX predictions to ML length
    arima_preds <- tail(res$ARIMA, n_ml)
    arimax_preds <- tail(res$ARIMAX, n_ml)
    
    # Calculate metrics for each model using aligned predictions
    for (m in models) {
      if (m == "ARIMA") {
        preds <- arima_preds
      } else if (m == "ARIMAX") {
        preds <- arimax_preds
      } else {
        preds <- res[[m]]
      }
      
      metrics_df <- rbind(metrics_df, data.frame(
        Dataset = ds_name,
        Model = m,
        MAE = mmetric(Y_ml, preds, metric = "MAE"),
        RMSE = mmetric(Y_ml, preds, metric = "RMSE"),
        R2 = mmetric(Y_ml, preds, metric = "R2"),
        NMAE = mmetric(Y_ml, preds, metric = "NMAE", val = srange_ml),
        RRSE = mmetric(Y_ml, preds, metric = "RRSE")
      ))
    }
  }
  
  # Aggregate across datasets (mean metrics)
  agg_metrics <- metrics_df %>%
    group_by(Model) %>%
    summarise(across(c(MAE, RMSE, R2, NMAE, RRSE), mean, na.rm = TRUE))
  
  # Compute Pareto frontier: non-dominated solutions
  is_pareto <- function(df, x_col, y_col, lower_x = TRUE) {
    n <- nrow(df)
    pareto_idx <- logical(n)
    for (i in 1:n) {
      dominated <- FALSE
      for (j in 1:n) {
        if (i == j) next
        better_x <- if (lower_x) df[j, x_col] < df[i, x_col] else df[j, x_col] > df[i, x_col]
        better_y <- df[j, y_col] > df[i, y_col]  # R2: higher is better
        if (better_x && better_y) {
          dominated <- TRUE
          break
        }
      }
      pareto_idx[i] <- !dominated
    }
    return(pareto_idx)
  }
  
  agg_metrics$Pareto <- is_pareto(agg_metrics, metric_x, metric_y, lower_is_better_x)
  
  # Sort Pareto points for proper line drawing
  pareto_df <- agg_metrics[agg_metrics$Pareto, ]
  if (nrow(pareto_df) > 1) {
    pareto_df <- pareto_df[order(pareto_df[[metric_x]]), ]
  }
  
  # Build the plot
  p <- ggplot(agg_metrics, aes_string(x = metric_x, y = metric_y, color = "Model")) +
    geom_point(aes(size = Pareto), alpha = 0.8) +
    geom_text(aes(label = Model), vjust = -1, size = 3) +
    # Add line ONLY if multiple Pareto points exist
    {if(nrow(pareto_df) > 1) {
      geom_line(data = pareto_df, 
                aes_string(x = metric_x, y = metric_y), 
                color = "red", linetype = "dashed", linewidth = 1, inherit.aes = FALSE)
    }} +
    # Add convex hull for visual context
    {if(nrow(agg_metrics) > 2) {
      geom_polygon(data = agg_metrics[chull(agg_metrics[[metric_x]], agg_metrics[[metric_y]]), ],
                   aes_string(x = metric_x, y = metric_y), 
                   fill = "gray80", alpha = 0.3, color = NA, inherit.aes = FALSE)
    }} +
    labs(title = paste("Pareto Frontier:", metric_y, "vs", metric_x),
         subtitle = if(nrow(pareto_df) == 1) "Only one Pareto-optimal model" 
         else "Red dashed line connects Pareto-optimal models",
         x = paste0(metric_x, " (", if(lower_is_better_x) "lower" else "higher", " is better)"),
         y = paste0(metric_y, " (higher is better)"),
         size = "Pareto\nOptimal") +
    scale_size_manual(values = c("TRUE" = 5, "FALSE" = 3)) +
    theme_minimal() +
    theme(legend.position = "right")
  
  return(list(plot = p, metrics_table = agg_metrics))
}

# Usage example (after your main loop completes):
# pareto_result <- plot_pareto(all_results, metric_x = "MAE", metric_y = "R2")
# print(pareto_result$plot)
# ggsave("plots/pareto_MAE_R2.png", pareto_result$plot, width = 8, height = 6)
# print(pareto_result$metrics_table)  # View aggregated metrics

for (ds_name in names(all_results)) {
  plots <- plot_predictions(all_results[[ds_name]], ds_name)
  ggsave(paste0("plots/", ds_name, "_timeseries.png"), plots$timeseries, width = 10, height = 5, dpi = 300)
  ggsave(paste0("plots/", ds_name, "_scatter.png"), plots$scatter, width = 6, height = 6, dpi = 300)
}

# Then generate the cross-dataset Pareto plot
pareto_result <- plot_pareto(all_results, metric_x = "RMSE", metric_y = "R2")
print(pareto_result$plot)
ggsave("plots/pareto_frontier.png", pareto_result$plot, width = 9, height = 7, dpi = 300)