################################################################################
# compare_O3_methods.R - Performance comparison: Hill Climbing vs Simulated Annealing
# Objective O3: Minimize HR (primary) + Maximize Profit (secondary)
################################################################################
library(dplyr)

rm(list = ls())
source("hill.R")
source("eval_plan_O3.R")

# Load forecasts
raw <- read.csv("all_store_predictions.csv")
forecasts <- data.frame(
  Store    = raw$Store,
  Week_ID  = raw$Run,
  Day      = raw$Step,
  Forecast = raw$Num_Customers
)

# Configuration
week_id         <- 20
max_sales_units <- 10000
max_J_global    <- 50
max_X_global    <- 30
seed            <- 123
n_runs          <- 5  # Multiple runs for statistical comparison

# Dynamic limits function (from run_hill_climbing_O3.R)
get_dynamic_limits <- function(forecasts, week_id, store_names) {
  max_J <- numeric(28); max_X <- numeric(28); idx <- 1
  for(s in 1:4) {
    fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      C_pred <- fc_store$Forecast[d]
      max_J[idx] <- min(ceiling(C_pred * 1.5 / 6), 40)
      max_X[idx] <- min(ceiling(C_pred * 1.5 / 7), 25)
      idx <- idx + 1
    }
  }
  return(list(max_J = max_J, max_X = max_X, max_PR = rep(0.30, 28)))
}

store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")
limits <- get_dynamic_limits(forecasts, week_id, store_names)
lower <- rep(0, 84)
upper <- c(limits$max_J, limits$max_X, limits$max_PR)

# Helper: Extract metrics from a solution
extract_metrics <- function(sol, forecasts, week_id, max_sales_units, max_J, max_X) {
  # Get objective value (remember: eval_plan_O3 returns negative for maximization)
  obj_val <- -eval_plan_O3(sol, forecasts, week_id, max_sales_units, 
                           verbose = FALSE, max_J = max_J, max_X = max_X)
  
  # Calculate HR and Profit separately by re-evaluating with verbose
  dim(sol) <- c(4, 7, 3)
  total_hr <- sum(round(sol[, , 1]) + round(sol[, , 2]))
  
  # Quick profit estimate (simplified - for comparison purposes)
  profit <- 0
  for(s in 1:4) {
    fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      J <- round(sol[s, d, 1]); X <- round(sol[s, d, 2]); PR <- sol[s, d, 3]
      C_pred <- fc_store$Forecast[d]
      max_assisted <- 7*X + 6*J; A <- min(max_assisted, C_pred)
      # Simplified revenue calculation
      if(A > 0) profit <- profit + A * 15 * (1 - PR)  # ~$15 avg revenue per assisted customer
    }
  }
  profit <- profit - sum(c(700, 730, 760, 800))  # Subtract fixed costs
  
  return(list(objective = obj_val, hr = total_hr, profit = profit))
}

# ============================================================================
# 1. RUN HILL CLIMBING (with convergence tracking)
# ============================================================================
run_hill_climbing_tracked <- function(seed_val) {
  set.seed(seed_val)
  
  # Initial solution (heuristic)
  s0 <- rep(0, 84); dim(s0) <- c(4,7,3)
  for(s in 1:4) {
    fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      C_pred <- fc_store$Forecast[d]
      X_heur <- floor(C_pred / 7); rest <- C_pred - 7 * X_heur
      J_heur <- ceiling(rest / 6)
      s0[s, d, 1] <- min(J_heur, limits$max_J[(s-1)*7 + d])
      s0[s, d, 2] <- min(X_heur, limits$max_X[(s-1)*7 + d])
      s0[s, d, 3] <- 0.15
    }
  }
  s0 <- c(s0); s0 <- repair_solution_inplace(s0, forecasts, week_id, max_sales_units, 
                                             limits$max_J, limits$max_X)
  
  # Tracking environment
  tracker <- list(iter = 0, history = data.frame(iter=integer(), obj=numeric(), hr=numeric(), profit=numeric()))
  
  # Mixed change function
  change_fn <- function(par, lower, upper) {
    new_par <- par
    for(i in 1:length(par)) {
      if(runif(1) < 0.3) {
        factor <- rnorm(1, mean = 1, sd = 0.1)
        new_par[i] <- par[i] * factor
        new_par[i] <- min(upper[i], max(lower[i], new_par[i]))
      }
    }
    return(new_par)
  }
  
  # Evaluation with tracking
  eval_fn <- function(sol) {
    val <- eval_plan_O3(sol, forecasts, week_id, max_sales_units, 
                        verbose = FALSE, max_J = limits$max_J, max_X = limits$max_X)
    tracker$iter <<- tracker$iter + 1
    if(tracker$iter %% 100 == 0) {
      metrics <- extract_metrics(sol, forecasts, week_id, max_sales_units, limits$max_J, limits$max_X)
      new_row <- data.frame(iter = tracker$iter, obj = val, hr = metrics$hr, profit = metrics$profit)
      tracker$history <<- rbind(tracker$history, new_row)
    }
    return(val)
  }
  
  result <- hclimbing(par = s0, fn = eval_fn, change = change_fn,
                      lower = lower, upper = upper, type = "max",
                      control = list(maxit = 5000, REPORT = 0))
  
  return(list(solution = result$sol, value = result$value, history = tracker$history, 
              metrics = extract_metrics(result$sol, forecasts, week_id, max_sales_units, limits$max_J, limits$max_X)))
}

# ============================================================================
# 2. RUN SIMULATED ANNEALING (with convergence tracking)
# ============================================================================
run_sann_tracked <- function(seed_val) {
  set.seed(seed_val)
  
  # Initial random solution
  s0 <- runif(84, min = lower, max = upper)
  
  # Tracking
  tracker <- list(iter = 0, best_val = Inf, history = data.frame(iter=integer(), best_obj=numeric(), best_hr=numeric(), best_profit=numeric()))
  
  # Neighborhood generator
  sann_gr <- function(par) {
    hchange(par, lower = lower, upper = upper, operator = "*",
            dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
  }
  
  # Evaluation with tracking
  eval_fn <- function(sol) {
    val <- -eval_plan_O3(sol, forecasts = forecasts, week_id = week_id,
                         max_sales_units = max_sales_units, verbose = FALSE,
                         max_J = limits$max_J, max_X = limits$max_X)
    tracker$iter <<- tracker$iter + 1
    
    if(!is.na(val) && is.finite(val) && val < tracker$best_val) {
      tracker$best_val <<- val
      metrics <- extract_metrics(sol, forecasts, week_id, max_sales_units, limits$max_J, limits$max_X)
      new_row <- data.frame(iter = tracker$iter, best_obj = -tracker$best_val, 
                            best_hr = metrics$hr, best_profit = metrics$profit)
      tracker$history <<- rbind(tracker$history, new_row)
    }
    return(val)
  }
  
  result <- optim(par = s0, fn = eval_fn, gr = sann_gr, method = "SANN",
                  control = list(maxit = 5000, temp = 50, trace = FALSE, tmax = 20))
  
  return(list(solution = result$par, value = -result$value, history = tracker$history,
              metrics = extract_metrics(result$par, forecasts, week_id, max_sales_units, limits$max_J, limits$max_X)))
}

# ============================================================================
# 3. EXECUTE COMPARISON
# ============================================================================
cat("Running comparative optimization...\n")

# Run multiple seeds for robustness
hc_results <- list(); sann_results <- list()

for(i in 1:n_runs) {
  cat(sprintf("Run %d/%d...\n", i, n_runs))
  hc_results[[i]] <- run_hill_climbing_tracked(seed + i)
  sann_results[[i]] <- run_sann_tracked(seed + i)
}

# Aggregate final metrics
aggregate_metrics <- function(results_list, method_name) {
  metrics_df <- do.call(rbind, lapply(results_list, function(r) {
    data.frame(
      Method = method_name,
      Objective = r$metrics$objective,
      HR = r$metrics$hr,
      Profit = r$metrics$profit,
      Sales_Units = NA  # Could extract from eval_plan_O3 if needed
    )
  }))
  return(metrics_df)
}

final_comparison <- rbind(
  aggregate_metrics(hc_results, "Hill Climbing"),
  aggregate_metrics(sann_results, "Simulated Annealing")
)

# ============================================================================
# 4. VISUALIZATIONS
# ============================================================================
if(require(ggplot2, quietly = TRUE) && require(gridExtra, quietly = TRUE)) {
  
  cat("Generating convergence plots...\n")
  
  # Combine histories - Standardize column names
  hc_hist <- do.call(rbind, lapply(seq_along(hc_results), function(i) {
    h <- hc_results[[i]]$history
    if("obj" %in% names(h)) {
      h$best_obj <- h$obj
    }
    h$Method <- "Hill Climbing"
    h$Run <- i
    h[, c("Run", "Method", "iter", "best_obj")]
  }))
  
  sann_hist <- do.call(rbind, lapply(seq_along(sann_results), function(i) {
    s <- sann_results[[i]]$history
    s$Method <- "Simulated Annealing"
    s$Run <- i
    s[, c("Run", "Method", "iter", "best_obj")]
  }))
  
  all_history <- rbind(hc_hist, sann_hist)
  
  # Plot 1: Convergence Curves
  avg_conv <- aggregate(best_obj ~ iter + Method, data = all_history, FUN = mean, na.rm = TRUE)
  
  p1 <- ggplot(avg_conv, aes(x = iter, y = best_obj, color = Method)) +
    geom_line(size = 1.2) +
    geom_point(alpha = 0.3) +
    scale_color_manual(values = c("Hill Climbing" = "#2E86AB", "Simulated Annealing" = "#A23B72")) +
    labs(title = "Convergence: Objective Value vs Iterations",
         subtitle = "Average across 5 runs (higher = better)",
         x = "Iteration", y = "Objective Value",
         color = "Method") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  # Plot 2: Final Metrics Comparison (boxplots)
  p2 <- ggplot(final_comparison, aes(x = Method, y = Objective, fill = Method)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.15, alpha = 0.6) +
    scale_fill_manual(values = c("Hill Climbing" = "#2E86AB", "Simulated Annealing" = "#A23B72")) +
    labs(title = "Final Objective Values", x = "Method", y = "Objective Value") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "none")
  
  # Plot 3: HR vs Profit Trade-off
  p3 <- ggplot(final_comparison, aes(x = HR, y = Profit, color = Method, shape = Method)) +
    geom_point(size = 4, alpha = 0.8) +
    scale_color_manual(values = c("Hill Climbing" = "#2E86AB", "Simulated Annealing" = "#A23B72")) +
    labs(title = "Pareto View: HR Usage vs Profit Achieved",
         subtitle = "Each point = one optimization run",
         x = "Total HR Resources (J+X)", y = "Total Profit (USD)",
         color = "Method", shape = "Method") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  # Display all plots
  grid.arrange(p1, p2, p3, ncol = 1)
  
  # Save plots
  ggsave("O3_convergence_comparison.png", p1, width = 10, height = 5, dpi = 300)
  ggsave("O3_final_metrics.png", p2, width = 8, height = 5, dpi = 300)
  ggsave("O3_pareto_comparison.png", p3, width = 10, height = 6, dpi = 300)
  
  cat("Plots saved successfully!\n")
  
} else {
  cat("Install required packages: install.packages(c('ggplot2', 'gridExtra'))\n")
}

# ============================================================================
# 5. SAVE RESULTS
# ============================================================================
saveRDS(list(
  hill_climbing = hc_results,
  simulated_annealing = sann_results,
  final_comparison = final_comparison,
  configuration = list(week_id = week_id, max_sales_units = max_sales_units, n_runs = n_runs)
), file = "O3_method_comparison_results.rds")

cat("\n✓ Comparison complete! Results saved to 'O3_method_comparison_results.rds'\n")
cat("✓ Plots saved as PNG files in working directory\n")