################################################################################
# compare_O2_methods.R - Performance comparison: Hill Climbing vs Simulated Annealing
# Objective O2: Maximize Profit
################################################################################
rm(list = ls())
source("hill.R")
library(dplyr)
library(ggplot2)
library(gridExtra)

# Load evaluation function
source("eval_plan_O2.R")

# Load forecasts
raw <- read.csv("all_store_predictions.csv")
forecasts <- data.frame(
  Store    = raw$Store,
  Week_ID  = raw$Run,
  Day      = raw$Step,
  Forecast = raw$Num_Customers
)

# ============================================================================
# CONFIGURATION
# ============================================================================
week_id         <- 20
max_sales_units <- 10000
max_J           <- 50  # Global limit for O2
max_X           <- 30  # Global limit for O2
seed            <- 123
n_runs          <- 5

store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")

# Simple bounds (O2 doesn't use dynamic limits)
lower <- rep(0, 84)
upper <- c(rep(max_J, 28), rep(max_X, 28), rep(0.30, 28))

# ============================================================================
# HELPER: Extract metrics from a solution
# ============================================================================
extract_metrics <- function(sol, forecasts, week_id, max_sales_units) {
  # Get objective value (O2 maximizes profit)
  obj_val <- eval_plan_O2(sol, forecasts, week_id, max_sales_units, verbose = FALSE)
  
  dim(sol) <- c(4, 7, 3)
  total_hr <- sum(round(sol[, , 1]) + round(sol[, , 2]))
  
  # Calculate profit (simplified estimate)
  profit <- 0
  for(s in 1:4) {
    fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      J <- round(sol[s, d, 1])
      X <- round(sol[s, d, 2])
      PR <- sol[s, d, 3]
      C_pred <- fc_store$Forecast[d]
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      if(A > 0) profit <- profit + A * 15 * (1 - PR)
    }
  }
  profit <- profit - sum(c(700, 730, 760, 800))  # Fixed costs
  
  return(list(objective = obj_val, hr = total_hr, profit = profit))
}

# ============================================================================
# 1. RUN HILL CLIMBING (with convergence tracking)
# ============================================================================
run_hill_climbing_tracked <- function(seed_val) {
  set.seed(seed_val)
  
  # Initial solution (heuristic)
  s0 <- rep(0, 84)
  dim(s0) <- c(4, 7, 3)
  for(s in 1:4) {
    fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      C_pred <- fc_store$Forecast[d]
      X_heur <- floor(C_pred / 7)
      rest <- C_pred - 7 * X_heur
      J_heur <- ceiling(rest / 6)
      s0[s, d, 1] <- min(J_heur, max_J)
      s0[s, d, 2] <- min(X_heur, max_X)
      s0[s, d, 3] <- 0.15
    }
  }
  s0 <- c(s0)
  
  tracker <- list(iter = 0, history = data.frame(iter = integer(), best_obj = numeric()))
  
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
  
  eval_fn <- function(sol) {
    val <- eval_plan_O2(sol, forecasts, week_id, max_sales_units, verbose = FALSE)
    tracker$iter <<- tracker$iter + 1
    if(tracker$iter %% 100 == 0) {
      tracker$history <<- rbind(tracker$history, data.frame(iter = tracker$iter, best_obj = val))
    }
    return(val)
  }
  
  result <- hclimbing(par = s0, fn = eval_fn, change = change_fn,
                      lower = lower, upper = upper, type = "max",
                      control = list(maxit = 5000, REPORT = 0))
  
  return(list(solution = result$sol, value = result$value, history = tracker$history,
              metrics = extract_metrics(result$sol, forecasts, week_id, max_sales_units)))
}

# ============================================================================
# 2. RUN SIMULATED ANNEALING (with convergence tracking)
# ============================================================================
run_sann_tracked <- function(seed_val) {
  set.seed(seed_val)
  s0 <- runif(84, min = lower, max = upper)
  
  tracker <- list(iter = 0, best_val = -Inf, history = data.frame(iter = integer(), best_obj = numeric()))
  
  sann_gr <- function(par) {
    hchange(par, lower = lower, upper = upper, operator = "*",
            dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
  }
  
  eval_fn <- function(sol) {
    # SANN minimizes, O2 maximizes, so we negate
    val <- -eval_plan_O2(sol, forecasts = forecasts, week_id = week_id,
                         max_sales_units = max_sales_units, verbose = FALSE)
    tracker$iter <<- tracker$iter + 1
    
    if(!is.na(val) && is.finite(val) && val < tracker$best_val) {
      tracker$best_val <<- val
      tracker$history <<- rbind(tracker$history, data.frame(iter = tracker$iter, best_obj = -tracker$best_val))
    }
    return(val)
  }
  
  result <- optim(par = s0, fn = eval_fn, gr = sann_gr, method = "SANN",
                  control = list(maxit = 5000, temp = 50, trace = FALSE, tmax = 20))
  
  return(list(solution = result$par, value = -result$value, history = tracker$history,
              metrics = extract_metrics(result$par, forecasts, week_id, max_sales_units)))
}

# ============================================================================
# 3. EXECUTE COMPARISON
# ============================================================================
cat("Running O2 comparative optimization...\n")
hc_results <- list()
sann_results <- list()

for(i in 1:n_runs) {
  cat(sprintf("Run %d/%d...\n", i, n_runs))
  hc_results[[i]] <- run_hill_climbing_tracked(seed + i)
  sann_results[[i]] <- run_sann_tracked(seed + i)
}

aggregate_metrics <- function(results_list, method_name) {
  do.call(rbind, lapply(results_list, function(r) {
    data.frame(
      Method = method_name,
      Objective = r$metrics$objective,
      HR = r$metrics$hr,
      Profit = r$metrics$profit
    )
  }))
}

final_comparison <- rbind(
  aggregate_metrics(hc_results, "Hill Climbing"),
  aggregate_metrics(sann_results, "Simulated Annealing")
)

# ============================================================================
# 4. VISUALIZATIONS
# ============================================================================
cat("Generating convergence plots...\n")

# Standardize history columns
hc_hist <- do.call(rbind, lapply(seq_along(hc_results), function(i) {
  h <- hc_results[[i]]$history
  data.frame(Run = i, Method = "Hill Climbing", iter = h$iter, best_obj = h$best_obj)
}))

sann_hist <- do.call(rbind, lapply(seq_along(sann_results), function(i) {
  s <- sann_results[[i]]$history
  data.frame(Run = i, Method = "Simulated Annealing", iter = s$iter, best_obj = s$best_obj)
}))

all_history <- rbind(hc_hist, sann_hist)
avg_conv <- aggregate(best_obj ~ iter + Method, data = all_history, FUN = mean, na.rm = TRUE)

# Plot 1: Convergence
p1 <- ggplot(avg_conv, aes(x = iter, y = best_obj, color = Method)) +
  geom_line(linewidth = 1.2) +
  geom_point(alpha = 0.3) +
  scale_color_manual(values = c("Hill Climbing" = "#2E86AB", "Simulated Annealing" = "#A23B72")) +
  labs(title = "O2 Convergence: Objective vs Iterations",
       x = "Iteration", y = "Objective Value (Profit)", color = "Method") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Plot 2: Final Metrics
p2 <- ggplot(final_comparison, aes(x = Method, y = Objective, fill = Method)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  scale_fill_manual(values = c("Hill Climbing" = "#2E86AB", "Simulated Annealing" = "#A23B72")) +
  labs(title = "O2 Final Objective Values", x = "Method", y = "Profit (USD)") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "none")

# Plot 3: HR vs Profit Trade-off
p3 <- ggplot(final_comparison, aes(x = HR, y = Profit, color = Method, shape = Method)) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(values = c("Hill Climbing" = "#2E86AB", "Simulated Annealing" = "#A23B72")) +
  labs(title = "O2 Pareto View: HR Usage vs Profit",
       x = "Total HR Resources (J+X)", y = "Total Profit (USD)",
       color = "Method", shape = "Method") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", hjust = 0.5))

grid.arrange(p1, p2, p3, ncol = 1)

ggsave("O2_convergence_comparison.png", p1, width = 10, height = 5, dpi = 300)
ggsave("O2_final_metrics.png", p2, width = 8, height = 5, dpi = 300)
ggsave("O2_pareto_comparison.png", p3, width = 10, height = 6, dpi = 300)

# ============================================================================
# 5. SAVE & SUMMARY
# ============================================================================
stats_summary <- final_comparison %>%
  group_by(Method) %>%
  summarise(
    `Avg Objective` = sprintf("%.2f ± %.2f", mean(Objective), sd(Objective)),
    `Avg HR` = sprintf("%.1f ± %.1f", mean(HR), sd(HR)),
    `Avg Profit` = sprintf("$%.0f ± $%.0f", mean(Profit), sd(Profit)),
    `Best Objective` = sprintf("%.2f", max(Objective))
  )

cat("\n=== O2 STATISTICAL COMPARISON ===\n")
print(stats_summary)

saveRDS(list(
  hill_climbing = hc_results,
  simulated_annealing = sann_results,
  final_comparison = final_comparison,
  configuration = list(week_id = week_id, n_runs = n_runs)
), file = "O2_method_comparison_results.rds")

cat("\n✓ O2 comparison complete! Results saved.\n")