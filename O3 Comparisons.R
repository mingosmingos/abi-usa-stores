################################################################################
# compare_optimizers_O3.R
# Runs Hill Climbing, Simulated Annealing, and Genetic Algorithm on Objective O3
# (Profit maximization + HR minimization via alpha-weighted scalarization)
# REQUIREMENTS: hill.R, eval_plan_O3.R, all_store_predictions.csv
# R packages: GA
################################################################################

# 0. Shared dependencies
source("hill.R")
source("eval_plan_O3.R")  # Contains eval_plan_O3 & repair_solution_inplace
library(GA)

# 1. Shared settings
WEEK_ID       <- 20
MAX_UNITS     <- 10000    # O3 hard constraint
ALPHA         <- 0.5      # Weight for profit in: F = alpha*Profit - HR
MAX_ITER      <- 3000     # evaluations for HC and SANN
GA_ITER       <- 30      # generations for GA
POP_SIZE      <- 100
SEED          <- 123

# Fixed bounds matching your O3 implementations
lower <- rep(0, 84)
upper <- c(rep(50, 28), rep(30, 28), rep(0.30, 28))

# 2. Load forecasts (unified format)
cat("Loading forecasts...\n")
raw <- read.csv("all_store_predictions.csv")
forecasts <- data.frame(
  Store    = raw$Store,
  Week_ID  = raw$Run,
  Day      = raw$Step,
  Forecast = raw$Num_Customers
)
cat("Available weeks:", paste(sort(unique(forecasts$Week_ID)), collapse = ", "), "\n\n")

# 3. Shared evaluation wrapper
# eval_plan_O3 already handles repair internally. Returns -(HR - alpha*Profit)
eval_fn_o3 <- function(sol) {
  eval_plan_O3(sol, forecasts = forecasts, week_id = WEEK_ID,
               max_sales_units = MAX_UNITS, verbose = FALSE,
               alpha = ALPHA, return_components = FALSE)
}

# 4. Shared perturbation / neighbourhood generator
change_fn <- function(par) {
  hchange(par, lower = lower, upper = upper, operator = "*",
          dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
}

# 5. Hill Climbing
cat("=== Running Hill Climbing (O3) ===\n")
set.seed(SEED)
s0 <- runif(84, min = lower, max = upper)

hc_track_best <- numeric(MAX_ITER)
hc_iter       <- 0L
hc_eval_tracked <- function(sol) {
  val <- eval_fn_o3(sol)
  hc_iter <<- hc_iter + 1L
  hc_track_best[hc_iter] <<- if (hc_iter == 1L || val > hc_track_best[hc_iter - 1L]) 
    val else hc_track_best[hc_iter - 1L]
  val
}

hc_result <- hclimbing(
  par     = s0,
  fn      = hc_eval_tracked,
  change  = function(par, lower, upper) change_fn(par),
  lower   = lower,
  upper   = upper,
  type    = "max",
  control = list(maxit = MAX_ITER, REPORT = 500, digits = 2)
)
hc_best_obj <- hc_result$eval
hc_curve    <- hc_track_best[seq_len(hc_iter)]
cat(sprintf("HC best weighted obj: $%.2f\n\n", hc_best_obj))

# 6. Simulated Annealing
cat("=== Running Simulated Annealing (O3) ===\n")
set.seed(SEED)
s0 <- runif(84, min = lower, max = upper)

sa_track <- new.env(parent = emptyenv())
sa_track$iter      <- 0L
sa_track$best_val  <- numeric(MAX_ITER + 10L)

eval_fn_sann <- function(sol) {
  val <- -eval_fn_o3(sol)   # optim() minimizes -> negate
  i   <- sa_track$iter + 1L
  sa_track$iter     <- i
  sa_track$best_val[i] <- if (i == 1L || val < sa_track$best_val[i - 1L]) 
    val else sa_track$best_val[i - 1L]
  val
}

sann_result <- optim(
  par     = s0,
  fn      = eval_fn_sann,
  gr      = function(par, lower, upper) change_fn(par),
  method  = "SANN",
  control = list(maxit = MAX_ITER, temp = 50, tmax = 30, trace = FALSE)
)
sa_best_obj <- -sann_result$value
sa_curve    <- -sa_track$best_val[seq_len(sa_track$iter)]
cat(sprintf("SANN best weighted obj: $%.2f\n\n", sa_best_obj))

# 7. Genetic Algorithm
cat("=== Running Genetic Algorithm (O3) ===\n")

ga_fitness <- function(sol, ...) {
  if (is.matrix(sol)) {
    obj <- numeric(nrow(sol))
    for(i in seq_len(nrow(sol))) {
      obj[i] <- eval_fn_o3(sol[i, ])
    }
    return(obj)
  } else {
    return(eval_fn_o3(sol))
  }
}

ga_result <- ga(
  type       = "real-valued",
  fitness    = ga_fitness,
  lower      = lower,
  upper      = upper,
  popSize    = POP_SIZE,
  maxiter    = GA_ITER,       # Fixed: was maxiter <- GA_ITER
  pcrossover = 0.8,
  pmutation  = 0.1,
  elitism    = 10,
  run        = 30,
  monitor    = FALSE,
  seed       = SEED
)
ga_best_obj <- ga_result@fitnessValue
ga_curve    <- ga_result@summary[, "max"]
cat(sprintf("GA   best weighted obj: $%.2f\n\n", ga_best_obj))

# Helper: Extract HR & Profit for reporting
get_components <- function(sol) {
  eval_plan_O3(sol, forecasts, WEEK_ID, MAX_UNITS, verbose = FALSE, 
               alpha = ALPHA, return_components = TRUE)
}

hc_comp <- get_components(hc_result$sol)
sa_comp <- get_components(sann_result$par)
ga_comp <- get_components(ga_result@solution[1, ])

# 8. Comparison Plots
pdf("optimizer_comparison_O3.pdf", width = 12, height = 10)
col_hc   <- "#2196F3"
col_sann <- "#FF9800"
col_ga   <- "#4CAF50"

# Plot 1: Convergence curves (normalised to function evaluations)
ga_evals <- seq(POP_SIZE, by = POP_SIZE, length.out = length(ga_curve))
x_max    <- max(MAX_ITER, max(ga_evals))
y_min    <- min(hc_curve[1], sa_curve[1], ga_curve[1])
y_max    <- max(hc_best_obj, sa_best_obj, ga_best_obj) * 1.05

plot(seq_along(hc_curve), hc_curve,
     type = "l", col = col_hc, lwd = 2,
     xlim = c(0, x_max), ylim = c(y_min, y_max),
     xlab = "Function Evaluations", ylab = "Best Weighted Objective (alpha*Profit - HR)",
     main = paste0("Convergence Comparison - Week ", WEEK_ID, " (O3)"))
lines(seq_along(sa_curve), sa_curve, col = col_sann, lwd = 2)
lines(ga_evals, ga_curve, col = col_ga, lwd = 2, lty = 2)
abline(h = hc_best_obj, col = col_hc, lty = 3, lwd = 1)
abline(h = sa_best_obj, col = col_sann, lty = 3, lwd = 1)
abline(h = ga_best_obj, col = col_ga, lty = 3, lwd = 1)
legend("bottomright",
       legend = c(
         sprintf("Hill Climbing  ($%.1f)", hc_best_obj),
         sprintf("Simul. Annealing ($%.1f)", sa_best_obj),
         sprintf("Genetic Alg. ($%.1f)", ga_best_obj)
       ),
       col = c(col_hc, col_sann, col_ga), lty = c(1, 1, 2), lwd = 2, bg = "white")
grid()

# Plot 2: Bar chart - final best weighted objective
objs <- c(hc_best_obj, sa_best_obj, ga_best_obj)
names(objs) <- c("Hill\nClimbing", "Simulated\nAnnealing", "Genetic\nAlgorithm")
bp <- barplot(objs, col = c(col_hc, col_sann, col_ga), border = NA,
              ylim = c(0, max(objs) * 1.15),
              ylab = "Best Weighted Objective (USD)",
              main = paste0("Final Best Weighted Objective - Week ", WEEK_ID, " (O3)"),
              cex.names = 0.9)
text(bp, objs + max(objs) * 0.02, labels = sprintf("$%.1f", objs), cex = 0.85, font = 2)
grid(nx = NA, ny = NULL)

# Plot 3: O3 Trade-off - HR vs Profit for final GA population
final_pop <- ga_result@population
final_hr   <- numeric(nrow(final_pop))
final_prof <- numeric(nrow(final_pop))
for(i in 1:nrow(final_pop)) {
  comp <- get_components(final_pop[i, ])
  final_hr[i]   <- comp$total_hr
  final_prof[i] <- comp$total_profit
}

plot(final_hr, final_prof,
     col = adjustcolor(col_ga, 0.5), pch = 16, cex = 1.2,
     xlab = "Total HR Resources (J+X)", ylab = "Total Profit (USD)",
     main = paste0("GA Final Population - HR vs Profit Trade-off\nWeek ", WEEK_ID, " (O3)"))
points(ga_comp$total_hr, ga_comp$total_profit, col = "red", pch = 17, cex = 2)
text(ga_comp$total_hr, ga_comp$total_profit, 
     labels = sprintf("Best: HR=%d, $%d", ga_comp$total_hr, round(ga_comp$total_profit)), 
     pos = 3, col = "red", font = 2, cex = 0.9)
grid()

# Plot 4: SANN - best-so-far vs rolling average
window_size <- max(1L, as.integer(MAX_ITER / 100))
sa_rolling  <- stats::filter(sa_curve, rep(1 / window_size, window_size), sides = 1)
plot(seq_along(sa_curve), sa_curve, type = "l", col = col_sann, lwd = 2,
     xlab = "Iteration", ylab = "Weighted Objective (USD)", main = "SANN - Best-so-far vs Rolling Average")
lines(seq_along(sa_rolling), sa_rolling, col = "grey40", lwd = 1.5, lty = 2)
legend("bottomright", legend = c("Best so far", sprintf("Rolling avg (%d iters)", window_size)),
       col = c(col_sann, "grey40"), lty = c(1, 2), lwd = 2, bg = "white")
grid()
dev.off()

# 9. Summary table
cat("\n========== COMPARISON SUMMARY (O3) ==========\n")
cat(sprintf("  %-22s  %10s  %8s  %10s  %12s\n", 
            "Algorithm", "Weighted Obj", "HR", "Profit", "Evaluations"))
cat(sprintf("  %-22s  %10s  %8s  %10s  %12s\n", 
            "---------", "-----------", "--------", "--------", "-----------"))
cat(sprintf("  %-22s  $%9.1f  %8d  $%9.0f  %12d\n", 
            "Hill Climbing",       hc_best_obj, hc_comp$total_hr, hc_comp$total_profit, length(hc_curve)))
cat(sprintf("  %-22s  $%9.1f  %8d  $%9.0f  %12d\n", 
            "Simulated Annealing", sa_best_obj, sa_comp$total_hr, sa_comp$total_profit, length(sa_curve)))
cat(sprintf("  %-22s  $%9.1f  %8d  $%9.0f  %12d\n", 
            "Genetic Algorithm",   ga_best_obj, ga_comp$total_hr, ga_comp$total_profit, max(ga_evals)))
cat("=========================================\n")

# 10. Detailed HR Allocation for Best Solutions
cat("\n========== DETAILED HR ALLOCATION ==========\n\n")

cat("--- Hill Climbing Solution ---\n")
eval_plan_O3(hc_result$sol, forecasts, WEEK_ID, MAX_UNITS, 
             verbose = TRUE, alpha = ALPHA)
cat("\n")

cat("--- Simulated Annealing Solution ---\n")
eval_plan_O3(sann_result$par, forecasts, WEEK_ID, MAX_UNITS, 
             verbose = TRUE, alpha = ALPHA)
cat("\n")

cat("--- Genetic Algorithm Solution ---\n")
eval_plan_O3(ga_result@solution[1, ], forecasts, WEEK_ID, MAX_UNITS, 
             verbose = TRUE, alpha = ALPHA)
cat("\n")
cat("\nPlots saved to: optimizer_comparison_O3.pdf\n")
cat("\n=== O3 COMPARISON COMPLETE ===\n")