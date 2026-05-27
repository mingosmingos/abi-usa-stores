################################################################################
# compare_optimizers_O2.R
# Runs Hill Climbing, Simulated Annealing, and Genetic Algorithm on Objective O2
# (same week, same bounds, fair evaluation tracking, O2-specific constraints)
# REQUIREMENTS: hill.R, eval_plan_O2.R, all_store_predictions.csv
# R packages: GA
################################################################################

# 0. Shared dependencies
source("hill.R")
source("eval_plan_O2.R")  # Contains eval_plan_O2 & repair_solution_inplace
library(GA)

# 1. Shared settings
WEEK_ID       <- 20
MAX_UNITS     <- 10000    # O2 hard constraint
MAX_ITER      <- 300     # evaluations for HC and SANN
GA_ITER       <- 5       # generations for GA (approx 5000 evals with popSize=100)
POP_SIZE      <- 10
SEED          <- 12

# Fixed bounds matching your O2 implementations
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
# O2 requires repair before profit calculation. Safely apply if available.
eval_fn_o2 <- function(sol) {
  if (exists("repair_solution_inplace", mode = "function")) {
    sol <- repair_solution_inplace(sol, forecasts, WEEK_ID, MAX_UNITS)
  }
  eval_plan_O2(sol, forecasts = forecasts, week_id = WEEK_ID, 
               max_sales_units = MAX_UNITS, verbose = FALSE)
}

# 4. Shared perturbation / neighbourhood generator
change_fn <- function(par) {
  hchange(par, lower = lower, upper = upper, operator = "*",
          dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
}

# 5. Hill Climbing
cat("=== Running Hill Climbing (O2) ===\n")
set.seed(SEED)
s0 <- runif(84, min = lower, max = upper)

hc_track_best <- numeric(MAX_ITER)
hc_iter       <- 0L
hc_eval_tracked <- function(sol) {
  val <- eval_fn_o2(sol)
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
hc_best_profit <- hc_result$eval
hc_curve       <- hc_track_best[seq_len(hc_iter)]
cat(sprintf("HC best profit:   $%.2f\n\n", hc_best_profit))

# 6. Simulated Annealing
cat("=== Running Simulated Annealing (O2) ===\n")
set.seed(SEED)
s0 <- runif(84, min = lower, max = upper)

sa_track <- new.env(parent = emptyenv())
sa_track$iter      <- 0L
sa_track$best_val  <- numeric(MAX_ITER + 10L)

# optim() minimizes -> negate O2 profit
eval_fn_sann <- function(sol) {
  val <- -eval_fn_o2(sol)
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
sa_best_profit <- -sann_result$value
sa_curve       <- -sa_track$best_val[seq_len(sa_track$iter)]
cat(sprintf("SANN best profit: $%.2f\n\n", sa_best_profit))

# 7. Genetic Algorithm
cat("=== Running Genetic Algorithm (O2) ===\n")

ga_fitness <- function(sol, ...) {
  # GA evaluates in parallel batches; repair must be safe for matrix inputs
  if (is.matrix(sol)) {
    profit <- numeric(nrow(sol))
    for(i in seq_len(nrow(sol))) {
      row_sol <- sol[i, ]
      if (exists("repair_solution_inplace", mode = "function")) {
        row_sol <- repair_solution_inplace(row_sol, forecasts, WEEK_ID, MAX_UNITS)
      }
      profit[i] <- eval_plan_O2(row_sol, forecasts, WEEK_ID, MAX_UNITS, FALSE)
    }
    return(profit)
  } else {
    return(eval_fn_o2(sol))
  }
}

ga_result <- ga(
  type       = "real-valued",
  fitness    = ga_fitness,
  lower      = lower,
  upper      = upper,
  popSize    = POP_SIZE,
  maxiter    <- GA_ITER,
  pcrossover = 0.8,
  pmutation  = 0.1,
  elitism    = 10,
  run        = 30,       # early stop if no improvement for 30 gens
  monitor    = FALSE,
  seed       = SEED
)
ga_best_profit <- ga_result@fitnessValue
ga_curve       <- ga_result@summary[, "max"]
cat(sprintf("GA   best profit: $%.2f\n\n", ga_best_profit))

# 8. Comparison Plots
pdf("optimizer_comparison_O2.pdf", width = 12, height = 10)
col_hc   <- "#2196F3"
col_sann <- "#FF9800"
col_ga   <- "#4CAF50"

# Plot 1: Convergence curves (normalised to function evaluations)
ga_evals <- seq(POP_SIZE, by = POP_SIZE, length.out = length(ga_curve))
x_max    <- max(MAX_ITER, max(ga_evals))
y_min    <- min(hc_curve[1], sa_curve[1], ga_curve[1])
y_max    <- max(hc_best_profit, sa_best_profit, ga_best_profit) * 1.05

plot(seq_along(hc_curve), hc_curve,
     type = "l", col = col_hc, lwd = 2,
     xlim = c(0, x_max), ylim = c(y_min, y_max),
     xlab = "Function Evaluations", ylab = "Best Profit Found (USD)",
     main = paste0("Convergence Comparison - Week ", WEEK_ID, " (O2)"))
lines(seq_along(sa_curve), sa_curve, col = col_sann, lwd = 2)
lines(ga_evals, ga_curve, col = col_ga, lwd = 2, lty = 2)
abline(h = hc_best_profit, col = col_hc, lty = 3, lwd = 1)
abline(h = sa_best_profit, col = col_sann, lty = 3, lwd = 1)
abline(h = ga_best_profit, col = col_ga, lty = 3, lwd = 1)
legend("bottomright",
       legend = c(
         sprintf("Hill Climbing  ($%.0f)", hc_best_profit),
         sprintf("Simul. Annealing ($%.0f)", sa_best_profit),
         sprintf("Genetic Alg. ($%.0f)", ga_best_profit)
       ),
       col = c(col_hc, col_sann, col_ga), lty = c(1, 1, 2), lwd = 2, bg = "white")
grid()

# Plot 2: Bar chart - final best profit per algorithm
profits <- c(hc_best_profit, sa_best_profit, ga_best_profit)
names(profits) <- c("Hill\nClimbing", "Simulated\nAnnealing", "Genetic\nAlgorithm")
bp <- barplot(profits, col = c(col_hc, col_sann, col_ga), border = NA,
              ylim = c(0, max(profits) * 1.15),
              ylab = "Best Profit (USD)",
              main = paste0("Final Best Profit - Week ", WEEK_ID, " (O2)"),
              cex.names = 0.9)
text(bp, profits + max(profits) * 0.02, labels = sprintf("$%.0f", profits), cex = 0.85, font = 2)
grid(nx = NA, ny = NULL)

# Plot 3: GA - fitness distribution across final population
final_pop_fitness <- apply(ga_result@population, 1, ga_fitness)
hist(final_pop_fitness, col = adjustcolor(col_ga, 0.6), border = "white", breaks = 20,
     xlab = "Profit (USD)", main = paste0("GA Final Population - Fitness Distribution\nWeek ", WEEK_ID, " (O2)"),
     freq = TRUE)
abline(v = ga_best_profit, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = sprintf("Best: $%.0f", ga_best_profit), col = "red", lty = 2, lwd = 2, bg = "white")
grid()

# Plot 4: SANN - best-so-far vs rolling average
window_size <- max(1L, as.integer(MAX_ITER / 100))
sa_rolling  <- stats::filter(sa_curve, rep(1 / window_size, window_size), sides = 1)
plot(seq_along(sa_curve), sa_curve, type = "l", col = col_sann, lwd = 2,
     xlab = "Iteration", ylab = "Profit (USD)", main = "SANN - Best-so-far vs Rolling Average")
lines(seq_along(sa_rolling), sa_rolling, col = "grey40", lwd = 1.5, lty = 2)
legend("bottomright", legend = c("Best so far", sprintf("Rolling avg (%d iters)", window_size)),
       col = c(col_sann, "grey40"), lty = c(1, 2), lwd = 2, bg = "white")
grid()
dev.off()

# 9. Summary table
cat("\n========== COMPARISON SUMMARY (O2) ==========\n")
cat(sprintf("  %-22s  %12s  %12s\n", "Algorithm", "Best Profit", "Evaluations"))
cat(sprintf("  %-22s  %12s  %12s\n", "---------", "-----------", "-----------"))
cat(sprintf("  %-22s  $%11.2f  %12d\n", "Hill Climbing",       hc_best_profit,  length(hc_curve)))
cat(sprintf("  %-22s  $%11.2f  %12d\n", "Simulated Annealing", sa_best_profit,  length(sa_curve)))
cat(sprintf("  %-22s  $%11.2f  %12d\n", "Genetic Algorithm",   ga_best_profit,  max(ga_evals)))
cat("=========================================\n")
cat("\nPlots saved to: optimizer_comparison_O2.pdf\n")
cat("\n=== O2 COMPARISON COMPLETE ===\n")