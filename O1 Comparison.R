################################################################################
# compare_optimizers_O1.R
# Runs Hill Climbing, Simulated Annealing, and Genetic Algorithm on the same
# problem (same week, same seed logic) and produces comparison plots.
#
# REQUIREMENTS (must exist in your working directory):
#   hill.R, eval_plan_O1.R, all_store_predictions.csv
#   R packages: GA
################################################################################

# ── 0. Shared dependencies ────────────────────────────────────────────────────
source("hill.R")
source("eval_plan_O1.R")
library(GA)

# ── 1. Shared settings ────────────────────────────────────────────────────────
WEEK_ID   <- 20
MAX_J     <- 50
MAX_X     <- 30
MAX_ITER  <- 5000       # iterations for HC and SANN
GA_ITER   <- 100        # generations for GA  (each gen ≈ popSize evals)
POP_SIZE  <- 100
SEED      <- 123

lower <- rep(0, 84)
upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(0.30, 28))

# ── 2. Load forecasts (unified format) ───────────────────────────────────────
cat("Loading forecasts...\n")
raw <- read.csv("all_store_predictions.csv")
forecasts <- data.frame(
  Store    = raw$Store,
  Week_ID  = raw$Run,
  Day      = raw$Step,
  Forecast = raw$Num_Customers
)
cat("Available weeks:", paste(sort(unique(forecasts$Week_ID)), collapse = ", "), "\n\n")

# ── 3. Shared evaluation wrapper ──────────────────────────────────────────────
eval_fn <- function(sol) {
  eval_plan_O1(sol, forecasts = forecasts, week_id = WEEK_ID, verbose = FALSE)
}

# ── 4. Shared perturbation / neighbourhood generator ─────────────────────────
change_fn <- function(par) {
  hchange(par, lower = lower, upper = upper, operator = "*",
          dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
}

# ── 5. Hill Climbing ──────────────────────────────────────────────────────────
cat("=== Running Hill Climbing ===\n")
set.seed(SEED)
s0 <- runif(84, min = lower, max = upper)

hc_track_best <- numeric(MAX_ITER)
hc_iter       <- 0L

hc_eval_tracked <- function(sol) {
  val <- eval_fn(sol)
  hc_iter        <<- hc_iter + 1L
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

# ── 6. Simulated Annealing ────────────────────────────────────────────────────
cat("=== Running Simulated Annealing ===\n")
set.seed(SEED)
s0 <- runif(84, min = lower, max = upper)

sa_track <- new.env(parent = emptyenv())
sa_track$iter      <- 0L
sa_track$best_val  <- numeric(MAX_ITER + 10L)  # pre-allocate (negated profit)

eval_fn_sann <- function(sol) {
  val <- -eval_fn(sol)   # optim() minimises → negate
  i   <- sa_track$iter + 1L
  sa_track$iter     <- i
  sa_track$best_val[i] <- if (i == 1L || val < sa_track$best_val[i - 1L])
    val else sa_track$best_val[i - 1L]
  val
}

sann_result <- optim(
  par     = s0,
  fn      = eval_fn_sann,
  gr      = function(par) change_fn(par),
  method  = "SANN",
  control = list(maxit = MAX_ITER, temp = 50, tmax = 30, trace = FALSE)
)

sa_best_profit <- -sann_result$value
sa_curve       <- -sa_track$best_val[seq_len(sa_track$iter)]
cat(sprintf("SANN best profit: $%.2f\n\n", sa_best_profit))

# ── 7. Genetic Algorithm ──────────────────────────────────────────────────────
cat("=== Running Genetic Algorithm ===\n")

# GA tracks best-per-generation internally via @summary
ga_result <- ga(
  type       = "real-valued",
  fitness    = eval_fn,
  lower      = lower,
  upper      = upper,
  popSize    = POP_SIZE,
  maxiter    = GA_ITER,
  pcrossover = 0.8,
  pmutation  = 0.1,
  elitism    = 10,
  run        = 50,       # early stop if no improvement for 50 gens
  monitor    = FALSE,
  seed       = SEED
)

ga_best_profit <- ga_result@fitnessValue
# ga_result@summary is a matrix: rows = generations, col "max" = best fitness
ga_curve       <- ga_result@summary[, "max"]
cat(sprintf("GA   best profit: $%.2f\n\n", ga_best_profit))

# ── 8. Comparison Plots ───────────────────────────────────────────────────────
pdf("optimizer_comparison_O1.pdf", width = 12, height = 10)

# Colour palette
col_hc   <- "#2196F3"   # blue
col_sann <- "#FF9800"   # orange
col_ga   <- "#4CAF50"   # green

# ── Plot 1: Convergence curves (normalised to function evaluations) ──────────
# HC and SANN: x = evaluation index (1..MAX_ITER)
# GA: each generation = popSize evaluations → rescale
ga_evals <- seq(POP_SIZE, by = POP_SIZE, length.out = length(ga_curve))

x_max <- max(MAX_ITER, max(ga_evals))
y_min <- min(hc_curve[1], sa_curve[1], ga_curve[1])
y_max <- max(hc_best_profit, sa_best_profit, ga_best_profit) * 1.05

plot(seq_along(hc_curve), hc_curve,
     type = "l", col = col_hc, lwd = 2,
     xlim = c(0, x_max), ylim = c(y_min, y_max),
     xlab = "Function Evaluations", ylab = "Best Profit Found (USD)",
     main = paste0("Convergence Comparison – Week ", WEEK_ID, " (O1)"))
lines(seq_along(sa_curve), sa_curve, col = col_sann, lwd = 2)
lines(ga_evals, ga_curve, col = col_ga, lwd = 2, lty = 2)

abline(h = hc_best_profit,   col = col_hc,   lty = 3, lwd = 1)
abline(h = sa_best_profit,   col = col_sann,  lty = 3, lwd = 1)
abline(h = ga_best_profit,   col = col_ga,    lty = 3, lwd = 1)

legend("bottomright",
       legend = c(
         sprintf("Hill Climbing  ($%.0f)", hc_best_profit),
         sprintf("Simul. Annealing ($%.0f)", sa_best_profit),
         sprintf("Genetic Alg. ($%.0f)", ga_best_profit)
       ),
       col = c(col_hc, col_sann, col_ga),
       lty = c(1, 1, 2), lwd = 2, bg = "white")
grid()

# ── Plot 2: Bar chart – final best profit per algorithm ──────────────────────
profits <- c(hc_best_profit, sa_best_profit, ga_best_profit)
names(profits) <- c("Hill\nClimbing", "Simulated\nAnnealing", "Genetic\nAlgorithm")

bar_cols <- c(col_hc, col_sann, col_ga)
bp <- barplot(profits,
              col    = bar_cols,
              border = NA,
              ylim   = c(0, max(profits) * 1.15),
              ylab   = "Best Profit (USD)",
              main   = paste0("Final Best Profit – Week ", WEEK_ID, " (O1)"),
              cex.names = 0.9)
text(bp, profits + max(profits) * 0.02,
     labels = sprintf("$%.0f", profits),
     cex = 0.85, font = 2)
grid(nx = NA, ny = NULL)

# ── Plot 3: GA – fitness distribution across final population ─────────────────
# Evaluate every individual in the final GA population
final_pop_fitness <- apply(ga_result@population, 1, eval_fn)

hist(final_pop_fitness,
     col    = adjustcolor(col_ga, 0.6),
     border = "white",
     breaks = 20,
     xlab   = "Profit (USD)",
     main   = paste0("GA Final Population – Fitness Distribution\nWeek ", WEEK_ID, " (O1)"),
     freq   = TRUE)
abline(v = ga_best_profit, col = "red", lwd = 2, lty = 2)
legend("topleft",
       legend = sprintf("Best: $%.0f", ga_best_profit),
       col = "red", lty = 2, lwd = 2, bg = "white")
grid()

# ── Plot 4: SANN – current vs best (exploration vs exploitation) ───────────────
sa_current <- -sa_track$best_val  # this was stored as "best so far"; current is not
# Note: optim/SANN does not expose current-candidate easily without wrapping further.
# We show best-so-far vs running average as a proxy.
window_size <- max(1L, as.integer(MAX_ITER / 100))
sa_rolling  <- stats::filter(sa_curve, rep(1 / window_size, window_size), sides = 1)

plot(seq_along(sa_curve), sa_curve,
     type = "l", col = col_sann, lwd = 2,
     xlab = "Iteration", ylab = "Profit (USD)",
     main = "SANN – Best-so-far vs Rolling Average")
lines(seq_along(sa_rolling), sa_rolling, col = "grey40", lwd = 1.5, lty = 2)
legend("bottomright",
       legend = c("Best so far", sprintf("Rolling avg (%d iters)", window_size)),
       col = c(col_sann, "grey40"), lty = c(1, 2), lwd = 2, bg = "white")
grid()

dev.off()

# ── 9. Summary table ─────────────────────────────────────────────────────────
cat("\n========== COMPARISON SUMMARY ==========\n")
cat(sprintf("  %-22s  %12s  %12s\n", "Algorithm", "Best Profit", "Evaluations"))
cat(sprintf("  %-22s  %12s  %12s\n", "---------", "-----------", "-----------"))
cat(sprintf("  %-22s  $%11.2f  %12d\n", "Hill Climbing",       hc_best_profit,  length(hc_curve)))
cat(sprintf("  %-22s  $%11.2f  %12d\n", "Simulated Annealing", sa_best_profit,  length(sa_curve)))
cat(sprintf("  %-22s  $%11.2f  %12d\n", "Genetic Algorithm",   ga_best_profit,  max(ga_evals)))
cat("=========================================\n")
cat("\nPlots saved to: optimizer_comparison_O1.pdf\n")
cat("\n=== COMPARISON COMPLETE ===\n")