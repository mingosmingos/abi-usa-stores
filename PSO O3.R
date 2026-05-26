library(particle.swarm.optimisation)
source("eval_plan_O3.R")

# 2. Load & format forecasts (matches your SANN scripts)
raw <- read.csv("all_store_predictions.csv")
forecasts <- data.frame(
  Store    = raw$Store,
  Week_ID  = raw$Run,
  Day      = raw$Step,
  Forecast = raw$Num_Customers
)

# 3. Configuration (aligns with your SANN setups)
week_id         <- 20          # Change to 51 or any available week
max_sales_units <- 10000       # Hard constraint
max_J           <- 50          # Upper bound for Junior staff
max_X           <- 30          # Upper bound for Expert staff

# 4. Define search space bounds (84 dimensions: 28 J + 28 X + 28 PR)
# R fills arrays column-major: [store, day, var]
ranges_of_values <- c(
  rep(list(c(0, max_J)), 28),
  rep(list(c(0, max_X)), 28),
  rep(list(c(0, 0.30)), 28)
)

# 5. Fitness wrapper (bridges PSO vector -> eval_plan_O3)
fitness_fn <- function(values) {
  tryCatch({
    # eval_plan_O3 already returns -(HR - profit*0.1) for maximization
    eval_plan_O3(
      sol             = values,
      forecasts       = forecasts,
      week_id         = week_id,
      max_sales_units = max_sales_units,
      verbose         = FALSE
    )
  }, error = function(e) -1e6) # Safe fallback on unexpected errors
}

# 6. Initialize PSO
swarm <- ParticleSwarm$new(
  pop_size = 60,
  values_names = NULL, # Optional: rep(c("J","X","PR"), each=28)
  fitness_function = fitness_fn,
  max_it = 150,
  acceleration_coefficient_range = list(c(0.5, 2.0), c(0.5, 2.0)),
  inertia = 0.7,
  ranges_of_values = ranges_of_values
)

cat("🚀 Starting PSO optimization for O3 (Week", week_id, ")...\n")
start_time <- Sys.time()
swarm$run(verbose = TRUE, plot = FALSE, save_file = FALSE)
cat(sprintf("⏱️  Finished in %.2f seconds.\n", as.numeric(difftime(Sys.time(), start_time, units = "secs"))))

# 7. Extract & Verify Results
best_sol_flat <- swarm$swarm_best_values
best_score    <- swarm$swarm_best_fitness

cat("\n✅ Optimization Complete!\n")
cat(sprintf("Best O3 Score (max): %.4f\n", best_score))
cat("Optimal Schedule (J, X, PR per Store-Day):\n")

# Reshape to 4x7x3 for readability
dim(best_sol_flat) <- c(4, 7, 3)
print(best_sol_flat)

# 🔍 Detailed verification using your function's verbose mode
cat("\n📊 Detailed Evaluation of Best Solution:\n")
eval_plan_O3(
  sol             = best_sol_flat,
  forecasts       = forecasts,
  week_id         = week_id,
  max_sales_units = max_sales_units,
  verbose         = TRUE
)