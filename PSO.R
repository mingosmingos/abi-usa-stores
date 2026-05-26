library(particle.swarm.optimisation)
source("eval_plan_O2.R") # Loads eval_plan_O2() into environment

# 2. Preprocess forecasts CSV to match function expectations
forecasts <- read.csv("all_store_predictions.csv", stringsAsFactors = FALSE)
colnames(forecasts) <- c("Week_ID", "Day", "Forecast", "Store")
forecasts$Week_ID <- as.integer(forecasts$Week_ID)
forecasts$Day     <- as.integer(forecasts$Day)

# 3. Define search space: 4 stores * 7 days * 3 vars = 84 dimensions
n_dims <- 84
ranges_of_values <- vector("list", n_dims)
for (i in seq_len(n_dims)) {
  var_type <- ((i - 1) %% 3) + 1  # 1=J, 2=X, 3=PR
  if (var_type == 1)      ranges_of_values[[i]] <- c(0, 25)   # Junior HR
  else if (var_type == 2) ranges_of_values[[i]] <- c(0, 25)   # Expert HR
  else                    ranges_of_values[[i]] <- c(0, 0.30) # Promotion
}

# 4. Create fitness wrapper (bridges PSO -> your eval function)
target_week   <- 1L
max_units     <- 5000  # Adjust based on your business constraint

fitness_fn <- function(values) {
  # Reshape 1D PSO vector to 4x7x3 array (matches R's column-major fill order)
  sol <- array(values, dim = c(4, 7, 3))
  
  # Evaluate safely to prevent PSO crashes on edge cases
  score <- tryCatch({
    -eval_plan_O2(sol, forecasts, target_week, 
                 max_sales_units = max_units, verbose = FALSE)
  }, error = function(e) -1e6)
  
  # O2 returns -1e9 for hard constraint violations. Convert to stable low fitness.
  if (is.na(score) || score <= -1e8) return(-1e6)
  return(score)  # PSO MAXIMIZES, so higher profit = better
}

# 5. Initialize & Run PSO
swarm <- ParticleSwarm$new(
  pop_size = 30,
  values_names = NULL,  # Optional: rep(c("J","X","PR"), 28)
  fitness_function = fitness_fn,
  max_it = 50,
  acceleration_coefficient_range = list(c(0.5, 2.0), c(0.5, 2.0)),
  inertia = 0.7,
  ranges_of_values = ranges_of_values
)

cat("🚀 Starting PSO optimization...\n")
swarm$run(verbose = TRUE, plot = FALSE, save_file = FALSE)

# 6. Extract & Interpret Results
best_sol_flat <- swarm$swarm_best_values
best_profit   <- swarm$swarm_best_fitness
best_sol      <- array(best_sol_flat, dim = c(4, 7, 3))

cat("\n✅ Optimization Complete!\n")
cat(sprintf("Best Profit: $%.2f\n", best_profit))
cat("Optimal Schedule (J, X, PR per Store-Day):\n")
print(best_sol)