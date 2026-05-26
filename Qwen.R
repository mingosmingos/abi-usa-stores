library(particle.swarm.optimisation)

# 2. Define Fitness Function
# ⚠️ IMPORTANT: This package MAXIMISES fitness. 
# Example from docs: Find 'a' and 'b' such that a*5 + b*25 + 10 = 15
fitness_function <- function(values) {
  a <- values[1]
  b <- values[2]
  particule_result <- a * 5 + b * 25 + 10
  difference <- 15 - particule_result
  fitness <- 1 - abs(difference)  # Max fitness = 1 when difference = 0
  return(fitness)
}

# 3. Define search space bounds for each variable
ranges_of_values <- list(c(-1000, 1000), c(-1000, 1000))

# 4. Initialize the Particle Swarm
swarm <- ParticleSwarm$new(
  pop_size = 50,
  values_names = c("a", "b"),
  fitness_function = fitness_function,
  max_it = 100,
  acceleration_coefficient_range = list(c(0.5, 1.5), c(0.5, 1.5)), # c1 range, c2 range
  inertia = 0.5,
  ranges_of_values = ranges_of_values
)

# 5. Run the Optimization
# run() automatically handles population generation, iteration, and plotting/saving if enabled
swarm$run(verbose = TRUE, plot = FALSE, save_file = FALSE)

# 6. Extract & Verify Results
cat("\n✅ Optimization Complete!\n")
cat("Best parameters found:\n")
print(swarm$swarm_best_values)
cat(sprintf("Best fitness score: %.6f\n", swarm$swarm_best_fitness))

# Quick verification
a_opt <- swarm$swarm_best_values[[1]]
b_opt <- swarm$swarm_best_values[[2]]
cat(sprintf("Verification: %.4f*5 + %.4f*25 + 10 = %.4f (Target: 15)\n", 
            a_opt, b_opt, a_opt*5 + b_opt*25 + 10))