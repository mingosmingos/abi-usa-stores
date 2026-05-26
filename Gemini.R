# Load the package
library(particle.swarm.optimisation)

# 1. Define the fitness function
# We want to solve: a*5 + b*25 + 10 = 15
fitness_function <- function(values) {
  a <- values[1]
  b <- values[2]
  
  particule_result <- a * 5 + b * 25 + 10
  difference <- 15 - particule_result
  
  # Maximize fitness by minimizing the difference
  fitness <- 1 / abs(difference)
  return(fitness)
}

# 2. Set the ranges for the values (min and max for 'a' and 'b')
values_ranges <- list(c(-10^3, 10^3), c(-10^3, 10^3))

# 3. Initialize the Swarm
swarm <- ParticleSwarm$new(
  pop_size = 200,
  values_names = list("a", "b"),
  fitness_function = fitness_function,
  max_it = 75,
  acceleration_coefficient_range = list(c(0, 1), c(0, 1)),
  inertia = 0.5,
  ranges_of_values = values_ranges
)

# 4. Run the Optimization
# Setting verbose, plot, and save_file to FALSE for a clean console run
swarm$run(plot = FALSE, verbose = FALSE, save_file = FALSE)

# 5. Extract and print the best solution
cat("Optimal values found:\n")
print(swarm$swarm_best_values)

# Verify the result
result <- swarm$swarm_best_values[[1]] * 5 + swarm$swarm_best_values[[2]] * 25 + 10
cat("\nResult of the equation with these values (Target = 15):", result, "\n")