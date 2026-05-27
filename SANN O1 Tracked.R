################################################################################
# run_sann_O1.R - Simulated Annealing version of run_hill_climbing_O1.R
################################################################################
rm(list = ls())

# Load dependencies
source("hill.R")           # Still needed for hchange()
source("eval_plan_O1.R")   # Objective function
source("eval_plan_O2.R")

# 1. CARREGAR PREVISÕES (identical to original)
cat("Carregando previsões...\n")
# raw <- read.csv("all_store_predictions.csv")
# forecasts <- data.frame(
#   Store    = raw$Store,
#   Week_ID  = raw$Run,
#   Day      = raw$Step,
#   Forecast = raw$Num_Customers
# )

forecasts <- read.csv("all_store_predictions.csv")
names(forecasts) <- c("Week_ID", "Day", "Forecast", "Store")

semanas_disponiveis <- sort(unique(forecasts$Week_ID))
cat("Semanas disponíveis:", paste(semanas_disponiveis, collapse = ", "), "\n")

# 2. CONFIGURAÇÕES
week_id      <- 20
max_J        <- 50
max_X        <- 30
max_iter     <- 3000
initial_temp <- 5          # SANN: initial temperature
seed         <- 122

# 3. DEFINIR LIMITES
lower <- rep(0, 84)
upper <- c(rep(max_J, 28), rep(max_X, 28), rep(0.30, 28))

# 4. SANN NEIGHBORHOOD GENERATOR (gr argument)
# In optim(method="SANN"), gr is NOT a gradient. It's a perturbation function.
sann_gr <- function(par) {
  hchange(par, lower = lower, upper = upper, operator = "*",
          dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
}

# 5. AVALIAÇÃO (SANN MINIMIZES BY DEFAULT → negate profit)
eval_fn_sann <- function(sol) {
  # Return negative profit so optim minimizes -profit == maximizes profit
  -eval_plan_O1(sol, forecasts = forecasts, week_id = week_id, verbose = FALSE)
  #-eval_plan_O2(sol, forecasts = forecasts, week_id = week_id, verbose = FALSE)
}

# 6. SOLUÇÃO INICIAL ALEATÓRIA
set.seed(seed)
s0 <- runif(84, min = lower, max = upper)

# 7. EXECUTAR SIMULATED ANNEALING
# cat("\n=== Executando Simulated Annealing para O1 ===\n")
# cat("Semana:", week_id, "\n")
# cat("Iterações máximas:", max_iter, "\n")
# cat("Temperatura inicial:", initial_temp, "\n\n")

control_sann <- list(maxit = max_iter, temp = initial_temp, trace = TRUE, tmax = 30)

# 5. CONVERGENCE TRACKING SETUP
track <- new.env()
track$iter <- 0
track$current_val <- numeric(max_iter + 5)  # Pre-allocate safely
track$best_val    <- numeric(max_iter + 5)

# Wrapper that logs values while calling your original objective
eval_fn_sann_trace <- function(sol) {
  val <- eval_fn_sann(sol)
  track$iter <- track$iter + 1
  track$current_val[track$iter] <- val
  
  # Track best-so-far (SANN minimizes, so lower -profit = better)
  if (track$iter == 1 || val < track$best_val[track$iter - 1]) {
    track$best_val[track$iter] <- val
  } else {
    track$best_val[track$iter] <- track$best_val[track$iter - 1]
  }
  return(val)
}

# optim() minimizes by default. We pass the negated profit.
sann_result <- optim(par = s0,
                     fn = eval_fn_sann_trace,
                     gr = sann_gr,          # Neighborhood generator
                     method = "SANN",
                     control = control_sann)

# 8. RESULTADOS
best_sol   <- sann_result$par
best_profit <- -sann_result$value          # Convert back to positive profit

cat("\n=== MELHOR SOLUÇÃO ENCONTRADA ===\n")
cat("Lucro total: $", round(best_profit, 2), "\n\n")

# dim(best_sol) <- c(4, 7, 3)
# store_names <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
# for(s in 1:4) {
#   cat("\n", store_names[s], ":\n", sep = "")
#   cat("     Dia  |  J  |  X  |   PR  \n")
#   cat("     -----|-----|-----|-------\n")
#   for(d in 1:7) {
#     J_val  <- round(best_sol[s, d, 1])
#     X_val  <- round(best_sol[s, d, 2])
#     PR_val <- round(best_sol[s, d, 3], 3)
#     cat(sprintf("       %d   | %3d | %3d | %.3f\n", d, J_val, X_val, PR_val))
#   }
# }

# 9. DETALHES DO LUCRO
cat("\n=== DETALHES DO LUCRO POR LOJA/DIA ===\n")
eval_plan_O1(sann_result$par, forecasts = forecasts, week_id = week_id, verbose = TRUE)

# 10. GUARDAR RESULTADOS
saveRDS(sann_result, file = paste0("resultado_SANN_O1_semana_", week_id, ".rds"))
cat("\nResultado guardado em: resultado_SANN_O1_semana_", week_id, ".rds\n")
cat("\n=== FIM ===\n")

valid_iters <- seq_len(track$iter)
best_profits <- -track$best_val[valid_iters]  # Convert back to positive profit

plot(valid_iters, best_profits, type = "l", col = "steelblue", lwd = 2,
     xlab = "Iteration", ylab = "Best Profit Found (USD)",
     main = "Simulated Annealing Convergence (O2)")
abline(h = max(best_profits), col = "red", lty = 2)
grid()
legend("bottomright", legend = c("Best Profit", "Final Profit"),
       col = c("steelblue", "red"), lty = c(1, 2), lwd = 2)