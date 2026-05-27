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
max_iter     <- 5000
initial_temp <- 50          # SANN: initial temperature
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

# --- INSERT BEFORE SECTION 5 ---
# Initialize tracker for convergence plot
tracker <- list(
  iter = 0,
  best_val = Inf,  # For minimization, start with +Inf
  history = data.frame(iter = integer(), best_val = numeric())
)

# --- REPLACE SECTION 5 WITH THIS ---
# 5. AVALIAÇÃO (SANN MINIMIZES BY DEFAULT → negate profit)
eval_fn_sann <- function(sol) {
  # 1. Calculate value (negative for minimization)
  val <- -eval_plan_O2(sol, forecasts = forecasts, week_id = week_id, 
                       max_sales_units = 10000, verbose = FALSE)
  
  # 2. Update Tracker
  tracker$iter <<- tracker$iter + 1
  
  # We want to minimize 'val' (which is -profit), so we look for the lowest number
  if(!is.na(val) && is.finite(val) && val < tracker$best_val) {
    tracker$best_val <<- val
  }
  
  # Store history - only if val is valid
  if(!is.na(val) && is.finite(val)) {
    new_row <- data.frame(iter = tracker$iter, best_val = tracker$best_val)
    tracker$history <<- rbind(tracker$history, new_row)
  }
  
  # Print progress every 500 iterations
  if(tracker$iter %% 500 == 0) {
    cat(sprintf("Iteration %d: current_val = %.2f, best_val = %.2f\n", 
                tracker$iter, val, tracker$best_val))
  }
  
  # 3. Return value for optim
  return(val)
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

# optim() minimizes by default. We pass the negated profit.
sann_result <- optim(par = s0,
                     fn = eval_fn_sann,
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
eval_plan_O2(sann_result$par, forecasts = forecasts, week_id = week_id, verbose = TRUE)

# 10. GUARDAR RESULTADOS
saveRDS(sann_result, file = paste0("resultado_SANN_O1_semana_", week_id, ".rds"))
cat("\nResultado guardado em: resultado_SANN_O1_semana_", week_id, ".rds\n")
cat("\n=== FIM ===\n")

# --- ADD AT THE END OF FILE (SECTION 11) ---
# 11. VISUALIZAÇÃO DA CONVERGÊNCIA (O2)
cat("\n=== Gerando gráfico de convergência (O2) ===\n")

if(nrow(tracker$history) > 0) {
  df_plot <- tracker$history
  # Flip sign back to positive profit for display
  df_plot$Profit <- -df_plot$best_val
  
  plot(df_plot$iter, df_plot$Profit, 
       type = "l", col = "steelblue", lwd = 2,
       xlab = "Iteração", ylab = "Melhor Lucro Líquido (O2)", 
       main = paste("Convergência SANN - O2 Semana", week_id),
       ylim = c(min(df_plot$Profit) * 0.95, max(df_plot$Profit) * 1.05))
  
  grid()
  best_profit <- max(df_plot$Profit)
  abline(h = best_profit, lty = 2, col = "red")
  text(x = nrow(df_plot) * 0.8, y = best_profit, 
       labels = paste("Max:", round(best_profit, 2)), pos = 3, col = "red")
  
  cat("Gráfico gerado com sucesso.\n")
} else {
  cat("ERRO: Nenhum dado de iteração encontrado para plotar.\n")
}