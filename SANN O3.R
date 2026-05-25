################################################################################
# run_sann_O3.R - Simulated Annealing for Multi-Objective Plan (Profit - HR)
################################################################################
rm(list = ls())

# Load dependencies
source("hill.R")           # For hchange()
source("eval_plan_O3.R")   # Multi-objective evaluator with built-in repair

# 1. CARREGAR PREVISÕES
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
week_id          <- 20
max_J            <- 50
max_X            <- 30
max_iter         <- 5000
initial_temp     <- 50
seed             <- 123
max_sales_units  <- 10000   # Rígida: vendas não podem exceder este valor
weight_hr        <- 0.1      # Penalização por unidade de HR (trade-off)

# 3. DEFINIR LIMITES
lower <- rep(0, 84)
upper <- c(rep(max_J, 28), rep(max_X, 28), rep(0.30, 28))

# 4. SANN NEIGHBORHOOD GENERATOR
sann_gr <- function(par) {
  hchange(par, lower = lower, upper = upper, operator = "*",
          dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
}

# 5. FUNÇÃO DE AVALIAÇÃO (SANN minimiza → negamos o objetivo)
eval_fn_sann <- function(sol) {
  -eval_plan_O3(sol, forecasts = forecasts, week_id = week_id, 
                max_sales_units = max_sales_units, weight_hr = weight_hr, verbose = FALSE)
}

# 6. SOLUÇÃO INICIAL ALEATÓRIA
set.seed(seed)
s0 <- runif(84, min = lower, max = upper)

# 7. EXECUTAR SIMULATED ANNEALING
cat("\n=== Executando Simulated Annealing para O3 ===\n")
cat("Semana:", week_id, "\n")
cat("Iterações máximas:", max_iter, "\n")
cat("Limite de vendas:", max_sales_units, "\n")
cat("Peso do HR (weight_hr):", weight_hr, "\n\n")

control_sann <- list(maxit = max_iter, temp = initial_temp, trace = TRUE, tmax = 20)

sann_result <- optim(par = s0,
                     fn = eval_fn_sann,
                     gr = sann_gr,
                     method = "SANN",
                     control = control_sann)

# 8. RESULTADOS
best_sol   <- sann_result$par
best_obj   <- -sann_result$value  # Valor da função objetivo (profit - weight_hr*HR)

cat("\n=== MELHOR SOLUÇÃO ENCONTRADA ===\n")
cat("Função objetivo (max): $", round(best_obj, 2), "\n\n")

# Exibir plano por loja/dia
dim(best_sol) <- c(4, 7, 3)
store_names <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
for(s in 1:4) {
  cat("\n", store_names[s], ":\n", sep = "")
  cat("     Dia  |  J  |  X  |   PR  \n")
  cat("     -----|-----|-----|-------\n")
  for(d in 1:7) {
    J_val  <- round(best_sol[s, d, 1])
    X_val  <- round(best_sol[s, d, 2])
    PR_val <- round(best_sol[s, d, 3], 3)
    cat(sprintf("       %d   | %3d | %3d | %.3f\n", d, J_val, X_val, PR_val))
  }
}

# 9. DETALHES DO LUCRO & HR (verbose=TRUE imprime o resumo O3 automaticamente)
cat("\n=== DETALHES MULTI-OBJETIVO (O3) ===\n")
eval_plan_O3(sann_result$par, forecasts = forecasts, week_id = week_id, 
             max_sales_units = max_sales_units, weight_hr = weight_hr, verbose = TRUE)

# 10. GUARDAR RESULTADOS
saveRDS(sann_result, file = paste0("resultado_SANN_O3_semana_", week_id, ".rds"))
cat("\nResultado guardado em: resultado_SANN_O3_semana_", week_id, ".rds\n")
cat("\n=== FIM ===\n")