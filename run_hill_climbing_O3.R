################################################################################
# run_hill_climbing_O3.R
# Hill Climbing para O3 (maximizar profit - weight_hr * HR) com restrição ≤10000 units
# Inclui solução inicial heurística, limites dinâmicos e reparação
################################################################################

rm(list = ls())
source("hill.R")
source("eval_plan_O3.R")   # contém eval_plan_O3 e repair_solution_inplace

# Carregar previsões
forecasts <- read.csv("forecasts_var_final.csv")
forecasts$Week_Start <- as.Date(forecasts$Week_Start)
week_id <- 20
max_sales_units <- 10000
weight_hr <- 10            # peso para minimizar HR (ajustável)

# ------------------------------------------------------------
# 1. Limites dinâmicos (baseados nas previsões)
# ------------------------------------------------------------
store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")
max_J <- numeric(28)
max_X <- numeric(28)
idx <- 1
for(s in 1:4) {
  fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
  for(d in 1:7) {
    C_pred <- fc_store$Forecast[d]
    max_J[idx] <- min(ceiling(C_pred * 1.5 / 6), 40)
    max_X[idx] <- min(ceiling(C_pred * 1.5 / 7), 25)
    idx <- idx + 1
  }
}
max_PR <- rep(0.30, 28)
lower <- rep(0, 84)
upper <- c(max_J, max_X, max_PR)

cat("Limites dinâmicos para O3 (exemplo Baltimore):\n")
fc_balt <- forecasts[forecasts$Store == "baltimore" & forecasts$Week_ID == week_id, ]
for(d in 1:5) cat(sprintf("Dia %d: C_pred=%3.0f -> J_max=%2d, X_max=%2d\n", 
                          d, fc_balt$Forecast[d], max_J[d], max_X[d]))

# ------------------------------------------------------------
# 2. Solução inicial heurística (maximiza atendimento, sem excesso)
# ------------------------------------------------------------
s0 <- rep(0, 84)
dim(s0) <- c(4,7,3)
for(s in 1:4) {
  fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
  for(d in 1:7) {
    C_pred <- fc_store$Forecast[d]
    X_heur <- floor(C_pred / 7)
    rest <- C_pred - 7*X_heur
    J_heur <- ceiling(rest / 6)
    if(J_heur < 0) J_heur <- 0
    X_heur <- min(X_heur, max_X[(s-1)*7 + d])
    J_heur <- min(J_heur, max_J[(s-1)*7 + d])
    s0[s, d, 1] <- J_heur
    s0[s, d, 2] <- X_heur
    s0[s, d, 3] <- 0.15   # promoção média
  }
}
s0 <- c(s0)
s0 <- repair_solution_inplace(s0, forecasts, week_id, max_sales_units)

# Avaliar valor da solução inicial (para referência)
init_obj <- eval_plan_O3(s0, forecasts, week_id, max_sales_units, weight_hr, verbose = FALSE)
cat(sprintf("\nValor inicial da função objetivo (profit - %.1f*HR): %.2f\n", weight_hr, init_obj))

# ------------------------------------------------------------
# 3. Função de mudança mista (exploração)
# ------------------------------------------------------------
change_fn_mixed <- function(par, lower, upper) {
  new_par <- par
  for(i in 1:length(par)) {
    if(runif(1) < 0.3) {
      if(par[i] == 0) {
        new_par[i] <- runif(1, 0, upper[i]*0.2)
      } else {
        factor <- rnorm(1, mean = 1, sd = 0.1)
        new_par[i] <- par[i] * factor
      }
      new_par[i] <- min(upper[i], max(lower[i], new_par[i]))
    }
  }
  return(new_par)
}

# ------------------------------------------------------------
# 4. Função de avaliação (fechamento)
# ------------------------------------------------------------
eval_fn <- function(sol) {
  eval_plan_O3(sol, forecasts, week_id, max_sales_units, weight_hr, verbose = FALSE)
}

# ------------------------------------------------------------
# 5. Executar Hill Climbing
# ------------------------------------------------------------
cat("\n=== Hill Climbing para O3 (multiobjetivo) ===\n")
cat(sprintf("Objetivo: Maximizar (profit - %.1f * total_HR)\n", weight_hr))
cat(sprintf("Restrição: unidades totais ≤ %d\n", max_sales_units))
cat("Limites dinâmicos ativos\n\n")

set.seed(123)
hc <- hclimbing(par = s0, fn = eval_fn, change = change_fn_mixed,
                lower = lower, upper = upper, type = "max",
                control = list(maxit = 8000, REPORT = 1000, digits = 2))

# ------------------------------------------------------------
# 6. Resultados
# ------------------------------------------------------------
cat("\n=== MELHOR SOLUÇÃO ENCONTRADA ===\n")
cat(sprintf("Valor da função objetivo (profit - %.1f*HR): %.2f\n", weight_hr, hc$eval))

# Avaliar detalhadamente a melhor solução
cat("\n--- Detalhes da melhor solução ---\n")
eval_plan_O3(hc$sol, forecasts, week_id, max_sales_units, weight_hr, verbose = TRUE)

# Mostrar plano por loja/dia
best_sol <- hc$sol
dim(best_sol) <- c(4,7,3)
store_names_print <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
for(s in 1:4) {
  cat("\n", store_names_print[s], ":\n")
  cat(" Dia |  J  |  X  |   PR  \n")
  cat("-----|-----|-----|-------\n")
  for(d in 1:7) {
    Jv <- round(best_sol[s,d,1])
    Xv <- round(best_sol[s,d,2])
    PRv <- round(best_sol[s,d,3], 3)
    cat(sprintf("  %2d | %3d | %3d | %.3f\n", d, Jv, Xv, PRv))
  }
}

# Guardar resultados
saveRDS(hc, file = paste0("resultado_O3_semana_", week_id, "_peso_", weight_hr, ".rds"))
cat("\nResultado guardado.\n")