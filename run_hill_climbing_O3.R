################################################################################
# run_hill_climbing_O3.R
# Hill Climbing para O3: minimizar HR com lucro como critério secundário
# Restrição: unidades vendidas <= 10000
################################################################################

rm(list = ls())
source("hill.R")
source("eval_plan_O3.R")

# Carregar previsões
raw <- read.csv("all_store_predictions.csv")

# Mapear colunas do CSV para o formato esperado pela eval_plan_O3:
#   Run           -> Week_ID   (semana, 1..104)
#   Step          -> Day       (dia da semana, 1..7)
#   Num_Customers -> Forecast  (previsão de clientes)
#   Store         -> Store     (nome da loja, sem alteração)
forecasts <- data.frame(
  Store    = raw$Store,
  Week_ID  = raw$Run,
  Day      = raw$Step,
  Forecast = raw$Num_Customers
)
week_id <- 20
max_sales_units <- 10000

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
# 2. Solução inicial heurística (mínimo HR viável)
# ------------------------------------------------------------
s0 <- rep(0, 84)
dim(s0) <- c(4,7,3)
for(s in 1:4) {
  fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
  for(d in 1:7) {
    C_pred <- fc_store$Forecast[d]
    # Heurística: mínimo de staff necessário para atender os clientes previstos
    X_heur <- floor(C_pred / 7)
    rest <- C_pred - 7 * X_heur
    J_heur <- ceiling(rest / 6)
    if(J_heur < 0) J_heur <- 0
    X_heur <- min(X_heur, max_X[(s-1)*7 + d])
    J_heur <- min(J_heur, max_J[(s-1)*7 + d])
    s0[s, d, 1] <- J_heur
    s0[s, d, 2] <- X_heur
    s0[s, d, 3] <- 0.15
  }
}
s0 <- c(s0)
s0 <- repair_solution_inplace(s0, forecasts, week_id, max_sales_units, max_J, max_X)

init_obj <- eval_plan_O3(s0, forecasts, week_id, max_sales_units, verbose = FALSE, max_J = max_J, max_X = max_X)
cat(sprintf("\nValor inicial da função objetivo (-(HR - profit*0.1)): %.4f\n", init_obj))

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
  eval_plan_O3(sol, forecasts, week_id, max_sales_units, verbose = FALSE, max_J = max_J, max_X = max_X)
}

# ------------------------------------------------------------
# 5. Executar Hill Climbing
# ------------------------------------------------------------
cat("\n=== Hill Climbing para O3 ===\n")
cat("Objetivo: Minimizar HR (critério principal)\n")
cat("          Maximizar lucro (critério secundário)\n")
cat(sprintf("Restrição: unidades totais <= %d\n", max_sales_units))
cat("Limites dinâmicos ativos\n\n")

set.seed(123)
hc <- hclimbing(par = s0, fn = eval_fn, change = change_fn_mixed,
                lower = lower, upper = upper, type = "max",
                control = list(maxit = 8000, REPORT = 1000, digits = 2))

# ------------------------------------------------------------
# 6. Resultados
# ------------------------------------------------------------
cat("\n=== MELHOR SOLUÇÃO ENCONTRADA ===\n")

# Avaliar detalhadamente a melhor solução
cat("\n--- Detalhes da melhor solução ---\n")
eval_plan_O3(hc$sol, forecasts, week_id, max_sales_units, verbose = TRUE, max_J = max_J, max_X = max_X)

# Mostrar plano por loja/dia (com limites aplicados)
best_sol <- hc$sol
dim(best_sol) <- c(4,7,3)
store_names_print <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
for(s in 1:4) {
  cat("\n", store_names_print[s], ":\n")
  cat(" Dia |  J  |  X  |   PR  \n")
  cat("-----|-----|-----|-------\n")
  for(d in 1:7) {
    Jv <- min(max(0, round(best_sol[s,d,1])), max_J[(s-1)*7 + d])
    Xv <- min(max(0, round(best_sol[s,d,2])), max_X[(s-1)*7 + d])
    PRv <- round(best_sol[s,d,3], 3)
    cat(sprintf("  %2d | %3d | %3d | %.3f\n", d, Jv, Xv, PRv))
  }
}

# Guardar resultados
saveRDS(hc, file = paste0("resultado_O3_semana_", week_id, ".rds"))
cat("\nResultado guardado.\n")