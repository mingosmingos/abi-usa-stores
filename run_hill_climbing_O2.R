################################################################################
# run_hill_climbing_O2_melhorado.R
# Hill Climbing para O2 com solução inicial heurística e melhor exploração
################################################################################

rm(list = ls())
source("hill.R")
source("eval_plan_O2.R")  # já contém repair_solution_inplace e eval_plan_O2

forecasts <- read.csv("forecasts_var_final.csv")
forecasts$Week_Start <- as.Date(forecasts$Week_Start)
week_id <- 20
max_sales_units <- 10000

# ----------------------------------------------------------------------
# 1. Cálculo de limites superiores (mais generosos, mas ainda realistas)
# ----------------------------------------------------------------------
store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")
max_J_dynamic <- numeric(28)
max_X_dynamic <- numeric(28)

idx <- 1
for(s in 1:4) {
  fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
  for(d in 1:7) {
    C_pred <- fc_store$Forecast[fc_store$Day == d]
    # Limites mais folgados: máximo de HR que conseguem atender 150% dos clientes previstos
    max_J_dynamic[idx] <- ceiling(C_pred * 1.5 / 6)
    max_X_dynamic[idx] <- ceiling(C_pred * 1.5 / 7)
    idx <- idx + 1
  }
}

max_J <- pmin(max_J_dynamic, 40)  # teto absoluto
max_X <- pmin(max_X_dynamic, 25)
max_PR <- rep(0.30, 28)

lower <- rep(0, 84)
upper <- c(max_J, max_X, max_PR)

cat("Limites dinâmicos (exemplo Baltimore, primeiros 5 dias):\n")
fc_balt <- forecasts[forecasts$Store == "baltimore" & forecasts$Week_ID == week_id, ]
for(d in 1:5) cat(sprintf("Dia %d: C_pred=%3.0f -> J_max=%2d, X_max=%2d\n", 
                          d, fc_balt$Forecast[d], max_J[d], max_X[d]))

# ----------------------------------------------------------------------
# 2. Solução inicial heurística (inteligente) - já dá lucro positivo
# ----------------------------------------------------------------------
s0 <- rep(0, 84)
dim(s0) <- c(4,7,3)

for(s in 1:4) {
  store <- store_names[s]
  fc_store <- forecasts[forecasts$Store == store & forecasts$Week_ID == week_id, ]
  for(d in 1:7) {
    C_pred <- fc_store$Forecast[d]
    # Heurística: usar X para atender a maioria, J para o resto
    X_heur <- floor(C_pred / 7)
    rest <- C_pred - 7*X_heur
    J_heur <- ceiling(rest / 6)
    if(J_heur < 0) J_heur <- 0
    # Evitar excesso de HR (já que temos restrição de unidades)
    X_heur <- min(X_heur, max_X[(s-1)*7 + d])
    J_heur <- min(J_heur, max_J[(s-1)*7 + d])
    s0[s, d, 1] <- J_heur
    s0[s, d, 2] <- X_heur
    s0[s, d, 3] <- 0.15   # promoção média
  }
}
s0 <- c(s0)  # voltar a vetor

# Reparar a solução inicial para garantir que cumpre a restrição (se necessário)
s0 <- repair_solution_inplace(s0, forecasts, week_id, max_sales_units)

# Avaliar lucro da solução inicial
profit_init <- eval_plan_O2(s0, forecasts, week_id, max_sales_units, verbose = FALSE)
cat(sprintf("\nLucro da solução inicial (heurística): $%.2f\n", profit_init))

# ----------------------------------------------------------------------
# 3. Função de mudança mais agressiva (exploração)
# ----------------------------------------------------------------------
change_fn_explore <- function(par, lower, upper) {
  new_par <- par
  for(i in 1:length(par)) {
    if(runif(1) < 0.3) {  # 30% das vezes muda
      if(par[i] == 0) {
        new_par[i] <- runif(1, 0, upper[i]*0.2)
      } else {
        # Perturbação multiplicativa com sd maior (0.1) para saltos maiores
        factor <- rnorm(1, mean = 1, sd = 0.1)
        new_par[i] <- par[i] * factor
      }
      new_par[i] <- min(upper[i], max(lower[i], new_par[i]))
    }
  }
  return(new_par)
}

# ----------------------------------------------------------------------
# 4. Executar Hill Climbing com muitas iterações
# ----------------------------------------------------------------------
eval_fn <- function(sol) {
  eval_plan_O2(sol, forecasts, week_id, max_sales_units, verbose = FALSE)
}

cat("\n=== Hill Climbing para O2 (solução heurística + exploração) ===\n")
set.seed(123)
hc <- hclimbing(par = s0, fn = eval_fn, change = change_fn_explore,
                lower = lower, upper = upper, type = "max",
                control = list(maxit = 10000, REPORT = 1000, digits = 2))

# ----------------------------------------------------------------------
# 5. Resultados
# ----------------------------------------------------------------------
cat("\n=== MELHOR SOLUÇÃO ENCONTRADA ===\n")
cat("Lucro total: $", round(hc$eval, 2), "\n\n")

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

cat("\n=== VERIFICAÇÃO FINAL (verbose) ===\n")
eval_plan_O2(hc$sol, forecasts, week_id, max_sales_units, verbose = TRUE)

saveRDS(hc, file = "resultado_O2_melhorado.rds")
cat("\nResultado guardado em resultado_O2_melhorado.rds\n")