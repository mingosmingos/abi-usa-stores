################################################################################
# run_hill_climbing_O3.R
# Hill Climbing para O3: fronteira de Pareto entre HR e Lucro
# Corre o hill climbing para vários valores de alpha (peso do lucro)
# e produz um gráfico de Pareto HR vs Lucro
# Restrição: unidades vendidas <= 10000
################################################################################

rm(list = ls())
source("hill.R")
source("eval_plan_O3.R")

# Carregar previsões
raw <- read.csv("all_store_predictions.csv")

forecasts <- data.frame(
  Store    = raw$Store,
  Week_ID  = raw$Run,
  Day      = raw$Step,
  Forecast = raw$Num_Customers
)
week_id        <- 20
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
lower  <- rep(0, 84)
upper  <- c(max_J, max_X, max_PR)

cat("Limites dinâmicos para O3 (exemplo Baltimore):\n")
fc_balt <- forecasts[forecasts$Store == "baltimore" & forecasts$Week_ID == week_id, ]
for(d in 1:5) cat(sprintf("Dia %d: C_pred=%3.0f -> J_max=%2d, X_max=%2d\n",
                          d, fc_balt$Forecast[d], max_J[d], max_X[d]))

# ------------------------------------------------------------
# 2. Solução inicial heurística
# ------------------------------------------------------------
build_s0 <- function() {
  s0 <- array(0, dim = c(4, 7, 3))
  for(s in 1:4) {
    fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      C_pred <- fc_store$Forecast[d]
      X_heur <- floor(C_pred / 7)
      rest   <- C_pred - 7 * X_heur
      J_heur <- max(0, ceiling(rest / 6))
      X_heur <- min(X_heur, max_X[(s-1)*7 + d])
      J_heur <- min(J_heur, max_J[(s-1)*7 + d])
      s0[s, d, 1] <- J_heur
      s0[s, d, 2] <- X_heur
      s0[s, d, 3] <- 0.15
    }
  }
  s0 <- repair_solution_inplace(c(s0), forecasts, week_id, max_sales_units, max_J, max_X)
  return(s0)
}

# ------------------------------------------------------------
# 3. Função de mudança mista
# ------------------------------------------------------------
change_fn_mixed <- function(par, lower, upper) {
  new_par <- par
  for(i in 1:length(par)) {
    if(runif(1) < 0.3) {
      if(par[i] == 0) {
        new_par[i] <- runif(1, 0, upper[i] * 0.2)
      } else {
        new_par[i] <- par[i] * rnorm(1, mean = 1, sd = 0.1)
      }
      new_par[i] <- min(upper[i], max(lower[i], new_par[i]))
    }
  }
  return(new_par)
}

# ------------------------------------------------------------
# 4. Função auxiliar: extrair HR e lucro de uma solução
# ------------------------------------------------------------
get_hr_profit <- function(sol) {
  eval_plan_O3(sol, forecasts, week_id, max_sales_units,
               verbose = FALSE, max_J = max_J, max_X = max_X,
               return_components = TRUE)
}

# ------------------------------------------------------------
# 5. Correr hill climbing para vários alphas -> fronteira de Pareto
# ------------------------------------------------------------
# Alpha = peso do lucro na fórmula: HR - profit * alpha
# Alpha pequeno  -> foca em minimizar HR (menos staff, menos lucro)
# Alpha grande   -> foca em maximizar lucro (mais staff, mais lucro)
alphas <- c(0.001, 0.005, 0.01, 0.02, 0.05, 0.08, 0.1, 0.15, 0.2, 0.3, 0.5, 1.0)

pareto_results <- data.frame(alpha = numeric(), HR = numeric(), Profit = numeric())

cat("\n=== Fronteira de Pareto O3: a correr", length(alphas), "otimizações ===\n")

for(alpha in alphas) {
  cat(sprintf("  alpha = %.3f ... ", alpha))
  
  eval_fn_alpha <- function(sol) {
    res <- eval_plan_O3(sol, forecasts, week_id, max_sales_units,
                        verbose = FALSE, max_J = max_J, max_X = max_X,
                        alpha = alpha)
    return(res)
  }
  
  set.seed(123)
  s0 <- build_s0()
  hc <- hclimbing(par = s0, fn = eval_fn_alpha, change = change_fn_mixed,
                  lower = lower, upper = upper, type = "max",
                  control = list(maxit = 8000, REPORT = 0, digits = 2))
  
  comps <- get_hr_profit(hc$sol)
  pareto_results <- rbind(pareto_results,
                          data.frame(alpha  = alpha,
                                     HR     = comps$total_hr,
                                     Profit = comps$total_profit))
  cat(sprintf("HR = %3d, Lucro = %6.0f\n", comps$total_hr, comps$total_profit))
}

# ------------------------------------------------------------
# 6. Filtrar pontos dominados (fronteira de Pareto real)
# ------------------------------------------------------------
# Um ponto é não-dominado se nenhum outro tem HR <= e Lucro >= (com pelo menos um estrito)
is_nondominated <- function(df) {
  n <- nrow(df)
  dominated <- logical(n)
  for(i in 1:n) {
    for(j in 1:n) {
      if(i != j) {
        if(df$HR[j] <= df$HR[i] && df$Profit[j] >= df$Profit[i] &&
           (df$HR[j] < df$HR[i] || df$Profit[j] > df$Profit[i])) {
          dominated[i] <- TRUE
          break
        }
      }
    }
  }
  return(!dominated)
}

pareto_front <- pareto_results[is_nondominated(pareto_results), ]
pareto_front <- pareto_front[order(pareto_front$HR), ]

cat("\n=== Fronteira de Pareto (pontos não-dominados) ===\n")
cat(sprintf("%-8s  %6s  %8s\n", "Alpha", "HR", "Lucro"))
cat(strrep("-", 28), "\n")
for(i in 1:nrow(pareto_front)) {
  cat(sprintf("%-8.3f  %6d  %8.0f\n",
              pareto_front$alpha[i], pareto_front$HR[i], pareto_front$Profit[i]))
}

# ------------------------------------------------------------
# 7. Gráfico de Pareto
# ------------------------------------------------------------
png("pareto_O3.png", width = 800, height = 600, res = 120)

plot(pareto_results$HR, pareto_results$Profit,
     pch = 16, col = "grey70", cex = 1.0,
     xlab = "Total HR (Juniors + Experts)",
     ylab = "Lucro Total ($)",
     main = sprintf("Fronteira de Pareto O3 - Semana %d\nMinimizar HR vs Maximizar Lucro", week_id),
     xlim = range(pareto_results$HR) + c(-2, 2),
     ylim = range(pareto_results$Profit) + c(-50, 50))

# Linha e pontos da fronteira de Pareto
lines(pareto_front$HR, pareto_front$Profit, col = "steelblue", lwd = 2, type = "b",
      pch = 17, cex = 1.4)

# Destacar a solução com alpha = 0.1 (solução principal)
sol_01 <- pareto_results[pareto_results$alpha == 0.1, ]
if(nrow(sol_01) > 0) {
  points(sol_01$HR, sol_01$Profit, pch = 8, col = "red", cex = 2, lwd = 2)
  text(sol_01$HR, sol_01$Profit, labels = "alpha=0.1",
       pos = 3, col = "red", cex = 0.8)
}

legend("bottomright",
       legend = c("Todas as soluções", "Fronteira de Pareto", "Solução principal (alpha=0.1)"),
       col    = c("grey70", "steelblue", "red"),
       pch    = c(16, 17, 8),
       lty    = c(NA, 1, NA),
       lwd    = c(NA, 2, 2),
       cex    = 0.85,
       bg     = "white")

grid(col = "grey90")
dev.off()

cat("\nGráfico guardado em 'pareto_O3.png'\n")

# ------------------------------------------------------------
# 8. Melhor solução com alpha = 0.1 (detalhada)
# ------------------------------------------------------------
cat("\n=== DETALHE: Melhor solução com alpha = 0.1 ===\n")

eval_fn_01 <- function(sol) {
  eval_plan_O3(sol, forecasts, week_id, max_sales_units,
               verbose = FALSE, max_J = max_J, max_X = max_X, alpha = 0.1)
}
set.seed(123)
s0 <- build_s0()
hc_best <- hclimbing(par = s0, fn = eval_fn_01, change = change_fn_mixed,
                     lower = lower, upper = upper, type = "max",
                     control = list(maxit = 8000, REPORT = 1000, digits = 2))

eval_plan_O3(hc_best$sol, forecasts, week_id, max_sales_units,
             verbose = TRUE, max_J = max_J, max_X = max_X, alpha = 0.1)

best_sol <- hc_best$sol
dim(best_sol) <- c(4, 7, 3)
store_names_print <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")
for(s in 1:4) {
  cat("\n", store_names_print[s], ":\n")
  cat(" Dia |  J  |  X  |   PR  \n")
  cat("-----|-----|-----|-------\n")
  for(d in 1:7) {
    Jv  <- min(max(0, round(best_sol[s,d,1])), max_J[(s-1)*7 + d])
    Xv  <- min(max(0, round(best_sol[s,d,2])), max_X[(s-1)*7 + d])
    PRv <- round(best_sol[s,d,3], 3)
    cat(sprintf("  %2d | %3d | %3d | %.3f\n", d, Jv, Xv, PRv))
  }
}

saveRDS(list(pareto = pareto_results, pareto_front = pareto_front, best = hc_best),
        file = paste0("resultado_O3_semana_", week_id, ".rds"))
cat("\nResultado guardado.\n")