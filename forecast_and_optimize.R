################################################################################
# forecast_and_optimize.R
# 
# 1. Lê os CSVs originais de cada loja
# 2. Faz previsão semanal (Seasonal Naive) para Num_Customers
# 3. Otimiza planos semanais com Hill Climbing (O1, O2, O3)
################################################################################

rm(list = ls())

# Carregar função hill climbing
source("hill.R")

# ------------------------------------------------------------
# 1. FUNÇÕES DE PREVISÃO
# ------------------------------------------------------------

# Lê um CSV e retorna a série temporal de Num_Customers (primeiras 714 observações)
load_store_data <- function(file_path) {
  df <- read.csv(file_path)
  # Assumindo que a coluna se chama Num_Customers
  if(!"Num_Customers" %in% colnames(df)) stop("Coluna Num_Customers não encontrada em ", file_path)
  return(df$Num_Customers)
}

# Previsão Seasonal Naive: repete a última semana (7 dias) para os próximos 7 dias
forecast_seasonal_naive <- function(series, n_ahead = 7) {
  L <- length(series)
  if(L < n_ahead) stop("Série muito curta")
  last_week <- series[(L - n_ahead + 1):L]
  return(last_week)   # repete exatamente os valores da última semana
}

# Previsão simples: média móvel dos últimos 7 dias (alternativa robusta)
forecast_ma7 <- function(series, n_ahead = 7) {
  L <- length(series)
  if(L < 7) stop("Série muito curta")
  ma <- mean(series[(L-6):L])   # média da última semana
  return(rep(ma, n_ahead))
}

# ------------------------------------------------------------
# 2. FUNÇÕES DE AVALIAÇÃO (O1, O2, O3) - adaptadas para usar previsões
# ------------------------------------------------------------

# Recebe: sol (vetor 84), forecasts (lista com 4 vectores de 7 previsões), week_id (só para identificar)
# Mas aqui as previsões já são os valores reais previstos.

eval_plan_O1 <- function(sol, forecasts_list, store_names, verbose = FALSE) {
  # forecasts_list: lista de 4 vectores (cada um com 7 previsões diárias)
  dim(sol) <- c(4, 7, 3)
  
  W_s <- c(700, 730, 760, 800)
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
  
  hr_cost_J_weekday <- 60
  hr_cost_J_weekend <- 70
  hr_cost_X_weekday <- 80
  hr_cost_X_weekend <- 95
  
  total_profit <- 0
  
  for(s in 1:4) {
    fc_store <- forecasts_list[[s]]
    weekly_profit <- 0
    
    for(d in 1:7) {
      J <- max(0, round(sol[s, d, 1]))
      X <- max(0, round(sol[s, d, 2]))
      PR <- min(0.30, max(0, sol[s, d, 3]))
      C_pred <- fc_store[d]
      
      max_assisted <- 7*X + 6*J
      A <- min(max_assisted, C_pred)
      
      is_weekend <- (d %in% c(6,7))
      cost_J <- ifelse(is_weekend, hr_cost_J_weekend, hr_cost_J_weekday)
      cost_X <- ifelse(is_weekend, hr_cost_X_weekend, hr_cost_X_weekday)
      daily_cost_hr <- J*cost_J + X*cost_X
      
      n_X <- min(X*7, A)
      n_J <- A - n_X
      
      daily_units <- 0
      daily_rev <- 0
      if(n_X > 0) {
        U_X <- round(F_X[s] * 10 / log(2 - PR))
        P_X <- round(U_X * (1 - PR) * 1.07)
        daily_units <- daily_units + n_X * U_X
        daily_rev <- daily_rev + n_X * P_X
      }
      if(n_J > 0) {
        U_J <- round(F_J[s] * 10 / log(2 - PR))
        P_J <- round(U_J * (1 - PR) * 1.07)
        daily_units <- daily_units + n_J * U_J
        daily_rev <- daily_rev + n_J * P_J
      }
      
      daily_profit <- daily_rev - daily_cost_hr
      weekly_profit <- weekly_profit + daily_profit
      
      if(verbose) {
        cat(sprintf("%s dia %d: J=%d X=%d PR=%.3f pred=%.0f A=%d units=%d rev=%.0f cost=%.0f profit=%.0f\n",
                    store_names[s], d, J, X, PR, C_pred, A, daily_units, daily_rev, daily_cost_hr, daily_profit))
      }
    }
    total_profit <- total_profit + weekly_profit - W_s[s]
  }
  if(verbose) cat("Lucro total O1: $", round(total_profit,2), "\n")
  return(total_profit)
}

# Função de reparação (idêntica à usada anteriormente, mas adaptada para forecasts_list)
repair_solution <- function(sol, forecasts_list, max_sales_units = 10000) {
  dim(sol) <- c(4,7,3)
  store_names <- c("baltimore","lancaster","philadelphia","richmond")
  F_J <- c(1.00,1.05,1.10,1.15)
  F_X <- c(1.15,1.20,1.15,1.25)
  
  calc_units_day <- function(s, d, J, X, PR, C_pred) {
    max_assisted <- 7*X + 6*J
    A <- min(max_assisted, C_pred)
    n_X <- min(X*7, A)
    n_J <- A - n_X
    units <- 0
    if(n_X > 0) units <- units + n_X * round(F_X[s]*10/log(2-PR))
    if(n_J > 0) units <- units + n_J * round(F_J[s]*10/log(2-PR))
    return(units)
  }
  
  total_units <- 0
  units_by_day <- matrix(0,4,7)
  for(s in 1:4) {
    fc <- forecasts_list[[s]]
    for(d in 1:7) {
      J <- max(0, round(sol[s,d,1]))
      X <- max(0, round(sol[s,d,2]))
      PR <- min(0.30, max(0, sol[s,d,3]))
      u <- calc_units_day(s, d, J, X, PR, fc[d])
      units_by_day[s,d] <- u
      total_units <- total_units + u
    }
  }
  if(total_units <= max_sales_units) return(c(sol))
  
  while(total_units > max_sales_units) {
    max_pos <- which(units_by_day == max(units_by_day), arr.ind = TRUE)[1,]
    s <- max_pos[1]; d <- max_pos[2]
    fc <- forecasts_list[[s]]
    J <- max(0, round(sol[s,d,1]))
    X <- max(0, round(sol[s,d,2]))
    PR <- min(0.30, max(0, sol[s,d,3]))
    if(X > 0) X <- X-1
    else if(J > 0) J <- J-1
    else PR <- max(0, PR-0.05)
    sol[s,d,1] <- J; sol[s,d,2] <- X; sol[s,d,3] <- PR
    new_u <- calc_units_day(s, d, J, X, PR, fc[d])
    total_units <- total_units - units_by_day[s,d] + new_u
    units_by_day[s,d] <- new_u
  }
  return(c(sol))
}

eval_plan_O2 <- function(sol, forecasts_list, max_sales_units = 10000, store_names, verbose = FALSE) {
  sol_repaired <- repair_solution(sol, forecasts_list, max_sales_units)
  profit <- eval_plan_O1(sol_repaired, forecasts_list, store_names, verbose = FALSE)
  if(verbose) {
    # recalcular unidades para mostrar
    dim(sol_repaired) <- c(4,7,3)
    total_units <- 0
    for(s in 1:4) for(d in 1:7) {
      J <- round(sol_repaired[s,d,1]); X <- round(sol_repaired[s,d,2]); PR <- sol_repaired[s,d,3]
      C_pred <- forecasts_list[[s]][d]
      max_assisted <- 7*X+6*J; A <- min(max_assisted, C_pred)
      n_X <- min(X*7, A); n_J <- A - n_X
      if(n_X>0) total_units <- total_units + n_X * round(c(1.15,1.20,1.15,1.25)[s]*10/log(2-PR))
      if(n_J>0) total_units <- total_units + n_J * round(c(1.00,1.05,1.10,1.15)[s]*10/log(2-PR))
    }
    cat("O2 -> Lucro:", round(profit,2), " Unidades:", total_units, "/", max_sales_units, "\n")
  }
  return(profit)
}

eval_plan_O3 <- function(sol, forecasts_list, max_sales_units = 10000, weight_hr = 10, store_names, verbose = FALSE) {
  sol_repaired <- repair_solution(sol, forecasts_list, max_sales_units)
  profit <- eval_plan_O1(sol_repaired, forecasts_list, store_names, verbose = FALSE)
  # Calcular total HR
  dim(sol_repaired) <- c(4,7,3)
  total_hr <- sum(round(sol_repaired[,,1]) + round(sol_repaired[,,2]))
  obj <- profit - weight_hr * total_hr
  if(verbose) {
    cat(sprintf("O3 -> profit: %.2f, HR: %d, objetivo: %.2f\n", profit, total_hr, obj))
  }
  return(obj)
}

# ------------------------------------------------------------
# 3. PREVISÕES PARA CADA LOJA (última semana disponível)
# ------------------------------------------------------------
cat("=== A carregar dados originais e gerar previsões (Seasonal Naive) ===\n")
file_paths <- c("baltimore.csv", "lancaster.csv", "philadelphia.csv", "richmond.csv")
store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")

all_forecasts <- list()   # cada elemento é um vector de 7 previsões
for(i in 1:4) {
  series <- load_store_data(file_paths[i])
  # Usar Seasonal Naive (última semana)
  fc <- forecast_seasonal_naive(series, 7)
  all_forecasts[[i]] <- fc
  cat(store_names[i], " previsões para os próximos 7 dias:\n")
  print(round(fc))
}

# ------------------------------------------------------------
# 4. OTIMIZAÇÃO PARA O1, O2, O3 (Hill Climbing)
# ------------------------------------------------------------

# Limites dinâmicos com base nas previsões
max_J <- numeric(28)
max_X <- numeric(28)
idx <- 1
for(s in 1:4) {
  for(d in 1:7) {
    C_pred <- all_forecasts[[s]][d]
    max_J[idx] <- min(ceiling(C_pred * 1.5 / 6), 40)
    max_X[idx] <- min(ceiling(C_pred * 1.5 / 7), 25)
    idx <- idx + 1
  }
}
max_PR <- rep(0.30, 28)
lower <- rep(0, 84)
upper <- c(max_J, max_X, max_PR)

# Solução inicial heurística
s0 <- rep(0, 84)
dim(s0) <- c(4,7,3)
for(s in 1:4) {
  for(d in 1:7) {
    C <- all_forecasts[[s]][d]
    X_heur <- floor(C / 7)
    rest <- C - 7*X_heur
    J_heur <- ceiling(rest / 6)
    if(J_heur < 0) J_heur <- 0
    idx_var <- (s-1)*7 + d
    s0[s,d,1] <- min(J_heur, max_J[idx_var])
    s0[s,d,2] <- min(X_heur, max_X[idx_var])
    s0[s,d,3] <- 0.15
  }
}
s0 <- repair_solution(c(s0), all_forecasts, max_sales_units = 10000)

change_fn <- function(par, lower, upper) {
  new_par <- par
  for(i in 1:84) {
    if(runif(1) < 0.3) {
      if(par[i] == 0) new_par[i] <- runif(1, 0, upper[i]*0.2)
      else new_par[i] <- par[i] * rnorm(1, mean=1, sd=0.1)
      new_par[i] <- min(upper[i], max(lower[i], new_par[i]))
    }
  }
  return(new_par)
}

# Função helper para executar HC
run_hc <- function(obj_name, eval_fun, ...) {
  cat("\n========================================\n")
  cat("Executando Hill Climbing para", obj_name, "\n")
  cat("========================================\n")
  eval_fn <- function(sol) eval_fun(sol, ...)
  set.seed(123)
  hclimbing(par = s0, fn = eval_fn, change = change_fn,
            lower = lower, upper = upper, type = "max",
            control = list(maxit = 8000, REPORT = 2000, digits = 2))
}

# O1
hc1 <- run_hc("O1", eval_plan_O1, forecasts_list = all_forecasts, store_names = store_names)
cat("\n>>> O1 Melhor lucro: $", round(hc1$eval,2), "\n")

# O2
hc2 <- run_hc("O2", eval_plan_O2, forecasts_list = all_forecasts, max_sales_units = 10000, store_names = store_names)
cat("\n>>> O2 Melhor lucro (com restrição): $", round(hc2$eval,2), "\n")

# O3 (peso 5, ajustável)
weight_hr <- 5
hc3 <- run_hc("O3 (peso=5)", eval_plan_O3, forecasts_list = all_forecasts, max_sales_units = 10000, weight_hr = weight_hr, store_names = store_names)
cat("\n>>> O3 Melhor objetivo (profit - 5*HR):", round(hc3$eval,2), "\n")

# Mostrar plano final para O2 (exemplo)
best_sol <- hc2$sol
dim(best_sol) <- c(4,7,3)
cat("\n===== PLANO ÓTIMO PARA O2 (lucro com restrição 10000) =====\n")
for(s in 1:4) {
  cat("\n", toupper(store_names[s]), "\n")
  cat(" Dia |  J  |  X  |   PR\n")
  cat("-----|-----|-----|------\n")
  for(d in 1:7) {
    cat(sprintf("  %2d | %3d | %3d | %.3f\n", d, round(best_sol[s,d,1]), round(best_sol[s,d,2]), best_sol[s,d,3]))
  }
}

cat("\n--- Verificação final O2 (detalhado) ---\n")
eval_plan_O2(hc2$sol, all_forecasts, max_sales_units = 10000, store_names, verbose = TRUE)

cat("\n=== FIM ===\n")