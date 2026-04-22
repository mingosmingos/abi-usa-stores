################################################################################
# eval_plan_O3.R
# Função de avaliação para o objetivo O3 (multiobjetivo)
# Maximizar lucro e minimizar HR: objective = profit - weight_hr * total_HR
# Com restrição rígida de unidades vendidas ≤ max_sales_units
# Inclui reparação automática de soluções inválidas
################################################################################

# Função auxiliar de reparação (idêntica à usada no O2)
repair_solution_inplace <- function(sol, forecasts, week_id, max_sales_units) {
  dim(sol) <- c(4, 7, 3)
  
  store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
  
  calc_units_day <- function(s, d, J, X, PR, C_pred) {
    max_assisted <- 7 * X + 6 * J
    A <- min(max_assisted, C_pred)
    n_X <- min(X * 7, A)
    n_J <- A - n_X
    units <- 0
    if(n_X > 0) {
      U_X <- round(F_X[s] * 10 / log(2 - PR))
      units <- units + n_X * U_X
    }
    if(n_J > 0) {
      U_J <- round(F_J[s] * 10 / log(2 - PR))
      units <- units + n_J * U_J
    }
    return(units)
  }
  
  total_units <- 0
  units_by_day <- matrix(0, 4, 7)
  for(s in 1:4) {
    store <- store_names[s]
    fc_store <- forecasts[forecasts$Store == store & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      J <- max(0, round(sol[s, d, 1]))
      X <- max(0, round(sol[s, d, 2]))
      PR <- min(0.30, max(0, sol[s, d, 3]))
      C_pred <- fc_store$Forecast[fc_store$Day == d]
      units <- calc_units_day(s, d, J, X, PR, C_pred)
      units_by_day[s, d] <- units
      total_units <- total_units + units
    }
  }
  
  if(total_units <= max_sales_units) return(c(sol))
  
  while(total_units > max_sales_units) {
    max_units <- max(units_by_day)
    if(max_units == 0) break
    idx <- which(units_by_day == max_units, arr.ind = TRUE)[1,]
    s <- idx[1]; d <- idx[2]
    store <- store_names[s]
    fc_store <- forecasts[forecasts$Store == store & forecasts$Week_ID == week_id, ]
    C_pred <- fc_store$Forecast[fc_store$Day == d]
    
    J <- max(0, round(sol[s, d, 1]))
    X <- max(0, round(sol[s, d, 2]))
    PR <- min(0.30, max(0, sol[s, d, 3]))
    
    if(X > 0) {
      X <- X - 1
    } else if(J > 0) {
      J <- J - 1
    } else {
      PR <- max(0, PR - 0.05)
    }
    
    sol[s, d, 1] <- J
    sol[s, d, 2] <- X
    sol[s, d, 3] <- PR
    
    new_units <- calc_units_day(s, d, J, X, PR, C_pred)
    total_units <- total_units - units_by_day[s, d] + new_units
    units_by_day[s, d] <- new_units
  }
  return(c(sol))
}

# Função principal de avaliação para O3
eval_plan_O3 <- function(sol, forecasts, week_id, max_sales_units = 10000, 
                         weight_hr = 10, verbose = FALSE) {
  
  # Reparar solução (garante restrição de unidades)
  sol_repaired <- repair_solution_inplace(sol, forecasts, week_id, max_sales_units)
  dim(sol_repaired) <- c(4, 7, 3)
  
  store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")
  W_s <- c(700, 730, 760, 800)
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
  
  hr_cost_J_weekday <- 60
  hr_cost_J_weekend <- 70
  hr_cost_X_weekday <- 80
  hr_cost_X_weekend <- 95
  
  total_profit <- 0
  total_sales_units <- 0
  total_hr <- 0
  
  for(s in 1:4) {
    store <- store_names[s]
    fc_store <- forecasts[forecasts$Store == store & forecasts$Week_ID == week_id, ]
    if(nrow(fc_store) != 7) stop(paste("Previsões incompletas para", store))
    
    weekly_profit <- 0
    
    for(d in 1:7) {
      J <- max(0, round(sol_repaired[s, d, 1]))
      X <- max(0, round(sol_repaired[s, d, 2]))
      PR <- min(0.30, max(0, sol_repaired[s, d, 3]))
      
      C_pred <- fc_store$Forecast[fc_store$Day == d]
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      
      is_weekend <- (d %in% c(6,7))
      cost_J <- ifelse(is_weekend, hr_cost_J_weekend, hr_cost_J_weekday)
      cost_X <- ifelse(is_weekend, hr_cost_X_weekend, hr_cost_X_weekday)
      daily_cost_hr <- J * cost_J + X * cost_X
      
      n_assisted_by_X <- min(X * 7, A)
      n_assisted_by_J <- A - n_assisted_by_X
      
      daily_sales_units <- 0
      daily_revenue <- 0
      
      if(n_assisted_by_X > 0) {
        U_X <- round(F_X[s] * 10 / log(2 - PR))
        P_X <- round(U_X * (1 - PR) * 1.07)
        daily_sales_units <- daily_sales_units + n_assisted_by_X * U_X
        daily_revenue <- daily_revenue + n_assisted_by_X * P_X
      }
      if(n_assisted_by_J > 0) {
        U_J <- round(F_J[s] * 10 / log(2 - PR))
        P_J <- round(U_J * (1 - PR) * 1.07)
        daily_sales_units <- daily_sales_units + n_assisted_by_J * U_J
        daily_revenue <- daily_revenue + n_assisted_by_J * P_J
      }
      
      daily_net_profit <- daily_revenue - daily_cost_hr
      weekly_profit <- weekly_profit + daily_net_profit
      total_sales_units <- total_sales_units + daily_sales_units
      total_hr <- total_hr + J + X
      
      if(verbose) {
        cat(sprintf("%s dia %d: J=%d X=%d PR=%.3f pred=%.0f A=%d units=%d rev=%.0f cost=%.0f profit=%.0f\n",
                    store, d, J, X, PR, C_pred, A, daily_sales_units, daily_revenue, daily_cost_hr, daily_net_profit))
      }
    }
    total_profit <- total_profit + weekly_profit - W_s[s]
  }
  
  if(verbose) {
    cat("\n=== RESUMO O3 ===\n")
    cat("Total sales units:", total_sales_units, "/", max_sales_units, "\n")
    cat("Total HR (J+X):", total_hr, "\n")
    cat("Total profit: $", round(total_profit, 2), "\n")
    cat(sprintf("Função objetivo (profit - %.1f * HR) = %.2f\n", weight_hr, total_profit - weight_hr * total_hr))
  }
  
  # Garantia final (não deve acontecer, mas por segurança)
  if(total_sales_units > max_sales_units) return(-1e9)
  
  # Multiobjetivo: quanto maior weight_hr, mais se penaliza o uso de HR
  objective_value <- total_profit - weight_hr * total_hr
  return(objective_value)
}