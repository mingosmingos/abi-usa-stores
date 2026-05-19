################################################################################
# eval_plan_O1.R
# Função de avaliação do plano para o objetivo O1 (maximizar lucro)
################################################################################

eval_plan_O1 <- function(sol, forecasts, week_id, verbose = FALSE) {
  
  # Converter vetor para matriz 4 lojas x 7 dias x 3 variáveis (J, X, PR)
  dim(sol) <- c(4, 7, 3)
  
  # Parâmetros por loja (conforme enunciado)
  store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")
  W_s <- c(700, 730, 760, 800)           # custo fixo semanal por loja
  F_J <- c(1.00, 1.05, 1.10, 1.15)       # fator de ajuda Junior
  F_X <- c(1.15, 1.20, 1.15, 1.25)       # fator de ajuda Expert
  
  # Custo diário por trabalhador (USD)
  hr_cost_J_weekday <- 60
  hr_cost_J_weekend <- 70
  hr_cost_X_weekday <- 80
  hr_cost_X_weekend <- 95
  
  total_profit <- 0
  total_sales_units <- 0
  total_hr <- 0
  
  for(s in 1:4) {
    store <- store_names[s]
    
    # Obter previsões para esta loja nesta semana
    fc_store <- forecasts[forecasts$Store == store & forecasts$Week_ID == week_id, ]
    if(nrow(fc_store) != 7) {
      stop(paste("Previsões incompletas para", store, "semana", week_id))
    }
    
    weekly_profit <- 0
    
    for(d in 1:7) {
      # Arredondar J e X para inteiros (não negativos)
      J <- max(0, round(sol[s, d, 1]))
      X <- max(0, round(sol[s, d, 2]))
      PR <- min(0.30, max(0, sol[s, d, 3]))  # promoção entre 0% e 30%
      
      # Previsão de clientes para este dia
      C_pred <- fc_store$Forecast[fc_store$Day == d]
      
      # Número máximo de clientes que podem ser assistidos
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      
      # Custo com RH neste dia (domingo=1, sábado=7 são fim de semana)
      is_weekend <- (d %in% c(1, 7))
      cost_J <- ifelse(is_weekend, hr_cost_J_weekend, hr_cost_J_weekday)
      cost_X <- ifelse(is_weekend, hr_cost_X_weekend, hr_cost_X_weekday)
      daily_cost_hr <- J * cost_J + X * cost_X
      
      # Distribuir clientes: primeiros 7*X são atendidos por X, resto por J
      n_assisted_by_X <- min(X * 7, A)
      n_assisted_by_J <- A - n_assisted_by_X
      
      # Calcular lucro diário
      daily_sales_units <- 0
      daily_revenue_raw <- 0
      
      # Clientes atendidos por Experts (X)
      if(n_assisted_by_X > 0) {
        U_X <- round(F_X[s] * 10 / log(2 - PR))
        P_X_raw <- U_X * (1 - PR) * 1.07
        daily_sales_units <- daily_sales_units + n_assisted_by_X * U_X
        daily_revenue_raw <- daily_revenue_raw + n_assisted_by_X * P_X_raw
      }
      
      # Clientes atendidos por Juniors (J)
      if(n_assisted_by_J > 0) {
        U_J <- round(F_J[s] * 10 / log(2 - PR))
        P_J_raw <- U_J * (1 - PR) * 1.07
        daily_sales_units <- daily_sales_units + n_assisted_by_J * U_J
        daily_revenue_raw <- daily_revenue_raw + n_assisted_by_J * P_J_raw
      }
      
      daily_revenue <- round(daily_revenue_raw)
      
      # Lucro líquido diário = receita - custo RH
      daily_net_profit <- daily_revenue - daily_cost_hr
      weekly_profit <- weekly_profit + daily_net_profit
      total_sales_units <- total_sales_units + daily_sales_units
      total_hr <- total_hr + J + X
      
      if(verbose) {
        cat(sprintf("%s dia %d: J=%d X=%d PR=%.3f C_pred=%.0f A=%d units=%d rev=%.0f cost=%.0f profit=%.0f\n",
                    store, d, J, X, PR, C_pred, A, daily_sales_units, daily_revenue, 
                    daily_cost_hr, daily_net_profit))
      }
    }
    
    # Subtrair custo fixo semanal da loja
    total_profit <- total_profit + weekly_profit - W_s[s]
  }
  
  if(verbose) {
    cat("\n=== RESUMO ===\n")
    cat("Total sales units:", total_sales_units, "\n")
    cat("Total HR (J+X):", total_hr, "\n")
    cat("Total profit:", round(total_profit, 2), "\n")
  }
  
  return(total_profit)
}