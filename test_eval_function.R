# eval_plan_corrigida.R
# Função de avaliação corrigida conforme os exemplos do professor

#' Avalia o lucro semanal de uma única loja
#'
#' @param J vetor de 7 inteiros (Juniors por dia)
#' @param X vetor de 7 inteiros (Experts por dia)
#' @param PR vetor de 7 valores entre 0 e 0.3 (promoção)
#' @param C_pred vetor de 7 previsões de clientes
#' @param F_J fator de ajuda Junior
#' @param F_X fator de ajuda Expert
#' @param W_s custo fixo semanal
#' @param verbose exibir detalhes?
#' @return lucro semanal (após custo fixo)
eval_plan_store <- function(J, X, PR, C_pred, F_J, F_X, W_s, verbose = FALSE) {
  # Custos RH
  hr_cost_J_weekday <- 60
  hr_cost_J_weekend <- 70
  hr_cost_X_weekday <- 80
  hr_cost_X_weekend <- 95
  
  total_profit <- 0
  total_units <- 0
  total_revenue <- 0
  total_cost_hr <- 0
  
  for(d in 1:7) {
    Jd <- max(0, round(J[d]))
    Xd <- max(0, round(X[d]))
    PRd <- min(0.30, max(0, PR[d]))
    C <- C_pred[d]
    
    # Clientes assistidos
    max_assisted <- 7 * Xd + 6 * Jd
    A <- min(max_assisted, C)
    
    # Custo RH diário
    is_weekend <- (d %in% c(1, 7))  # domingo=1, sábado=7
    cost_J <- ifelse(is_weekend, hr_cost_J_weekend, hr_cost_J_weekday)
    cost_X <- ifelse(is_weekend, hr_cost_X_weekend, hr_cost_X_weekday)
    daily_cost_hr <- Jd * cost_J + Xd * cost_X
    
    # Distribuição: primeiros 7*Xd atendidos por X, depois J
    n_X <- min(Xd * 7, A)
    n_J <- A - n_X
    
    daily_units <- 0
    daily_revenue_raw <- 0
    
    if(n_X > 0) {
      U_X_raw <- F_X * 10 / log(2 - PRd)
      U_X <- round(U_X_raw)
      P_X_raw <- U_X * (1 - PRd) * 1.07
      daily_units <- daily_units + n_X * U_X
      daily_revenue_raw <- daily_revenue_raw + n_X * P_X_raw
    }
    if(n_J > 0) {
      U_J_raw <- F_J * 10 / log(2 - PRd)
      U_J <- round(U_J_raw)
      P_J_raw <- U_J * (1 - PRd) * 1.07
      daily_units <- daily_units + n_J * U_J
      daily_revenue_raw <- daily_revenue_raw + n_J * P_J_raw
    }
    
    daily_revenue <- round(daily_revenue_raw)
    daily_profit <- daily_revenue - daily_cost_hr
    
    total_units <- total_units + daily_units
    total_revenue <- total_revenue + daily_revenue
    total_cost_hr <- total_cost_hr + daily_cost_hr
    total_profit <- total_profit + daily_profit
    
    if(verbose) {
      dia <- c("Dom","Seg","Ter","Qua","Qui","Sex","Sab")[d]
      cat(sprintf("%s (d=%d): J=%2d X=%2d PR=%.2f | pred=%3.0f A=%2d | units=%3d rev=%4.0f cost=%3.0f profit=%4.0f\n",
                  dia, d, Jd, Xd, PRd, C, A, daily_units, daily_revenue, daily_cost_hr, daily_profit))
    }
  }
  
  weekly_profit <- total_profit - W_s
  if(verbose) {
    cat(sprintf("\nTotal units: %d\nTotal revenue: %.0f\nTotal HR cost: %.0f\nFixed cost W_s: %.0f\nWeekly profit: %.0f\n",
                total_units, total_revenue, total_cost_hr, W_s, weekly_profit))
  }
  return(weekly_profit)
}

# ==================== TESTES ====================
# Baltimore
cat("\n========== TESTE BALTIMORE ==========\n")
J_balt <- c(0, 10, 4, 0, 5, 5, 4)
X_balt <- c(4, 0, 8, 20, 0, 4, 3)
PR_balt <- c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
C_balt <- c(97, 61, 65, 71, 65, 89, 125)
F_J_balt <- 1.00
F_X_balt <- 1.15
W_s_balt <- 700

profit_balt <- eval_plan_store(J_balt, X_balt, PR_balt, C_balt, F_J_balt, F_X_balt, W_s_balt, verbose = TRUE)
cat(sprintf("\nLucro Baltimore: %.0f (esperado: 146)\n", profit_balt))

# Philadelphia
cat("\n========== TESTE PHILADELPHIA ==========\n")
J_phil <- c(0, 10, 4, 0, 5, 5, 4)
X_phil <- c(4, 0, 8, 20, 0, 4, 3)
PR_phil <- c(0.00, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30)
C_phil <- c(230, 144, 154, 168, 154, 211, 298)
F_J_phil <- 1.10
F_X_phil <- 1.15
W_s_phil <- 760

profit_phil <- eval_plan_store(J_phil, X_phil, PR_phil, C_phil, F_J_phil, F_X_phil, W_s_phil, verbose = TRUE)
cat(sprintf("\nLucro Philadelphia: %.0f (esperado: 1728)\n", profit_phil))