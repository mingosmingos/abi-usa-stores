################################################################################
# run_sann_O3.R - Simulated Annealing for Multi-Objective Plan (Profit - HR)
################################################################################
rm(list = ls())

# Load dependencies
source("hill.R")           # For hchange()
source("eval_plan_O1.R")
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
week_id          <- 51
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

tracker <- new.env()
tracker$iter <- 0
tracker$best_val <- -Inf
tracker$history <- c()

# 5. FUNÇÃO DE AVALIAÇÃO (SANN minimiza → negamos o objetivo)
# Initialize tracker as a global list (more reliable than environment)
tracker <- list(
  iter = 0,
  best_val = Inf,  # For minimization, start with +Inf
  history = data.frame(iter = integer(), best_val = numeric())
)

eval_fn_sann <- function(sol) {
  # 1. Calculate value (negative for minimization)
  val <- -eval_plan_O3(sol, forecasts = forecasts, week_id = week_id,
                       max_sales_units = max_sales_units, 
                       weight_hr = weight_hr, verbose = FALSE)
  
  # 2. Update tracker (use <<- to modify global variable)
  tracker$iter <<- tracker$iter + 1
  
  # Update best found so far (minimization: lower is better)
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

# 11. VISUALIZAÇÃO DA CONVERGÊNCIA E FRONTEIRA PARETO (O3)

# First, let's run multiple optimizations with different weight_hr values
cat("\n=== Executando múltiplas otimizações para fronteira Pareto ===\n")

weight_hr_values <- c(0.01, 0.05, 0.1, 0.5, 1.0, 2.0, 5.0, 10.0)
results_list <- list()

set.seed(seed)  # Same seed for fair comparison

for(w_hr in weight_hr_values) {
  cat(sprintf("\nOtimizando com weight_hr = %.3f...\n", w_hr))
  
  # Re-run optimization with this weight
  eval_fn_temp <- function(sol) {
    -eval_plan_O3(sol, forecasts = forecasts, week_id = week_id,
                  max_sales_units = max_sales_units, 
                  weight_hr = w_hr, verbose = FALSE)
  }
  

  s0_temp <- runif(84, min = lower, max = upper)
  
  result_temp <- optim(par = s0_temp,
                       fn = eval_fn_temp,
                       gr = sann_gr,
                       method = "SANN",
                       control = list(maxit = 2000, temp = initial_temp, tmax = 10))
  
  # Extract profit and HR from the solution
  temp_profit <- eval_plan_O1(result_temp$par, forecasts, week_id, verbose = FALSE)
  dim(result_temp$par) <- c(4, 7, 3)
  temp_hr <- sum(round(result_temp$par[, , 1]) + round(result_temp$par[, , 2]))
  
  results_list[[as.character(w_hr)]] <- list(
    profit = temp_profit,
    hr = temp_hr,
    solution = result_temp$par
  )
  
  cat(sprintf("  Profit: $%.2f, Total HR: %d\n", temp_profit, temp_hr))
}

# Now plot the Pareto front
if(require(ggplot2, quietly = TRUE)) {
  df <- do.call(rbind, lapply(names(results_list), function(name) {
    data.frame(
      Weight_HR = as.numeric(name),
      Profit = results_list[[name]]$profit,
      Total_HR = results_list[[name]]$hr
    )
  }))
  
  # Create Pareto plot
  p <- ggplot(df, aes(x = Total_HR, y = Profit, color = factor(Weight_HR))) +
    geom_point(size = 4, alpha = 0.8) +
    geom_line(alpha = 0.3) +
    scale_color_viridis_d(name = "HR Weight") +
    labs(title = "O3: Profit vs HR Usage Trade-off",
         subtitle = paste("Week", week_id, "- Each point = different weight_hr"),
         x = "Total HR Resources (J+X)", 
         y = "Total Profit (USD)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  
  print(p)
  
  # Save plot
  ggsave(filename = paste0("pareto_front_O3_week_", week_id, ".png"), 
         plot = p, width = 10, height = 6, dpi = 300)
  
  cat("\nGráfico Pareto salvo em: pareto_front_O3_week_", week_id, ".png\n", sep = "")
  
  # Show best solutions
  cat("\n=== RESUMO DAS SOLUÇÕES PARETO ===\n")
  print(df[order(df$Profit, decreasing = TRUE), ])
  
} else {
  cat("Instale ggplot2 para visualização: install.packages('ggplot2')\n")
}