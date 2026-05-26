# Verificar unidades da melhor solução O3

caminho_base <- "~/GitHub/abi-usa-stores"

# Carregar resultados do O3
resultados_path <- file.path(caminho_base, "analise_modelos", "resultados_O3_PARETO")
df_pareto <- readRDS(file.path(resultados_path, "fronteira_pareto.rds"))

# Carregar a melhor solução (a que minimiza F = RH - 0.1×Lucro)
melhor_idx <- which.min(df_pareto$F_objetivo)
melhor_rh <- df_pareto$RH[melhor_idx]
melhor_lucro <- df_pareto$Lucro[melhor_idx]

cat("📊 Melhor solução O3:\n")
cat("   RH =", melhor_rh, "funcionários\n")
cat("   Lucro = $", round(melhor_lucro, 2), "\n")

# ============================================================
# RECALCULAR UNIDADES PARA CONFIRMAR
# ============================================================

source(file.path(caminho_base, "scripts", "utils.R"))
source(file.path(caminho_base, "scripts", "optimization.R"))
source(file.path(caminho_base, "eval_plan_O3.R"))

# Carregar previsões
previsoes_df <- read.csv(file.path(caminho_base, "all_store_predictions.csv"), 
                         stringsAsFactors = FALSE)
previsoes_semana21 <- previsoes_df[previsoes_df$Run == 21, ]

lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")

forecasts <- data.frame()
for(store in lojas) {
  for(d in 1:7) {
    pred_value <- previsoes_semana21[previsoes_semana21$Store == store & 
                                       previsoes_semana21$Step == d, "Num_Customers"]
    if(length(pred_value) > 0) {
      forecasts <- rbind(forecasts, data.frame(
        Store = store, Week_ID = 21, Day = d, Forecast = pred_value
      ))
    }
  }
}

# Carregar a solução do NSGA-II
nsga2_resultado <- readRDS(file.path(resultados_path, "nsga2_resultado.rds"))

# Encontrar a solução que corresponde ao melhor F
# (precisamos da solução original, não só do df_pareto)
objetivos <- nsga2_resultado$value
pareto <- nsga2_resultado$pareto.optimal

# Calcular F para cada solução Pareto
f_values <- objetivos[pareto, 1] - 0.1 * (-objetivos[pareto, 2])
melhor_idx_nsga2 <- which.min(f_values)

# Obter a solução correspondente
solucoes_pareto <- nsga2_resultado$par[pareto, ]
melhor_solucao <- solucoes_pareto[melhor_idx_nsga2, ]

# ============================================================
# FUNÇÃO PARA CALCULAR UNIDADES MANUALMENTE
# ============================================================

calcular_unidades <- function(sol) {
  if(is.matrix(sol)) sol <- as.numeric(sol)
  dim(sol) <- c(4, 7, 3)
  
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
  
  total_units <- 0
  
  for(s in 1:4) {
    store <- lojas[s]
    for(d in 1:7) {
      J <- max(0, round(sol[s, d, 1]))
      X <- max(0, round(sol[s, d, 2]))
      PR <- min(0.30, max(0, sol[s, d, 3]))
      
      C_pred <- forecasts[forecasts$Store == store & forecasts$Day == d, "Forecast"]
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      n_X <- min(X * 7, A)
      n_J <- A - n_X
      
      if(n_X > 0) {
        U_X <- round(F_X[s] * 10 / log(2 - PR))
        total_units <- total_units + n_X * U_X
      }
      if(n_J > 0) {
        U_J <- round(F_J[s] * 10 / log(2 - PR))
        total_units <- total_units + n_J * U_J
      }
    }
  }
  return(total_units)
}

# Aplicar repair à solução (garantia extra)
sol_repaired <- repair_solution_inplace(melhor_solucao, forecasts, 21, 10000)

unidades <- calcular_unidades(sol_repaired)

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 VERIFICAÇÃO DE UNIDADES\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("\n💰 Lucro da solução: $", round(melhor_lucro, 2))
cat("\n👥 RH:", melhor_rh, "funcionários")
cat("\n📦 Unidades totais:", unidades, "/ 10000")

if(unidades <= 10000) {
  cat("\n✅ RESTRIÇÃO RESPECTADA! (unidades ≤ 10000)")
} else {
  cat("\n❌ RESTRIÇÃO VIOLADA! (unidades > 10000)")
}

# ============================================================
# MOSTRAR UNIDADES POR LOJA
# ============================================================

cat("\n\n📊 Unidades por loja:\n")
dim(sol_repaired) <- c(4, 7, 3)

for(s in 1:4) {
  store <- lojas[s]
  unidades_loja <- 0
  for(d in 1:7) {
    J <- max(0, round(sol_repaired[s, d, 1]))
    X <- max(0, round(sol_repaired[s, d, 2]))
    PR <- min(0.30, max(0, sol_repaired[s, d, 3]))
    C_pred <- forecasts[forecasts$Store == store & forecasts$Day == d, "Forecast"]
    max_assisted <- 7 * X + 6 * J
    A <- min(max_assisted, C_pred)
    n_X <- min(X * 7, A)
    n_J <- A - n_X
    
    if(n_X > 0) {
      U_X <- round(c(1.15,1.20,1.15,1.25)[s] * 10 / log(2 - PR))
      unidades_loja <- unidades_loja + n_X * U_X
    }
    if(n_J > 0) {
      U_J <- round(c(1.00,1.05,1.10,1.15)[s] * 10 / log(2 - PR))
      unidades_loja <- unidades_loja + n_J * U_J
    }
  }
  cat("   ", toupper(store), ":", round(unidades_loja, 0), "unidades\n")
}