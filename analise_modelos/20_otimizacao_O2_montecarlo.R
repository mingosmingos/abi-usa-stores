# 20_otimizacao_O2_montecarlo.R
# O2 - Hard constraint: máximo de 10.000 unidades para as 4 lojas
# Soluções inválidas → death penalty (-Inf)

source("scripts/utils.R")
source("scripts/optimization.R")
source("eval_plan_O1.R")
#source("montecarlo.R")

# ============================================================
# PARÂMETROS
# ============================================================

MAX_J <- 20
MAX_X <- 15
MAX_PR <- 0.30
MAX_UNIDADES <- 10000  # Limite HARD!

lower <- rep(0, 84)
upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))
N_SAMPLES <- 500000

# ============================================================
# CARREGAR PREVISÕES
# ============================================================

previsoes_df <- read.csv("all_store_predictions.csv", stringsAsFactors = FALSE)
previsoes_semana21 <- previsoes_df[previsoes_df$Run == 21, ]

cat("\n📊 Previsões para a semana 21:\n")
print(previsoes_semana21)

# ============================================================
# FUNÇÃO PARA CALCULAR UNIDADES TOTAIS
# ============================================================

calcular_unidades_totais <- function(sol) {
  
  dim(sol) <- c(4, 7, 3)
  
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
  
  total_units <- 0
  
  for(i in 1:4) {
    store <- lojas[i]
    for(d in 1:7) {
      J <- max(0, round(sol[i, d, 1]))
      X <- max(0, round(sol[i, d, 2]))
      PR <- min(0.30, max(0, sol[i, d, 3]))
      
      C_pred <- previsoes_semana21[previsoes_semana21$Store == store & 
                                     previsoes_semana21$Step == d, "Num_Customers"]
      
      # Clientes assistidos
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      
      # Distribuição
      n_X <- min(X * 7, A)
      n_J <- A - n_X
      
      # Unidades por cliente
      if(n_X > 0) {
        U_X <- round(F_X[i] * 10 / log(2 - PR))
        total_units <- total_units + n_X * U_X
      }
      if(n_J > 0) {
        U_J <- round(F_J[i] * 10 / log(2 - PR))
        total_units <- total_units + n_J * U_J
      }
    }
  }
  
  return(total_units)
}

# ============================================================
# FUNÇÃO DE AVALIAÇÃO O2 (DEATH PENALTY)
# ============================================================

eval_function_O2 <- function(sol) {
  
  # 1. Calcular unidades totais
  total_units <- calcular_unidades_totais(sol)
  
  # 2. HARD CONSTRAINT: se ultrapassar limite, morte da solução!
  if(total_units > MAX_UNIDADES) {
    return(-Inf)  # Death penalty!
  }
  
  # 3. Se válida, calcular lucro
  dim(sol) <- c(4, 7, 3)
  
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  
  # Preparar forecasts
  forecasts <- data.frame()
  for(i in 1:4) {
    store <- lojas[i]
    for(d in 1:7) {
      pred_value <- previsoes_semana21[previsoes_semana21$Store == store & 
                                         previsoes_semana21$Step == d, "Num_Customers"]
      forecasts <- rbind(forecasts, data.frame(
        Store = store, Week_ID = 21, Day = d, Forecast = pred_value
      ))
    }
  }
  
  # Calcular lucro
  profit <- eval_plan_O1(sol, forecasts, week_id = 21)
  
  return(profit)
}

# ============================================================
# OTIMIZAÇÃO MONTE CARLO PARA O2
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 OTIMIZAÇÃO O2 - HARD CONSTRAINT (10.000 unidades)\n")
cat("   Soluções inválidas → -Inf (death penalty)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("\n🔍 A testar", N_SAMPLES, "soluções aleatórias...\n")

resultado <- mcsearch(
  fn = eval_function_O2,
  lower = lower,
  upper = upper,
  N = N_SAMPLES,
  type = "max"
)

# ============================================================
# RESULTADOS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 MELHOR SOLUÇÃO O2 PARA A SEMANA 21\n")
cat(paste(rep("=", 70), collapse=""), "\n")

if(resultado$eval == -Inf) {
  cat("\n❌ NENHUMA SOLUÇÃO VÁLIDA ENCONTRADA!\n")
  cat("   O limite de", MAX_UNIDADES, "unidades é muito restritivo.\n")
  cat("   Sugestão: aumentar limite ou reduzir previsões.\n")
} else {
  cat("\n💰 Lucro total (válido!): $", round(resultado$eval, 2), "\n")
  
  # Verificar unidades da melhor solução
  melhor_solucao <- resultado$sol
  total_units <- calcular_unidades_totais(melhor_solucao)
  cat("📦 Unidades totais:", round(total_units, 0), "/", MAX_UNIDADES, "\n")
  
  # ============================================================
  # PLANO DETALHADO
  # ============================================================
  
  dim(melhor_solucao) <- c(4, 7, 3)
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  dias <- c("Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado", "Domingo")
  
  cat("\n📋 PLANO OTIMIZADO O2:\n")
  
  for(i in 1:4) {
    cat("\n", paste(rep("-", 50), collapse=""), "\n")
    cat("📍 LOJA:", toupper(lojas[i]), "\n")
    cat(paste(rep("-", 50), collapse=""), "\n")
    cat("\n   Dia       | J | X | PR\n")
    cat("   ----------|---|---|-----\n")
    
    for(d in 1:7) {
      J <- round(melhor_solucao[i, d, 1])
      X <- round(melhor_solucao[i, d, 2])
      PR <- round(melhor_solucao[i, d, 3] * 100, 0)
      cat("   ", dias[d], " | ", J, " | ", X, " | ", PR, "%\n", sep="")
    }
    
    total_J <- sum(round(melhor_solucao[i, , 1]))
    total_X <- sum(round(melhor_solucao[i, , 2]))
    cat("\n   Totais: J=", total_J, " | X=", total_X, " | RH=", total_J + total_X, "\n")
  }
}

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

resultados_path <- "resultados_O2_semana21"
if(!dir.exists(resultados_path)) dir.create(resultados_path)

saveRDS(resultado, file.path(resultados_path, "resultado_montecarlo_O2.rds"))

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("✅ RESULTADOS GUARDADOS EM:\n")
cat("   ", resultados_path, "\n")
cat(paste(rep("=", 70), collapse=""), "\n")