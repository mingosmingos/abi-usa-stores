# 21_otimizacao_GA_O1_detalhado.R - GA O1 com tabela detalhada

# ============================================================
# CAMINHOS ABSOLUTOS
# ============================================================

caminho_base <- "~/GitHub/abi-usa-stores"

source(file.path(caminho_base, "scripts", "utils.R"))
source(file.path(caminho_base, "scripts", "optimization.R"))
source(file.path(caminho_base, "eval_plan_O1.R"))

library(GA)

# ============================================================
# PARÂMETROS
# ============================================================

MAX_J <- 20
MAX_X <- 15
MAX_PR <- 0.30

lower <- rep(0, 84)
upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))

popSize <- 100
maxiter <- 100
pcrossover <- 0.8
pmutation <- 0.1
elitism <- 10

# ============================================================
# CARREGAR PREVISÕES (semana 21)
# ============================================================

previsoes_df <- read.csv(file.path(caminho_base, "all_store_predictions.csv"), 
                         stringsAsFactors = FALSE)
previsoes_semana21 <- previsoes_df[previsoes_df$Run == 21, ]

cat("\n📊 Previsões para a semana 21 carregadas\n")
cat("   Total de registos:", nrow(previsoes_semana21), "\n")

# ============================================================
# FUNÇÃO DE FITNESS PARA O1
# ============================================================

fitness_O1 <- function(sol) {
  
  if(is.matrix(sol)) sol <- as.numeric(sol)
  dim(sol) <- c(4, 7, 3)
  
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  forecasts <- data.frame()
  
  for(i in 1:4) {
    store <- lojas[i]
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
  
  profit <- eval_plan_O1(sol, forecasts, week_id = 21)
  return(profit)
}

# ============================================================
# FUNÇÃO PARA CALCULAR LUCRO POR DIA (DETALHADO)
# ============================================================

calcular_lucro_por_dia <- function(sol, forecasts, week_id) {
  
  dim(sol) <- c(4, 7, 3)
  
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  W_s <- c(700, 730, 760, 800)
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
  
  hr_cost_J_weekday <- 60
  hr_cost_J_weekend <- 70
  hr_cost_X_weekday <- 80
  hr_cost_X_weekend <- 95
  
  lucro_por_dia <- matrix(0, nrow = 4, ncol = 7)
  rownames(lucro_por_dia) <- lojas
  colnames(lucro_por_dia) <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")
  
  lucro_total <- 0
  
  for(s in 1:4) {
    store <- lojas[s]
    fc_store <- forecasts[forecasts$Store == store & forecasts$Week_ID == week_id, ]
    
    if(nrow(fc_store) != 7) next
    
    for(d in 1:7) {
      J <- max(0, round(sol[s, d, 1]))
      X <- max(0, round(sol[s, d, 2]))
      PR <- min(0.30, max(0, sol[s, d, 3]))
      
      C_pred <- fc_store$Forecast[fc_store$Day == d]
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      
      is_weekend <- (d %in% c(6, 7))
      cost_J <- ifelse(is_weekend, hr_cost_J_weekend, hr_cost_J_weekday)
      cost_X <- ifelse(is_weekend, hr_cost_X_weekend, hr_cost_X_weekday)
      daily_cost_hr <- J * cost_J + X * cost_X
      
      n_assisted_by_X <- min(X * 7, A)
      n_assisted_by_J <- A - n_assisted_by_X
      
      daily_revenue <- 0
      
      if(n_assisted_by_X > 0) {
        U_X <- round(F_X[s] * 10 / log(2 - PR))
        P_X <- round(U_X * (1 - PR) * 1.07)
        daily_revenue <- daily_revenue + n_assisted_by_X * P_X
      }
      if(n_assisted_by_J > 0) {
        U_J <- round(F_J[s] * 10 / log(2 - PR))
        P_J <- round(U_J * (1 - PR) * 1.07)
        daily_revenue <- daily_revenue + n_assisted_by_J * P_J
      }
      
      lucro_dia <- daily_revenue - daily_cost_hr
      lucro_por_dia[s, d] <- lucro_dia
      lucro_total <- lucro_total + lucro_dia
    }
    
    lucro_total <- lucro_total - W_s[s]
  }
  
  return(list(
    lucro_por_dia = lucro_por_dia,
    lucro_total = lucro_total
  ))
}

# ============================================================
# EXECUTAR ALGORITMO GENÉTICO
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 ALGORITMO GENÉTICO - O1 (MAXIMIZAR LUCRO)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("\nParâmetros:\n")
cat("   Dimensão:", length(lower), "variáveis\n")
cat("   População:", popSize, "\n")
cat("   Gerações:", maxiter, "\n")

set.seed(123)

ga_result <- ga(
  type = "real-valued",
  fitness = fitness_O1,
  lower = lower,
  upper = upper,
  popSize = popSize,
  maxiter = maxiter,
  pcrossover = pcrossover,
  pmutation = pmutation,
  elitism = elitism,
  run = 50,
  monitor = TRUE,
  seed = 123
)

# ============================================================
# RESULTADOS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 RESULTADOS DO ALGORITMO GENÉTICO - O1\n")
cat(paste(rep("=", 70), collapse=""), "\n")

melhor_solucao <- ga_result@solution[1, ]
melhor_fitness <- ga_result@fitnessValue

cat("\n💰 Melhor lucro encontrado: $", round(melhor_fitness, 2), "\n")

# ============================================================
# PREPARAR FORECASTS PARA TABELA DETALHADA
# ============================================================

forecasts <- data.frame()
lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")

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

# ============================================================
# CALCULAR LUCRO POR DIA
# ============================================================

resultado_detalhado <- calcular_lucro_por_dia(melhor_solucao, forecasts, 21)
dim(melhor_solucao) <- c(4, 7, 3)

dias <- c("Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado", "Domingo")

# ============================================================
# TABELA DETALHADA POR LOJA
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📋 PLANO OTIMIZADO E LUCRO POR DIA\n")
cat(paste(rep("=", 70), collapse=""), "\n")

for(i in 1:4) {
  cat("\n", paste(rep("-", 60), collapse=""), "\n")
  cat("📍 LOJA:", toupper(lojas[i]), "\n")
  cat(paste(rep("-", 60), collapse=""), "\n")
  
  cat("\n   Dia       | J | X | PR | Previsão | Lucro Dia\n")
  cat("   ----------|---|---|----|----------|----------\n")
  
  for(d in 1:7) {
    J <- round(melhor_solucao[i, d, 1])
    X <- round(melhor_solucao[i, d, 2])
    PR <- round(melhor_solucao[i, d, 3] * 100, 0)
    
    pred <- forecasts[forecasts$Store == lojas[i] & forecasts$Day == d, "Forecast"]
    lucro_dia <- resultado_detalhado$lucro_por_dia[i, d]
    
    cat("   ", dias[d], " | ", J, " | ", X, " |  ", PR, "% | ", 
        round(pred, 0), " | $", round(lucro_dia, 2), "\n", sep="")
  }
  
  # Totais da loja
  total_J <- sum(round(melhor_solucao[i, , 1]))
  total_X <- sum(round(melhor_solucao[i, , 2]))
  lucro_semana_loja <- sum(resultado_detalhado$lucro_por_dia[i, ]) - c(700, 730, 760, 800)[i]
  
  cat("\n   📊 TOTAIS DA SEMANA:\n")
  cat("      J =", total_J, " | X =", total_X, " | RH =", total_J + total_X, "\n")
  cat("      Lucro da loja: $", round(lucro_semana_loja, 2), "\n")
}

# ============================================================
# RESUMO FINAL
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📈 RESUMO FINAL DA SEMANA 21 (O1 - GA)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("\n💰 Lucro TOTAL das 4 lojas: $", round(resultado_detalhado$lucro_total, 2), "\n")

cat("\n📊 Lucro por loja:\n")
for(i in 1:4) {
  lucro_loja <- sum(resultado_detalhado$lucro_por_dia[i, ]) - c(700, 730, 760, 800)[i]
  cat("   ", toupper(lojas[i]), ": $", round(lucro_loja, 2), "\n")
}

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

resultados_path <- file.path(caminho_base, "analise_modelos", "resultados_GA_O1")
if(!dir.exists(resultados_path)) dir.create(resultados_path)

saveRDS(ga_result, file.path(resultados_path, "ga_resultado.rds"))
saveRDS(resultado_detalhado, file.path(resultados_path, "lucro_por_dia.rds"))

# Guardar tabela detalhada em CSV
tabela_detalhada <- data.frame()
for(i in 1:4) {
  for(d in 1:7) {
    tabela_detalhada <- rbind(tabela_detalhada, data.frame(
      Loja = toupper(lojas[i]),
      Dia = dias[d],
      J = round(melhor_solucao[i, d, 1]),
      X = round(melhor_solucao[i, d, 2]),
      PR = round(melhor_solucao[i, d, 3] * 100, 0),
      Previsao = round(forecasts[forecasts$Store == lojas[i] & forecasts$Day == d, "Forecast"], 0),
      Lucro_Dia = round(resultado_detalhado$lucro_por_dia[i, d], 2)
    ))
  }
}
write.csv(tabela_detalhada, file.path(resultados_path, "tabela_detalhada.csv"), row.names = FALSE)

cat("\n✅ Resultados guardados em:", resultados_path, "\n")
cat("   - ga_resultado.rds\n")
cat("   - lucro_por_dia.rds\n")
cat("   - tabela_detalhada.csv\n")