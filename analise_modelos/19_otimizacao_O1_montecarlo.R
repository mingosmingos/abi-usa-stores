# 19_otimizacao_O1_montecarlo.R
# Prever a semana 21 com base nas semanas 1 a 20
# Usa a função mcsearch fornecida pelo stor (dentro de analise_modelos/)

source("scripts/utils.R")
source("scripts/optimization.R")
source("eval_plan_O1.R")

# ============================================================
# CARREGAR A FUNÇÃO mcsearch (dentro da pasta analise_modelos)
# ============================================================

# Como o script está em analise_modelos/, e o ficheiro montecarlo.R
# também está em analise_modelos/, podemos usar:
#source("montecarlo.R")

# Se ainda der erro, usa o caminho absoluto:
# source("analise_modelos/montecarlo.R")

# ============================================================
# PARÂMETROS
# ============================================================

MAX_J <- 20      # Máximo de Juniors por dia
MAX_X <- 15      # Máximo de Experts por dia
MAX_PR <- 0.30   # Máximo de promoção (30%)

# Limites para o vetor de 84 posições
lower <- rep(0, 84)  # tudo zero

upper <- c(
  rep(MAX_J, 28),   # 4 lojas × 7 dias = 28 posições para J
  rep(MAX_X, 28),   # 28 posições para X
  rep(MAX_PR, 28)   # 28 posições para PR
)

N_SAMPLES <- 5000  # Número de amostras

# ============================================================
# CARREGAR PREVISÕES DAS SEMANAS 1 a 20
# ============================================================

# O ficheiro all_store_predictions.csv está na raiz do projeto
# Como estamos em analise_modelos/, temos que subir um nível
previsoes_df <- read.csv("~/GitHub/abi-usa-stores/all_store_predictions.csv", stringsAsFactors = FALSE)

# Se não funcionar, usa caminho absoluto:
# previsoes_df <- read.csv("~/GitHub/abi-usa-stores/all_store_predictions.csv", stringsAsFactors = FALSE)

# Filtrar semanas 1 a 20 (aprender padrões)
previsoes_treino <- previsoes_df[previsoes_df$Run <= 20, ]

# Extrair as previsões da semana 21 (o que vamos prever)
previsoes_semana21 <- previsoes_df[previsoes_df$Run == 21, ]

# Verificar se existem previsões para a semana 21
if(nrow(previsoes_semana21) == 0) {
  # Se não existir semana 21, estimar com base na média das semanas 1-20
  cat("\n⚠️ Semana 21 não encontrada. A estimar com base na média das semanas 1-20...\n")
  
  previsoes_semana21 <- data.frame()
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  
  for(store in lojas) {
    for(day in 1:7) {
      # Calcular média dos dias equivalentes nas semanas 1-20
      media_prev <- mean(previsoes_treino[previsoes_treino$Store == store & 
                                            previsoes_treino$Step == day, "Num_Customers"])
      previsoes_semana21 <- rbind(previsoes_semana21, data.frame(
        Run = 21,
        Step = day,
        Num_Customers = media_prev,
        Store = store
      ))
    }
  }
}

cat("\n📊 Previsões para a semana 21:\n")
print(previsoes_semana21)

# ============================================================
# FUNÇÃO DE AVALIAÇÃO QUE RETORNA LUCRO TOTAL (para otimização)
# ============================================================

eval_function_total <- function(sol) {
  # Converter vetor para matriz 4x7x3
  dim(sol) <- c(4, 7, 3)
  
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  
  # Preparar forecasts para a semana 21
  forecasts <- data.frame()
  
  for(i in 1:4) {
    store <- lojas[i]
    for(day in 1:7) {
      pred_value <- previsoes_semana21[previsoes_semana21$Store == store & 
                                         previsoes_semana21$Step == day, "Num_Customers"]
      if(length(pred_value) > 0) {
        forecasts <- rbind(forecasts, data.frame(
          Store = store,
          Week_ID = 21,
          Day = day,
          Forecast = pred_value
        ))
      }
    }
  }
  
  # Calcular lucro total
  profit <- eval_plan_O1(sol, forecasts, week_id = 21)
  
  return(profit)
}

# ============================================================
# FUNÇÃO PARA CALCULAR LUCRO POR DIA (detalhado)
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
  
  # Matriz para guardar lucro por loja e dia
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
    
    # Subtrair custo fixo semanal da loja
    lucro_total <- lucro_total - W_s[s]
  }
  
  return(list(
    lucro_por_dia = lucro_por_dia,
    lucro_total = lucro_total
  ))
}

# ============================================================
# OTIMIZAÇÃO MONTE CARLO PARA A SEMANA 21
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 OTIMIZAÇÃO O1 PARA A SEMANA 21 (MONTE CARLO)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("\n🔍 A testar", N_SAMPLES, "soluções aleatórias...\n")

# Usar a função mcsearch do stor
resultado <- mcsearch(
  fn = eval_function_total,
  lower = lower,
  upper = upper,
  N = N_SAMPLES,
  type = "max"    # Maximizar lucro
)

# ============================================================
# PREPARAR FORECASTS PARA A SEMANA 21 (usado no cálculo detalhado)
# ============================================================

forecasts_semana21 <- data.frame()
lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")

for(i in 1:4) {
  store <- lojas[i]
  for(day in 1:7) {
    pred_value <- previsoes_semana21[previsoes_semana21$Store == store & 
                                       previsoes_semana21$Step == day, "Num_Customers"]
    if(length(pred_value) > 0) {
      forecasts_semana21 <- rbind(forecasts_semana21, data.frame(
        Store = store,
        Week_ID = 21,
        Day = day,
        Forecast = pred_value
      ))
    }
  }
}

# ============================================================
# CALCULAR LUCRO POR DIA COM A MELHOR SOLUÇÃO
# ============================================================

melhor_solucao <- resultado$sol
resultado_detalhado <- calcular_lucro_por_dia(melhor_solucao, forecasts_semana21, 21)

# ============================================================
# RESULTADOS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 MELHOR SOLUÇÃO ENCONTRADA PARA A SEMANA 21\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("\n💰 Lucro total da semana: $", round(resultado_detalhado$lucro_total, 2), "\n")

# ============================================================
# CONVERTER SOLUÇÃO PARA MATRIZ LEGÍVEL
# ============================================================

dim(melhor_solucao) <- c(4, 7, 3)

dias <- c("Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado", "Domingo")

cat("\n📋 PLANO OTIMIZADO E LUCRO POR DIA:\n")

for(i in 1:4) {
  cat("\n", paste(rep("-", 50), collapse=""), "\n")
  cat("📍 LOJA:", toupper(lojas[i]), "\n")
  cat(paste(rep("-", 50), collapse=""), "\n")
  
  cat("\n   Dia       | J | X | PR | Lucro Dia\n")
  cat("   ----------|---|---|----|----------\n")
  
  for(d in 1:7) {
    J <- round(melhor_solucao[i, d, 1])
    X <- round(melhor_solucao[i, d, 2])
    PR <- round(melhor_solucao[i, d, 3] * 100, 0)
    lucro_dia <- resultado_detalhado$lucro_por_dia[i, d]
    
    cat("   ", dias[d], " | ", J, " | ", X, " |  ", PR, "% | $", round(lucro_dia, 2), "\n", sep="")
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
cat("📈 RESUMO FINAL DA SEMANA 21\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("\n💰 Lucro TOTAL das 4 lojas: $", round(resultado_detalhado$lucro_total, 2), "\n")

# Tabela resumo de lucro por loja
cat("\n📊 Lucro por loja:\n")
for(i in 1:4) {
  lucro_loja <- sum(resultado_detalhado$lucro_por_dia[i, ]) - c(700, 730, 760, 800)[i]
  cat("   ", toupper(lojas[i]), ": $", round(lucro_loja, 2), "\n")
}

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

resultados_path <- "resultados_O1_semana21"
if(!dir.exists(resultados_path)) {
  dir.create(resultados_path)
}

# Guardar solução em CSV
sol_matrix <- matrix(melhor_solucao, nrow = 4, ncol = 21, byrow = TRUE)
rownames(sol_matrix) <- lojas
colnames(sol_matrix) <- c(paste0("J", 1:7), paste0("X", 1:7), paste0("PR", 1:7))
write.csv(sol_matrix, file.path(resultados_path, "solucao_semana21.csv"))

# Guardar lucro por dia
write.csv(resultado_detalhado$lucro_por_dia, file.path(resultados_path, "lucro_por_dia.csv"))

# Guardar resultados completos
saveRDS(resultado, file.path(resultados_path, "resultado_montecarlo.rds"))

# Guardar resumo
resumo <- data.frame(
  Semana = 21,
  Lucro_Total = resultado_detalhado$lucro_total,
  Amostras = N_SAMPLES
)
write.csv(resumo, file.path(resultados_path, "resumo.csv"), row.names = FALSE)

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("✅ RESULTADOS GUARDADOS EM:\n")
cat("   ", resultados_path, "\n")
cat(paste(rep("=", 70), collapse=""), "\n")