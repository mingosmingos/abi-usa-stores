# 22_otimizacao_GA_O2_corrigido.R - GA O2 com death penalty e população inicial semeadas

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
MAX_UNIDADES <- 10000

lower <- rep(0, 84)
upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))

popSize <- 200
maxiter <- 150
pcrossover <- 0.8
pmutation <- 0.15
elitism <- 20

# ============================================================
# CARREGAR PREVISÕES (semana 21)
# ============================================================

previsoes_df <- read.csv(file.path(caminho_base, "all_store_predictions.csv"), 
                         stringsAsFactors = FALSE)
previsoes_semana21 <- previsoes_df[previsoes_df$Run == 21, ]

cat("\n📊 Previsões para a semana 21 carregadas\n")
cat("   Total de registos:", nrow(previsoes_semana21), "\n")

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
      if(length(C_pred) == 0) C_pred <- 0
      
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      
      n_X <- min(X * 7, A)
      n_J <- A - n_X
      
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
# FUNÇÃO DE FITNESS PARA O2 (DEATH PENALTY -Inf)
# ============================================================

fitness_O2 <- function(sol) {
  
  if(is.matrix(sol)) sol <- as.numeric(sol)
  
  total_units <- calcular_unidades_totais(sol)
  
  # DEATH PENALTY - solução inválida = -Inf (morte garantida)
  if(total_units > MAX_UNIDADES) {
    return(-Inf)
  }
  
  # Só calcula lucro se for válida
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
# FUNÇÃO PARA CRIAR POPULAÇÃO INICIAL COM SOLUÇÕES DE BAIXO RH
# ============================================================

criar_populacao_inicial <- function(popSize, lower, upper) {
  
  populacao <- matrix(NA, nrow = popSize, ncol = 84)
  
  for(i in 1:popSize) {
    
    # 60% soluções de baixo RH (explorar região válida)
    # 40% soluções aleatórias normais (diversidade)
    if(i <= 0.6 * popSize) {
      # Solução com poucos trabalhadores (J≤5, X≤3)
      for(j in 1:84) {
        if(j <= 28) {
          # J - valores baixos (0 a 5)
          populacao[i, j] <- sample(0:5, 1)
        } else if(j <= 56) {
          # X - valores baixos (0 a 3)
          populacao[i, j] <- sample(0:3, 1)
        } else {
          # PR - valores normais
          populacao[i, j] <- runif(1, 0, 0.3)
        }
      }
    } else {
      # Solução aleatória normal (exploração)
      populacao[i, ] <- runif(84, lower, upper)
    }
  }
  
  return(populacao)
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
# FUNÇÃO PARA CALCULAR UNIDADES POR DIA
# ============================================================

calcular_unidades_por_dia <- function(sol, week_id) {
  
  dim(sol) <- c(4, 7, 3)
  
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
  
  unidades_por_dia <- matrix(0, nrow = 4, ncol = 7)
  rownames(unidades_por_dia) <- lojas
  colnames(unidades_por_dia) <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sab", "Dom")
  
  for(i in 1:4) {
    store <- lojas[i]
    for(d in 1:7) {
      J <- max(0, round(sol[i, d, 1]))
      X <- max(0, round(sol[i, d, 2]))
      PR <- min(0.30, max(0, sol[i, d, 3]))
      
      C_pred <- previsoes_semana21[previsoes_semana21$Store == store & 
                                     previsoes_semana21$Step == d, "Num_Customers"]
      if(length(C_pred) == 0) C_pred <- 0
      
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      
      n_X <- min(X * 7, A)
      n_J <- A - n_X
      
      dia_units <- 0
      
      if(n_X > 0) {
        U_X <- round(F_X[i] * 10 / log(2 - PR))
        dia_units <- dia_units + n_X * U_X
      }
      if(n_J > 0) {
        U_J <- round(F_J[i] * 10 / log(2 - PR))
        dia_units <- dia_units + n_J * U_J
      }
      
      unidades_por_dia[i, d] <- dia_units
    }
  }
  
  return(unidades_por_dia)
}

# ============================================================
# EXECUTAR ALGORITMO GENÉTICO COM POPULAÇÃO INICIAL SEMEADA
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 ALGORITMO GENÉTICO - O2 (HARD CONSTRAINT: 10.000 unidades)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("\nParâmetros:\n")
cat("   Dimensão:", length(lower), "variáveis\n")
cat("   População:", popSize, "\n")
cat("   Gerações:", maxiter, "\n")
cat("   Limite unidades:", MAX_UNIDADES, "\n")
cat("   Death penalty: -Inf\n")
cat("\n   População inicial: 60% baixo RH (J≤5, X≤3) / 40% aleatória\n")

set.seed(123)

# Criar população inicial manualmente
populacao_inicial <- criar_populacao_inicial(popSize, lower, upper)

# Executar GA com população inicial personalizada
ga_result <- ga(
  type = "real-valued",
  fitness = fitness_O2,
  lower = lower,
  upper = upper,
  popSize = popSize,
  maxiter = maxiter,
  pcrossover = pcrossover,
  pmutation = pmutation,
  elitism = elitism,
  run = 50,
  monitor = TRUE,
  seed = 123,
  suggestions = populacao_inicial  # População inicial semeada!
)

# ============================================================
# RESULTADOS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 RESULTADOS DO ALGORITMO GENÉTICO - O2\n")
cat(paste(rep("=", 70), collapse=""), "\n")

melhor_solucao <- ga_result@solution[1, ]
melhor_fitness <- ga_result@fitnessValue

cat("\n💰 Melhor lucro encontrado: $", round(melhor_fitness, 2), "\n")

total_units <- calcular_unidades_totais(melhor_solucao)

if(is.finite(melhor_fitness) && melhor_fitness > -1e8) {
  cat("✅ Solução VÁLIDA encontrada!\n")
  cat("📦 Unidades totais:", round(total_units, 0), "/", MAX_UNIDADES, "\n")
} else {
  cat("❌ Nenhuma solução válida encontrada com os parâmetros atuais.\n")
}

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
# CALCULAR LUCRO POR DIA E UNIDADES POR DIA
# ============================================================

resultado_lucro <- calcular_lucro_por_dia(melhor_solucao, forecasts, 21)
resultado_unidades <- calcular_unidades_por_dia(melhor_solucao, 21)
dim(melhor_solucao) <- c(4, 7, 3)

dias <- c("Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado", "Domingo")

if(is.finite(melhor_fitness) && melhor_fitness > -1e8) {
  
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("📋 PLANO OTIMIZADO O2 - J, X, PR, Unidades e Lucro por Dia\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  
  for(i in 1:4) {
    cat("\n", paste(rep("-", 70), collapse=""), "\n")
    cat("📍 LOJA:", toupper(lojas[i]), "\n")
    cat(paste(rep("-", 70), collapse=""), "\n")
    
    cat("\n   Dia       | J | X | PR | Previsão | Unidades | Lucro Dia\n")
    cat("   ----------|---|---|----|----------|----------|----------\n")
    
    for(d in 1:7) {
      J <- round(melhor_solucao[i, d, 1])
      X <- round(melhor_solucao[i, d, 2])
      PR <- round(melhor_solucao[i, d, 3] * 100, 0)
      
      pred <- forecasts[forecasts$Store == lojas[i] & forecasts$Day == d, "Forecast"]
      lucro_dia <- resultado_lucro$lucro_por_dia[i, d]
      unidades_dia <- resultado_unidades[i, d]
      
      cat("   ", dias[d], " | ", J, " | ", X, " |  ", PR, "% | ", 
          round(pred, 0), " | ", round(unidades_dia, 0), " | $", round(lucro_dia, 2), "\n", sep="")
    }
    
    total_J <- sum(round(melhor_solucao[i, , 1]))
    total_X <- sum(round(melhor_solucao[i, , 2]))
    total_unidades_loja <- sum(resultado_unidades[i, ])
    lucro_semana_loja <- sum(resultado_lucro$lucro_por_dia[i, ]) - c(700, 730, 760, 800)[i]
    
    cat("\n   📊 TOTAIS DA SEMANA:\n")
    cat("      J =", total_J, " | X =", total_X, " | RH =", total_J + total_X, "\n")
    cat("      Unidades:", round(total_unidades_loja, 0), "\n")
    cat("      Lucro da loja: $", round(lucro_semana_loja, 2), "\n")
  }
  
  # ============================================================
  # RESUMO FINAL
  # ============================================================
  
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("📈 RESUMO FINAL DA SEMANA 21 (O2 - GA)\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  
  cat("\n💰 Lucro TOTAL das 4 lojas: $", round(resultado_lucro$lucro_total, 2), "\n")
  cat("📦 Unidades TOTAL das 4 lojas:", round(total_units, 0), "/", MAX_UNIDADES, "\n")
  
  if(total_units <= MAX_UNIDADES) {
    cat("✅ Restrição de", MAX_UNIDADES, "unidades RESPECTADA!\n")
  } else {
    cat("❌ Restrição de", MAX_UNIDADES, "unidades VIOLADA!\n")
  }
  
  cat("\n📊 Lucro por loja:\n")
  for(i in 1:4) {
    lucro_loja <- sum(resultado_lucro$lucro_por_dia[i, ]) - c(700, 730, 760, 800)[i]
    cat("   ", toupper(lojas[i]), ": $", round(lucro_loja, 2), "\n")
  }
  
  cat("\n📊 Unidades por loja:\n")
  for(i in 1:4) {
    cat("   ", toupper(lojas[i]), ":", round(sum(resultado_unidades[i, ]), 0), "unidades\n")
  }
}

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

resultados_path <- file.path(caminho_base, "analise_modelos", "resultados_GA_O2")
if(!dir.exists(resultados_path)) dir.create(resultados_path)

saveRDS(ga_result, file.path(resultados_path, "ga_resultado.rds"))
if(exists("resultado_lucro")) saveRDS(resultado_lucro, file.path(resultados_path, "lucro_por_dia.rds"))
if(exists("resultado_unidades")) saveRDS(resultado_unidades, file.path(resultados_path, "unidades_por_dia.rds"))

cat("\n✅ Resultados guardados em:", resultados_path, "\n")