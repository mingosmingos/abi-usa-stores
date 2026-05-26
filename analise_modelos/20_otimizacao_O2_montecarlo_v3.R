# 20_otimizacao_O2_montecarlo_v3.R - Monte Carlo O2 com amostragem estratégica (CORRIGIDO)

caminho_base <- "~/GitHub/abi-usa-stores"

source(file.path(caminho_base, "scripts", "utils.R"))
source(file.path(caminho_base, "scripts", "optimization.R"))
source(file.path(caminho_base, "eval_plan_O1.R"))

# ============================================================
# PARÂMETROS
# ============================================================

MAX_J <- 20
MAX_X <- 15
MAX_PR <- 0.30        # Máximo 30% (NÃO ALTERAR!)
MAX_UNIDADES <- 10000

lower <- rep(0, 84)
upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))

N_SAMPLES <- 5000     # 5.000 amostras

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
      PR <- min(MAX_PR, max(0, sol[i, d, 3]))  # GARANTE PR ≤ 30%
      
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
# FUNÇÃO DE AVALIAÇÃO O2 (DEATH PENALTY -Inf)
# ============================================================

eval_function_O2 <- function(sol) {
  
  if(is.matrix(sol)) sol <- as.numeric(sol)
  
  total_units <- calcular_unidades_totais(sol)
  
  # DEATH PENALTY - solução inválida = -Inf
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
# FUNÇÃO PARA GERAR AMOSTRAS COM LIMITES CORRETOS
# ============================================================

gerar_amostra_estrategica <- function(tipo = "baixo_RH") {
  
  sol <- numeric(84)
  idx <- 1
  
  for(loja in 1:4) {
    for(dia in 1:7) {
      
      if(tipo == "baixo_RH") {
        # RH muito baixo (0-5 Juniors, 0-3 Experts)
        J <- sample(0:5, 1)
        X <- sample(0:3, 1)
        PR <- runif(1, 0, MAX_PR)  # PR entre 0 e 30%
      } else if(tipo == "medio_RH") {
        # RH médio (3-10 Juniors, 2-6 Experts)
        J <- sample(3:10, 1)
        X <- sample(2:6, 1)
        PR <- runif(1, 0, MAX_PR)  # PR entre 0 e 30%
      } else {
        # RH normal (0-20 Juniors, 0-15 Experts)
        J <- sample(0:MAX_J, 1)
        X <- sample(0:MAX_X, 1)
        PR <- runif(1, 0, MAX_PR)  # PR entre 0 e 30%
      }
      
      # GARANTIR LIMITES (segurança extra)
      J <- min(J, MAX_J)
      X <- min(X, MAX_X)
      PR <- min(PR, MAX_PR)
      
      sol[idx] <- J
      idx <- idx + 1
      sol[idx] <- X
      idx <- idx + 1
      sol[idx] <- PR
      idx <- idx + 1
    }
  }
  
  return(sol)
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
      PR <- min(MAX_PR, max(0, sol[s, d, 3]))  # GARANTE PR ≤ 30%
      
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
      PR <- min(MAX_PR, max(0, sol[i, d, 3]))  # GARANTE PR ≤ 30%
      
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
# OTIMIZAÇÃO MONTE CARLO COM AMOSTRAGEM ESTRATÉGICA
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 MONTE CARLO O2 - HARD CONSTRAINT (10.000 unidades)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

cat("\nEstratégia de amostragem:\n")
cat("   - 50% amostras de BAIXO RH (J≤5, X≤3)\n")
cat("   - 30% amostras de RH MÉDIO (J≤10, X≤6)\n")
cat("   - 20% amostras aleatórias normais\n")
cat("   - Death penalty: -Inf\n")
cat("   - PR sempre entre 0% e 30% (garantido)\n")

melhor_valor <- -Inf
melhor_solucao <- NULL
melhor_unidades <- 0

n_baixo <- round(N_SAMPLES * 0.5)
n_medio <- round(N_SAMPLES * 0.3)
n_normal <- N_SAMPLES - n_baixo - n_medio

cat("\n🔍 A testar", N_SAMPLES, "soluções...\n")

# Barra de progresso
pb <- txtProgressBar(min = 0, max = N_SAMPLES, style = 3)

# Amostras de baixo RH
for(i in 1:n_baixo) {
  sol <- gerar_amostra_estrategica("baixo_RH")
  valor <- eval_function_O2(sol)
  
  if(is.finite(valor) && valor > melhor_valor) {
    melhor_valor <- valor
    melhor_solucao <- sol
    melhor_unidades <- calcular_unidades_totais(sol)
  }
  setTxtProgressBar(pb, i)
}

# Amostras de RH médio
for(i in 1:n_medio) {
  sol <- gerar_amostra_estrategica("medio_RH")
  valor <- eval_function_O2(sol)
  
  if(is.finite(valor) && valor > melhor_valor) {
    melhor_valor <- valor
    melhor_solucao <- sol
    melhor_unidades <- calcular_unidades_totais(sol)
  }
  setTxtProgressBar(pb, n_baixo + i)
}

# Amostras normais
for(i in 1:n_normal) {
  sol <- gerar_amostra_estrategica("normal")
  valor <- eval_function_O2(sol)
  
  if(is.finite(valor) && valor > melhor_valor) {
    melhor_valor <- valor
    melhor_solucao <- sol
    melhor_unidades <- calcular_unidades_totais(sol)
  }
  setTxtProgressBar(pb, n_baixo + n_medio + i)
}

close(pb)

# ============================================================
# RESULTADOS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 RESULTADOS DO MONTE CARLO - O2\n")
cat(paste(rep("=", 70), collapse=""), "\n")

if(is.finite(melhor_valor) && melhor_valor > -1e8) {
  cat("\n💰 Melhor lucro encontrado: $", round(melhor_valor, 2), "\n")
  cat("📦 Unidades totais:", round(melhor_unidades, 0), "/", MAX_UNIDADES, "\n")
  
  if(melhor_unidades <= MAX_UNIDADES) {
    cat("✅ SOLUÇÃO VÁLIDA encontrada!\n")
  } else {
    cat("❌ Solução inválida - ultrapassou limite\n")
  }
} else {
  cat("\n❌ NENHUMA solução válida encontrada!\n")
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
    if(length(pred_value) > 0) {
      forecasts <- rbind(forecasts, data.frame(
        Store = store, Week_ID = 21, Day = d, Forecast = pred_value
      ))
    }
  }
}

# ============================================================
# TABELA DETALHADA (SE EXISTIR SOLUÇÃO VÁLIDA)
# ============================================================

if(is.finite(melhor_valor) && melhor_valor > -1e8 && melhor_unidades <= MAX_UNIDADES) {
  
  resultado_lucro <- calcular_lucro_por_dia(melhor_solucao, forecasts, 21)
  resultado_unidades <- calcular_unidades_por_dia(melhor_solucao, 21)
  dim(melhor_solucao) <- c(4, 7, 3)
  
  dias <- c("Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado", "Domingo")
  
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("📋 PLANO OTIMIZADO O2 (MONTE CARLO)\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  
  for(i in 1:4) {
    cat("\n", paste(rep("-", 60), collapse=""), "\n")
    cat("📍 LOJA:", toupper(lojas[i]), "\n")
    cat(paste(rep("-", 60), collapse=""), "\n")
    
    cat("\n   Dia       | J | X | PR | Previsão | Unidades | Lucro Dia\n")
    cat("   ----------|---|---|----|----------|----------|----------\n")
    
    for(d in 1:7) {
      J <- round(melhor_solucao[i, d, 1])
      X <- round(melhor_solucao[i, d, 2])
      PR_raw <- melhor_solucao[i, d, 3]
      PR <- round(PR_raw * 100, 0)
      PR <- min(PR, 30)  # GARANTE QUE PR NÃO ULTRAPASSA 30 NA TABELA
      
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
  cat("📈 RESUMO FINAL DA SEMANA 21 (O2 - MONTE CARLO)\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  
  cat("\n💰 Lucro TOTAL das 4 lojas: $", round(resultado_lucro$lucro_total, 2), "\n")
  cat("📦 Unidades TOTAL das 4 lojas:", round(melhor_unidades, 0), "/", MAX_UNIDADES, "\n")
  
  if(melhor_unidades <= MAX_UNIDADES) {
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

resultados_path <- file.path(caminho_base, "analise_modelos", "resultados_MC_O2")
if(!dir.exists(resultados_path)) dir.create(resultados_path)

resultado <- list(
  melhor_lucro = melhor_valor,
  melhor_unidades = melhor_unidades,
  melhor_solucao = melhor_solucao,
  n_amostras = N_SAMPLES
)

saveRDS(resultado, file.path(resultados_path, "resultado_montecarlo_O2.rds"))

if(exists("resultado_lucro")) {
  saveRDS(resultado_lucro, file.path(resultados_path, "lucro_por_dia.rds"))
}
if(exists("resultado_unidades")) {
  saveRDS(resultado_unidades, file.path(resultados_path, "unidades_por_dia.rds"))
}

cat("\n✅ Resultados guardados em:", resultados_path, "\n")