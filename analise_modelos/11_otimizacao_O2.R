# 11_otimizacao_O2.R - Otimização O2 (Maximizar Lucro + Limite 10.000 unidades)

source("scripts/utils.R")
dados <- carregar_dados_lojas("dados/")
source("scripts/optimization.R")
source("scripts/forecasting.R")

library(ggplot2)

# ============================================================
# PARÂMETROS DA OTIMIZAÇÃO
# ============================================================

valores_J <- 0:20                    # 0 a 20 (21 valores)
valores_X <- 0:15                    # 0 a 15 (16 valores)
valores_PR <- seq(0, 0.3, by = 0.02) # 0% a 30% em passos de 2% (16 valores)

MAX_UNIDADES <- 10000  # Limite máximo de unidades

# ============================================================
# FUNÇÃO PARA CALCULAR UNIDADES VENDIDAS
# ============================================================

calcular_unidades_dia <- function(J, X, PR, clientes_previstos, loja) {
  
  loja_params <- parametros_lojas[[loja]]
  
  # Clientes assistidos
  capacidade_X <- 7 * X
  capacidade_J <- 6 * J
  
  if(capacidade_X >= clientes_previstos) {
    clientes_atendidos <- clientes_previstos
    clientes_X <- clientes_previstos
    clientes_J <- 0
  } else {
    clientes_atendidos <- capacidade_X + min(capacidade_J, clientes_previstos - capacidade_X)
    clientes_X <- capacidade_X
    clientes_J <- clientes_atendidos - capacidade_X
  }
  
  # Unidades por cliente
  unidades_X <- round(loja_params$Fx * 10 / log(2 - PR))
  unidades_J <- round(loja_params$Fj * 10 / log(2 - PR))
  
  # Total unidades
  total_unidades <- (unidades_X * clientes_X) + (unidades_J * clientes_J)
  
  return(total_unidades)
}

# ============================================================
# FUNÇÃO DE OTIMIZAÇÃO PARA O2
# ============================================================

otimizar_O2 <- function(previsoes, datas, loja) {
  
  n_dias <- length(previsoes)
  melhor_J <- rep(0, n_dias)
  melhor_X <- rep(0, n_dias)
  melhor_PR <- rep(0, n_dias)
  melhor_lucro <- 0
  
  cat("   A testar combinações para", n_dias, "dias...\n")
  
  for(dia in 1:n_dias) {
    
    melhor_lucro_dia <- -Inf
    melhor_J_dia <- 0
    melhor_X_dia <- 0
    melhor_PR_dia <- 0
    
    for(J in valores_J) {
      for(X in valores_X) {
        for(PR in valores_PR) {
          
          # Calcular lucro do dia
          lucro <- calcular_lucro_diario(J, X, PR, previsoes[dia], datas[dia], loja)
          
          # Calcular unidades do dia
          unidades <- calcular_unidades_dia(J, X, PR, previsoes[dia], loja)
          
          # O2: Penalizar se ultrapassar limite de unidades
          if(unidades > MAX_UNIDADES) {
            lucro <- lucro - 1000 * (unidades - MAX_UNIDADES)
          }
          
          if(lucro > melhor_lucro_dia) {
            melhor_lucro_dia <- lucro
            melhor_J_dia <- J
            melhor_X_dia <- X
            melhor_PR_dia <- PR
          }
        }
      }
    }
    
    melhor_J[dia] <- melhor_J_dia
    melhor_X[dia] <- melhor_X_dia
    melhor_PR[dia] <- melhor_PR_dia
    melhor_lucro <- melhor_lucro + melhor_lucro_dia
    
    cat("   Dia", dia, ": J=", melhor_J_dia, "X=", melhor_X_dia, 
        "PR=", round(melhor_PR_dia*100, 0), "%\n", sep="")
  }
  
  lucro_semanal <- melhor_lucro - parametros_lojas[[loja]]$Ws
  
  # Calcular unidades totais
  unidades_totais <- 0
  for(dia in 1:n_dias) {
    unidades_totais <- unidades_totais + calcular_unidades_dia(melhor_J[dia], melhor_X[dia], 
                                                               melhor_PR[dia], previsoes[dia], loja)
  }
  
  return(list(
    J = melhor_J,
    X = melhor_X,
    PR = melhor_PR,
    lucro_semanal = lucro_semanal,
    unidades_totais = unidades_totais
  ))
}

# ============================================================
# CARREGAR PREVISÕES DO ARIMAX
# ============================================================

resultados_arimax <- readRDS("analise_modelos/resultados_arimax.rds")
datas_futuras <- seq.Date(Sys.Date(), by = "day", length.out = 7)

# ============================================================
# EXECUTAR OTIMIZAÇÃO O2
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("OTIMIZAÇÃO O2 - ARIMAX (Limite de 10.000 unidades)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

resultados_O2 <- list()

for(loja in names(dados)) {
  
  cat("\n📍 LOJA:", toupper(loja), "\n")
  
  previsoes <- resultados_arimax[[loja]]$previsoes[1:7]
  
  plano <- otimizar_O2(previsoes, datas_futuras, loja)
  
  cat("\n   📊 RESULTADOS O2:\n")
  cat("      Lucro semanal: $", round(plano$lucro_semanal, 2), "\n", sep="")
  cat("      Unidades totais:", format(plano$unidades_totais, big.mark = ","), "\n")
  cat("      Total funcionários:", sum(plano$J) + sum(plano$X), "\n")
  
  resultados_O2[[loja]] <- plano
}

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

saveRDS(resultados_O2, "analise_modelos/resultados_otimizacao_O2.rds")

cat("\n✅ Resultados guardados em 'analise_modelos/resultados_otimizacao_O2.rds'\n")