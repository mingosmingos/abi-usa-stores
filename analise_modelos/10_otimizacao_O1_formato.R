# 10_otimizacao_O1_formato.R - Otimização O1 (mesmo formato dos outros)

source("scripts/utils.R")
dados <- carregar_dados_lojas("dados/")
source("scripts/optimization.R")
source("scripts/forecasting.R")

# ============================================================
# PARÂMETROS DA OTIMIZAÇÃO
# ============================================================

valores_J <- 0:20
valores_X <- 0:15
valores_PR <- seq(0, 0.3, by = 0.02)

# ============================================================
# FUNÇÃO PARA CALCULAR UNIDADES VENDIDAS
# ============================================================

calcular_unidades_dia <- function(J, X, PR, clientes_previstos, loja) {
  
  loja_params <- parametros_lojas[[loja]]
  
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
  
  unidades_X <- round(loja_params$Fx * 10 / log(2 - PR))
  unidades_J <- round(loja_params$Fj * 10 / log(2 - PR))
  
  total_unidades <- (unidades_X * clientes_X) + (unidades_J * clientes_J)
  
  return(total_unidades)
}

# ============================================================
# FUNÇÃO DE OTIMIZAÇÃO PARA O1
# ============================================================

otimizar_O1 <- function(previsoes, datas, loja) {
  
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
          
          lucro <- calcular_lucro_diario(J, X, PR, previsoes[dia], datas[dia], loja)
          
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
# EXECUTAR OTIMIZAÇÃO O1
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("OTIMIZAÇÃO O1 - ARIMAX (Maximizar Lucro)\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

resultados_O1 <- list()

for(loja in names(dados)) {
  
  cat("\n📍 LOJA:", toupper(loja), "\n")
  
  previsoes <- resultados_arimax[[loja]]$previsoes[1:7]
  
  plano <- otimizar_O1(previsoes, datas_futuras, loja)
  
  cat("\n   📊 RESULTADOS O1:\n")
  cat("      Lucro semanal: $", round(plano$lucro_semanal, 2), "\n", sep="")
  cat("      Unidades totais:", format(round(plano$unidades_totais, 2), big.mark = ","), "\n")
  cat("      Total funcionários:", sum(plano$J) + sum(plano$X), "\n")
  
  resultados_O1[[loja]] <- plano
}

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

saveRDS(resultados_O1, "analise_modelos/resultados_otimizacao_O1.rds")

cat("\n✅ Resultados guardados em 'analise_modelos/resultados_otimizacao_O1.rds'\n")