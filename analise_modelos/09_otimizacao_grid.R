# 09_otimizacao_grid.R - Otimização com Grid Search para ARIMA/ARIMAX

source("scripts/utils.R")
dados <- carregar_dados_lojas("dados/")
source("scripts/optimization.R")
source("scripts/forecasting.R")

library(ggplot2)

# ============================================================
# PARÂMETROS DA OTIMIZAÇÃO
# ============================================================

# Grid com passos mais pequenos (mais preciso)
valores_J <- 0:20                    # 0 a 20 (21 valores)
valores_X <- 0:15                    # 0 a 15 (16 valores)
valores_PR <- seq(0, 0.3, by = 0.02) # 0% a 30% em passos de 2% (16 valores)

# Total de combinações por dia: 7 × 7 × 7 = 343 combinações
# Para 7 dias: muito pesado! Vamos otimizar 1 dia de cada vez

# ============================================================
# FUNÇÃO PARA OTIMIZAR 1 DIA COM GRID SEARCH
# ============================================================

otimizar_dia_grid <- function(previsao_cliente, data, loja, objetivo = "O1") {
  
  loja_params <- parametros_lojas[[loja]]
  weekend <- is_weekend(data)
  
  melhor_lucro <- -Inf
  melhor_J <- 0
  melhor_X <- 0
  melhor_PR <- 0
  
  cat("   A testar", length(valores_J) * length(valores_X) * length(valores_PR), "combinações...\n")
  
  for(J in valores_J) {
    for(X in valores_X) {
      for(PR in valores_PR) {
        
        # Calcular lucro para esta combinação
        lucro <- calcular_lucro_diario(J, X, PR, previsao_cliente, data, loja)
        
        if(lucro > melhor_lucro) {
          melhor_lucro <- lucro
          melhor_J <- J
          melhor_X <- X
          melhor_PR <- PR
        }
      }
    }
  }
  
  return(list(
    J = melhor_J,
    X = melhor_X,
    PR = melhor_PR,
    lucro = melhor_lucro
  ))
}

# ============================================================
# OTIMIZAR PARA ARIMA (usando previsões do ARIMA)
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("OTIMIZAÇÃO COM GRID SEARCH - ARIMA\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Carregar previsões do ARIMA
resultados_arima <- readRDS("analise_modelos/resultados_arima.rds")

# Datas para a próxima semana (simulação)
datas_futuras <- seq.Date(Sys.Date(), by = "day", length.out = 7)

for(loja in names(dados)) {
  cat("\n📍 LOJA:", toupper(loja), "\n")
  
  # Previsões ARIMA para esta loja
  previsoes <- resultados_arima[[loja]]$previsoes[1:7]
  
  plano <- list(J = numeric(7), X = numeric(7), PR = numeric(7), lucro = numeric(7))
  
  for(dia in 1:7) {
    cat("   Dia", dia, ":", sep="")
    resultado <- otimizar_dia_grid(previsoes[dia], datas_futuras[dia], loja)
    plano$J[dia] <- resultado$J
    plano$X[dia] <- resultado$X
    plano$PR[dia] <- resultado$PR
    plano$lucro[dia] <- resultado$lucro
    cat(" J=", resultado$J, " X=", resultado$X, " PR=", round(resultado$PR*100, 0), "%\n", sep="")
  }
  
  # Calcular lucro semanal
  lucro_total <- sum(plano$lucro) - parametros_lojas[[loja]]$Ws
  
  cat("\n   📊 RESUMO ARIMA -", toupper(loja), ":\n")
  cat("      Lucro semanal:", format_money(lucro_total), "\n")
  cat("      Lucro médio/dia:", format_money(mean(plano$lucro)), "\n")
  
  # Guardar plano
  saveRDS(plano, paste0("analise_modelos/plano_arima_grid_", loja, ".rds"))
}

# ============================================================
# OTIMIZAR PARA ARIMAX (usando previsões do ARIMAX)
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("OTIMIZAÇÃO COM GRID SEARCH - ARIMAX\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Carregar previsões do ARIMAX
resultados_arimax <- readRDS("analise_modelos/resultados_arimax.rds")

for(loja in names(dados)) {
  cat("\n📍 LOJA:", toupper(loja), "\n")
  
  # Previsões ARIMAX para esta loja
  previsoes <- resultados_arimax[[loja]]$previsoes[1:7]
  
  plano <- list(J = numeric(7), X = numeric(7), PR = numeric(7), lucro = numeric(7))
  
  for(dia in 1:7) {
    cat("   Dia", dia, ":", sep="")
    resultado <- otimizar_dia_grid(previsoes[dia], datas_futuras[dia], loja)
    plano$J[dia] <- resultado$J
    plano$X[dia] <- resultado$X
    plano$PR[dia] <- resultado$PR
    plano$lucro[dia] <- resultado$lucro
    cat(" J=", resultado$J, " X=", resultado$X, " PR=", round(resultado$PR*100, 0), "%\n", sep="")
  }
  
  # Calcular lucro semanal
  lucro_total <- sum(plano$lucro) - parametros_lojas[[loja]]$Ws
  
  cat("\n   📊 RESUMO ARIMAX -", toupper(loja), ":\n")
  cat("      Lucro semanal:", format_money(lucro_total), "\n")
  cat("      Lucro médio/dia:", format_money(mean(plano$lucro)), "\n")
  
  # Guardar plano
  saveRDS(plano, paste0("analise_modelos/plano_arimax_grid_", loja, ".rds"))
}

# ============================================================
# COMPARAÇÃO DOS PLANOS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("COMPARAÇÃO DOS PLANOS OTIMIZADOS\n")
cat(paste(rep("=", 70), collapse=""), "\n")

comparacao_planos <- data.frame(
  Loja = names(dados),
  Lucro_ARIMA = NA,
  Lucro_ARIMAX = NA,
  Diferenca = NA
)

for(loja in names(dados)) {
  plano_arima <- readRDS(paste0("analise_modelos/plano_arima_grid_", loja, ".rds"))
  plano_arimax <- readRDS(paste0("analise_modelos/plano_arimax_grid_", loja, ".rds"))
  
  lucro_arima <- sum(plano_arima$lucro) - parametros_lojas[[loja]]$Ws
  lucro_arimax <- sum(plano_arimax$lucro) - parametros_lojas[[loja]]$Ws
  
  comparacao_planos[comparacao_planos$Loja == loja, "Lucro_ARIMA"] <- round(lucro_arima, 2)
  comparacao_planos[comparacao_planos$Loja == loja, "Lucro_ARIMAX"] <- round(lucro_arimax, 2)
  comparacao_planos[comparacao_planos$Loja == loja, "Diferenca"] <- round(lucro_arimax - lucro_arima, 2)
}

print(comparacao_planos)

# ============================================================
# GRÁFICO COMPARATIVO
# ============================================================

df_plot <- data.frame(
  Loja = rep(comparacao_planos$Loja, 2),
  Modelo = c(rep("ARIMA", 4), rep("ARIMAX", 4)),
  Lucro = c(comparacao_planos$Lucro_ARIMA, comparacao_planos$Lucro_ARIMAX)
)

p <- ggplot(df_plot, aes(x = Loja, y = Lucro, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparação de Lucro Otimizado - ARIMA vs ARIMAX",
       x = "Loja", y = "Lucro Semanal ($)") +
  theme_minimal() +
  scale_fill_manual(values = c("ARIMA" = "red", "ARIMAX" = "blue")) +
  geom_text(aes(label = scales::dollar(Lucro)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)

print(p)

ggsave("analise_modelos/otimizacao_grid_comparacao.png", p, width = 10, height = 6)

cat("\n✅ Resultados guardados:\n")
cat("   - plano_arima_grid_*.rds\n")
cat("   - plano_arimax_grid_*.rds\n")
cat("   - otimizacao_grid_comparacao.png\n")