# comparacao_arima_arimax.R - Com MAE, NMAE, RMSE, NRMSE e R²

source("scripts/utils.R")
dados <- carregar_dados_lojas("dados/")

# Carregar resultados
resultados_arima <- readRDS("analise_modelos/resultados_arima.rds")
resultados_arimax <- readRDS("analise_modelos/resultados_arimax.rds")

# ============================================================
# FUNÇÃO PARA CALCULAR R²
# ============================================================

calcular_r2 <- function(reais, previstos) {
  ss_res <- sum((reais - previstos)^2)
  ss_tot <- sum((reais - mean(reais))^2)
  r2 <- 1 - (ss_res / ss_tot)
  return(r2)
}

# ============================================================
# CALCULAR MÉTRICAS PARA CADA LOJA
# ============================================================

resultados <- data.frame(
  Loja = names(resultados_arima),
  
  # ARIMA
  ARIMA_MAE = NA,
  ARIMA_NMAE = NA,
  ARIMA_RMSE = NA,
  ARIMA_NRMSE = NA,
  ARIMA_R2 = NA,
  
  # ARIMAX
  ARIMAX_MAE = NA,
  ARIMAX_NMAE = NA,
  ARIMAX_RMSE = NA,
  ARIMAX_NRMSE = NA,
  ARIMAX_R2 = NA
)

for(loja in names(dados)) {
  
  # Dados da loja (para amplitude)
  dados_loja <- dados[[loja]]
  y_min <- min(dados_loja$Num_Customers, na.rm = TRUE)
  y_max <- max(dados_loja$Num_Customers, na.rm = TRUE)
  amplitude <- y_max - y_min
  
  # ====================
  # ARIMA
  # ====================
  mae_arima <- resultados_arima[[loja]]$mae
  rmse_arima <- resultados_arima[[loja]]$rmse
  
  reais_arima <- resultados_arima[[loja]]$reais
  prev_arima <- resultados_arima[[loja]]$previsoes
  
  if(is.list(reais_arima)) reais_arima <- unlist(reais_arima)
  if(is.list(prev_arima)) prev_arima <- unlist(prev_arima)
  
  reais_arima <- as.numeric(reais_arima)
  prev_arima <- as.numeric(prev_arima)
  
  r2_arima <- calcular_r2(reais_arima, prev_arima)
  
  # ====================
  # ARIMAX
  # ====================
  mae_arimax <- resultados_arimax[[loja]]$mae
  rmse_arimax <- resultados_arimax[[loja]]$rmse
  
  reais_arimax <- resultados_arimax[[loja]]$reais
  prev_arimax <- resultados_arimax[[loja]]$previsoes
  
  if(is.list(reais_arimax)) reais_arimax <- unlist(reais_arimax)
  if(is.list(prev_arimax)) prev_arimax <- unlist(prev_arimax)
  
  reais_arimax <- as.numeric(reais_arimax)
  prev_arimax <- as.numeric(prev_arimax)
  
  r2_arimax <- calcular_r2(reais_arimax, prev_arimax)
  
  # ====================
  # GUARDAR
  # ====================
  # ARIMA
  resultados[resultados$Loja == loja, "ARIMA_MAE"] <- round(mae_arima, 2)
  resultados[resultados$Loja == loja, "ARIMA_NMAE"] <- round(mae_arima / amplitude * 100, 2)
  resultados[resultados$Loja == loja, "ARIMA_RMSE"] <- round(rmse_arima, 2)
  resultados[resultados$Loja == loja, "ARIMA_NRMSE"] <- round(rmse_arima / amplitude * 100, 2)
  resultados[resultados$Loja == loja, "ARIMA_R2"] <- round(r2_arima, 4)
  
  # ARIMAX
  resultados[resultados$Loja == loja, "ARIMAX_MAE"] <- round(mae_arimax, 2)
  resultados[resultados$Loja == loja, "ARIMAX_NMAE"] <- round(mae_arimax / amplitude * 100, 2)
  resultados[resultados$Loja == loja, "ARIMAX_RMSE"] <- round(rmse_arimax, 2)
  resultados[resultados$Loja == loja, "ARIMAX_NRMSE"] <- round(rmse_arimax / amplitude * 100, 2)
  resultados[resultados$Loja == loja, "ARIMAX_R2"] <- round(r2_arimax, 4)
}

# ============================================================
# MOSTRAR RESULTADOS
# ============================================================

cat("\n", paste(rep("=", 100), collapse=""), "\n")
cat("COMPARAÇÃO ARIMA vs ARIMAX - MAE, NMAE, RMSE, NRMSE, R²\n")
cat(paste(rep("=", 100), collapse=""), "\n\n")

# Tabela MAE
cat("📊 MAE (Erro Absoluto Médio - clientes):\n")
print(resultados[, c("Loja", "ARIMA_MAE", "ARIMAX_MAE")])

cat("\n📊 NMAE (Erro Normalizado - % da amplitude):\n")
print(resultados[, c("Loja", "ARIMA_NMAE", "ARIMAX_NMAE")])

cat("\n📊 RMSE (Raiz do Erro Quadrático - clientes):\n")
print(resultados[, c("Loja", "ARIMA_RMSE", "ARIMAX_RMSE")])

cat("\n📊 NRMSE (RMSE Normalizado - % da amplitude):\n")
print(resultados[, c("Loja", "ARIMA_NRMSE", "ARIMAX_NRMSE")])

cat("\n📊 R² (Coeficiente de Determinação):\n")
print(resultados[, c("Loja", "ARIMA_R2", "ARIMAX_R2")])

# ============================================================
# MÉDIAS GLOBAIS
# ============================================================

cat("\n", paste(rep("=", 100), collapse=""), "\n")
cat("MÉDIAS GLOBAIS\n")
cat(paste(rep("=", 100), collapse=""), "\n")

cat("\n📊 MAE médio:\n")
cat("  ARIMA:  ", round(mean(resultados$ARIMA_MAE), 2), "clientes\n")
cat("  ARIMAX: ", round(mean(resultados$ARIMAX_MAE), 2), "clientes\n")

cat("\n📊 NMAE médio:\n")
cat("  ARIMA:  ", round(mean(resultados$ARIMA_NMAE), 2), "%\n")
cat("  ARIMAX: ", round(mean(resultados$ARIMAX_NMAE), 2), "%\n")

cat("\n📊 RMSE médio:\n")
cat("  ARIMA:  ", round(mean(resultados$ARIMA_RMSE), 2), "clientes\n")
cat("  ARIMAX: ", round(mean(resultados$ARIMAX_RMSE), 2), "clientes\n")

cat("\n📊 NRMSE médio:\n")
cat("  ARIMA:  ", round(mean(resultados$ARIMA_NRMSE), 2), "%\n")
cat("  ARIMAX: ", round(mean(resultados$ARIMAX_NRMSE), 2), "%\n")

cat("\n📊 R² médio:\n")
cat("  ARIMA:  ", round(mean(resultados$ARIMA_R2), 4), "\n")
cat("  ARIMAX: ", round(mean(resultados$ARIMAX_R2), 4), "\n")

# ============================================================
# INTERPRETAÇÃO
# ============================================================

cat("\n", paste(rep("=", 100), collapse=""), "\n")
cat("📈 INTERPRETAÇÃO DOS RESULTADOS\n")
cat(paste(rep("=", 100), collapse=""), "\n")

for(i in 1:nrow(resultados)) {
  loja <- resultados$Loja[i]
  mae_arima <- resultados$ARIMA_MAE[i]
  mae_arimax <- resultados$ARIMAX_MAE[i]
  rmse_arimax <- resultados$ARIMAX_RMSE[i]
  r2_arimax <- resultados$ARIMAX_R2[i]
  
  cat("\n📍", toupper(loja), ":\n")
  cat("   MAE:  ARIMA", mae_arima, "→ ARIMAX", mae_arimax, 
      paste0("(", round((mae_arima - mae_arimax)/mae_arima*100, 1), "% melhor)\n"))
  cat("   RMSE: ARIMAX =", rmse_arimax, "clientes\n")
  
  if(r2_arimax > 0.7) {
    cat("   ✅ R² =", r2_arimax, "- O modelo explica mais de 70% da variância\n")
  } else if(r2_arimax > 0.5) {
    cat("   📈 R² =", r2_arimax, "- O modelo explica mais de 50% da variância\n")
  } else if(r2_arimax > 0) {
    cat("   📉 R² =", r2_arimax, "- O modelo explica menos de 50% da variância\n")
  } else {
    cat("   ⚠️ R² negativo - O modelo é pior que a média simples\n")
  }
}

# ============================================================
# COMPARAÇÃO MAE vs RMSE
# ============================================================

cat("\n", paste(rep("=", 100), collapse=""), "\n")
cat("📊 ANÁLISE MAE vs RMSE\n")
cat(paste(rep("=", 100), collapse=""), "\n")

cat("\nA diferença entre RMSE e MAE indica a presença de erros grandes:\n\n")

for(i in 1:nrow(resultados)) {
  loja <- resultados$Loja[i]
  mae <- resultados$ARIMAX_MAE[i]
  rmse <- resultados$ARIMAX_RMSE[i]
  diferenca <- rmse - mae
  
  if(diferenca > 10) {
    cat("📍", toupper(loja), ": RMSE é", round(diferenca, 1), 
        "maior que MAE → existem alguns erros grandes\n")
  } else {
    cat("📍", toupper(loja), ": RMSE é", round(diferenca, 1), 
        "maior que MAE → erros são consistentes\n")
  }
}

# ============================================================
# GUARDAR
# ============================================================

saveRDS(resultados, "analise_modelos/comparacao_arima_arimax_metricas.rds")
write.csv(resultados, "analise_modelos/comparacao_arima_arimax_metricas.csv", row.names = FALSE)

cat("\n✅ Resultados guardados!\n")
cat("   - comparacao_arima_arimax_metricas.rds\n")
cat("   - comparacao_arima_arimax_metricas.csv\n")