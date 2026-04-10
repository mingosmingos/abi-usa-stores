# 05_arimax_v2.R - ARIMAX completo (guarda previsoes e reais)

# Encontrar a raiz do projeto
if(basename(getwd()) == "analise_modelos") {
  setwd("..")
}
source("scripts/utils.R")
dados <- carregar_dados_lojas("dados/")

library(forecast)

# ============================================================
# FUNÇÃO PARA ANALISAR ARIMAX
# ============================================================

analisar_arimax <- function(dados_loja, nome_loja) {
  
  cat("\n", paste(rep("=", 50), collapse=""), "\n")
  cat("ARIMAX - LOJA:", toupper(nome_loja), "\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  # Dividir em treino (80%) e teste (20%)
  n <- nrow(dados_loja)
  treino <- dados_loja[1:floor(n*0.8), ]
  teste <- dados_loja[(floor(n*0.8)+1):n, ]
  
  # Série temporal
  serie_treino <- ts(treino$Num_Customers, frequency = 7)
  
  # Variáveis exógenas (como matriz)
  xreg_treino <- as.matrix(treino[, c("Num_Employees", "Pct_On_Sale", "TouristEvent_bin")])
  xreg_teste <- as.matrix(teste[, c("Num_Employees", "Pct_On_Sale", "TouristEvent_bin")])
  
  # ARIMAX
  modelo <- auto.arima(serie_treino, 
                       xreg = xreg_treino,
                       seasonal = TRUE)
  
  cat("\n📈 Modelo escolhido:\n")
  print(modelo)
  
  # MOSTRAR COEFICIENTES
  cat("\n📊 Coeficientes:\n")
  coefs <- coef(modelo)
  print(round(coefs, 4))
  
  # ============================================================
  # PREVISÃO PARA TODO O PERÍODO DE TESTE
  # ============================================================
  
  # Fazer previsão para todos os dias de teste
  previsao <- forecast(modelo, xreg = xreg_teste, h = nrow(teste))
  
  # Valores reais e previstos (TODOS)
  valores_reais <- teste$Num_Customers
  valores_previstos <- as.numeric(previsao$mean)
  
  # Verificar se há NAs
  if(any(is.na(valores_previstos))) {
    cat("\n⚠️ Aviso: Previsões com NA. Usando apenas dias válidos.\n")
    validos <- !is.na(valores_previstos)
    valores_reais <- valores_reais[validos]
    valores_previstos <- valores_previstos[validos]
  }
  
  # Métricas (usando TODOS os dias)
  mae <- mean(abs(valores_previstos - valores_reais))
  rmse <- sqrt(mean((valores_previstos - valores_reais)^2))
  
  cat("\n📏 MÉTRICAS (", length(valores_reais), "dias):\n")
  cat("MAE:", round(mae, 2), "clientes\n")
  cat("RMSE:", round(rmse, 2), "clientes\n")
  
  # ============================================================
  # RESULTADOS (COM PREVISOES E REAIS)
  # ============================================================
  
  resultados <- list(
    loja = nome_loja,
    mae = mae,
    rmse = rmse,
    modelo = modelo,
    coeficientes = coefs,
    previsoes = valores_previstos,  # <--- GUARDA
    reais = valores_reais           # <--- GUARDA
  )
  
  return(resultados)
}

# ============================================================
# CORRER PARA TODAS AS LOJAS
# ============================================================

resultados_arimax <- list()

for(loja in names(dados)) {
  resultados_arimax[[loja]] <- analisar_arimax(dados[[loja]], loja)
}

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

saveRDS(resultados_arimax, "analise_modelos/resultados_arimax.rds")

# ============================================================
# RESUMO
# ============================================================

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("RESUMO ARIMAX\n")
cat(paste(rep("=", 50), collapse=""), "\n")

for(loja in names(resultados_arimax)) {
  cat(loja, ": MAE =", round(resultados_arimax[[loja]]$mae, 2), "clientes\n")
}

cat("\n✅ Resultados guardados em 'analise_modelos/resultados_arimax.rds'\n")
cat("✅ Agora com previsoes e reais guardados!\n")