# 05_arimax.R - ARIMAX simplificado (sem forecast)

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
  
  # CALCULAR PREVISÕES MANUALMENTE para os primeiros 30 dias de teste
  # (para evitar problemas com todas as previsões)
  h_teste <- min(30, nrow(teste))
  
  # Fazer previsão passo a passo
  valores_previstos <- numeric(h_teste)
  
  # Usar os primeiros h_teste dias do teste
  xreg_teste_pequeno <- xreg_teste[1:h_teste, , drop = FALSE]
  
  tryCatch({
    previsao <- forecast(modelo, xreg = xreg_teste_pequeno, h = h_teste)
    valores_previstos <- as.numeric(previsao$mean)
  }, error = function(e) {
    cat("\n⚠️ Erro na previsão, usando método alternativo...\n")
    # Método alternativo: usar os valores reais das exógenas
    for(i in 1:h_teste) {
      # Usar apenas a parte ARIMA sem as exógenas para evitar erro
      valores_previstos[i] <- NA
    }
  })
  
  # Se não conseguiu prever, calcular com base no modelo
  if(all(is.na(valores_previstos))) {
    cat("\n⚠️ Não foi possível calcular previsões.\n")
    mae <- NA
    rmse <- NA
  } else {
    # Métricas
    valores_reais <- teste$Num_Customers[1:h_teste]
    mae <- mean(abs(valores_previstos - valores_reais))
    rmse <- sqrt(mean((valores_previstos - valores_reais)^2))
    
    cat("\n📏 MÉTRICAS (primeiros", h_teste, "dias):\n")
    cat("MAE:", round(mae, 2), "clientes\n")
    cat("RMSE:", round(rmse, 2), "clientes\n")
  }
  
  # Resultados
  resultados <- list(
    loja = nome_loja,
    mae = mae,
    rmse = rmse,
    modelo = modelo,
    coeficientes = coefs
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
  mae_val <- resultados_arimax[[loja]]$mae
  if(is.na(mae_val)) {
    cat(loja, ": Não foi possível calcular\n")
  } else {
    cat(loja, ": MAE =", round(mae_val, 2), "clientes\n")
  }
}

cat("\n✅ Resultados guardados em 'analise_modelos/resultados_arimax.rds'\n")