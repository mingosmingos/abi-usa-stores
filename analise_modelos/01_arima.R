source("scripts/utils.R")

library(forecast)
library(ggplot2)

# Carregar dados (caminho: sobe um nível e entra em dados/)
dados <- carregar_dados_lojas("dados/")

# Função para analisar ARIMA numa loja
analisar_arima <- function(dados_loja, nome_loja) {
  
  cat("\n", paste(rep("=", 50), collapse=""), "\n")
  cat("ANÁLISE ARIMA - LOJA:", toupper(nome_loja), "\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  # 1. Estatísticas descritivas
  cat("\n📊 ESTATÍSTICAS DESCRITIVAS:\n")
  cat("Média clientes:", round(mean(dados_loja$Num_Customers), 2), "\n")
  cat("Desvio padrão:", round(sd(dados_loja$Num_Customers), 2), "\n")
  cat("Mínimo:", min(dados_loja$Num_Customers), "\n")
  cat("Máximo:", max(dados_loja$Num_Customers), "\n")
  
  # 2. Dividir em treino (80%) e teste (20%)
  n <- nrow(dados_loja)
  treino <- dados_loja[1:floor(n*0.8), ]
  teste <- dados_loja[(floor(n*0.8)+1):n, ]
  
  cat("\n📅 DIVISÃO DOS DADOS:\n")
  cat("Treino:", nrow(treino), "dias (", min(treino$Date), "a", max(treino$Date), ")\n")
  cat("Teste:", nrow(teste), "dias (", min(teste$Date), "a", max(teste$Date), ")\n")
  
  # 3. Criar série temporal
  serie_treino <- ts(treino$Num_Customers, frequency = 7)
  
  # 4. Auto ARIMA
  cat("\n🤖 A ajustar modelo ARIMA...\n")
  modelo <- auto.arima(serie_treino, 
                       seasonal = TRUE,
                       stepwise = FALSE,
                       approximation = FALSE)
  
  cat("\n📈 MODELO ESCOLHIDO:\n")
  print(modelo)
  
  cat("\n📊 COEFICIENTES:\n")
  print(coef(modelo))
  
  # 5. Diagnóstico dos resíduos
  cat("\n🔍 DIAGNÓSTICO DOS RESÍDUOS:\n")
  checkresiduals(modelo)
  
  # 6. Previsões para o período de teste
  previsoes <- forecast(modelo, h = nrow(teste))
  
  # 7. Calcular métricas
  valores_reais <- teste$Num_Customers
  valores_previstos <- as.numeric(previsoes$mean)
  
  mae <- mean(abs(valores_previstos - valores_reais))
  rmse <- sqrt(mean((valores_previstos - valores_reais)^2))
  mape <- mean(abs((valores_previstos - valores_reais) / valores_reais)) * 100
  
  cat("\n📏 MÉTRICAS DE ERRO:\n")
  cat("MAE  (Erro Absoluto Médio):", round(mae, 2), "clientes\n")
  cat("RMSE (Raiz do Erro Quadrático):", round(rmse, 2), "clientes\n")
  cat("MAPE (Erro Percentual):", round(mape, 2), "%\n")
  
  # 8. Gráfico
  df_plot <- data.frame(
    Data = teste$Date,
    Real = valores_reais,
    Previsto = valores_previstos
  )
  
  p <- ggplot(df_plot, aes(x = Data)) +
    geom_line(aes(y = Real, color = "Real"), size = 1) +
    geom_line(aes(y = Previsto, color = "Previsto"), size = 1, linetype = "dashed") +
    labs(title = paste("ARIMA -", nome_loja),
         x = "Data", y = "Clientes",
         color = "Legenda") +
    theme_minimal() +
    scale_color_manual(values = c("Real" = "black", "Previsto" = "red"))
  
  print(p)
  
  # Guardar resultados
  resultados <- list(
    loja = nome_loja,
    modelo = modelo,
    mae = mae,
    rmse = rmse,
    mape = mape,
    previsoes = valores_previstos,
    reais = valores_reais,
    grafico = p
  )
  
  return(resultados)
}

# Analisar todas as lojas
resultados <- list()
for(loja in names(dados)) {
  resultados[[loja]] <- analisar_arima(dados[[loja]], loja)
}

# Resumo final
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("RESUMO COMPARATIVO - ARIMA\n")
cat(paste(rep("=", 50), collapse=""), "\n")

resumo <- data.frame(
  Loja = names(resultados),
  MAE = sapply(resultados, function(x) round(x$mae, 2)),
  RMSE = sapply(resultados, function(x) round(x$rmse, 2)),
  MAPE = sapply(resultados, function(x) round(x$mape, 2))
)
print(resumo)

# Salvar resultados
saveRDS(resultados, "analise_modelos/resultados_arima.rds")
cat("\n✅ Resultados guardados em 'analise_modelos/resultados_arima.rds'\n")