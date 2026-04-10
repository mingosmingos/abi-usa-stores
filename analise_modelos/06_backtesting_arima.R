# 06_backtesting_arima.R - Backtesting com 3 métodos (Holdout, Growing, Rolling)

source("scripts/utils.R")
dados <- carregar_dados_lojas("dados/")

library(forecast)
library(ggplot2)

# ============================================================
# FUNÇÃO PARA BACKTESTING GROWING WINDOW
# ============================================================

backtesting_growing <- function(dados_loja, nome_loja, n_testes = 30) {
  
  n <- nrow(dados_loja)
  tamanho_inicial <- floor(n * 0.6)  # começa com 60% dos dados
  erros <- numeric(n_testes)
  
  for(i in 1:n_testes) {
    fim_treino <- tamanho_inicial + i - 1
    if(fim_treino + 1 > n) break
    
    treino <- dados_loja[1:fim_treino, ]
    teste <- dados_loja[fim_treino + 1, ]
    
    serie_treino <- ts(treino$Num_Customers, frequency = 7)
    modelo <- auto.arima(serie_treino, seasonal = TRUE)
    previsao <- forecast(modelo, h = 1)
    
    erro <- abs(as.numeric(previsao$mean) - teste$Num_Customers)
    erros[i] <- erro
  }
  
  erros_validos <- erros[!is.na(erros) & erros > 0]
  return(list(mae = mean(erros_validos), rmse = sqrt(mean(erros_validos^2))))
}

# ============================================================
# FUNÇÃO PARA BACKTESTING ROLLING WINDOW
# ============================================================

backtesting_rolling <- function(dados_loja, nome_loja, n_testes = 30) {
  
  n <- nrow(dados_loja)
  tamanho_janela <- floor(n * 0.6)  # janela fixa de 60%
  erros <- numeric(n_testes)
  
  for(i in 1:n_testes) {
    inicio_treino <- i
    fim_treino <- inicio_treino + tamanho_janela - 1
    if(fim_treino + 1 > n) break
    
    treino <- dados_loja[inicio_treino:fim_treino, ]
    teste <- dados_loja[fim_treino + 1, ]
    
    serie_treino <- ts(treino$Num_Customers, frequency = 7)
    modelo <- auto.arima(serie_treino, seasonal = TRUE)
    previsao <- forecast(modelo, h = 1)
    
    erro <- abs(as.numeric(previsao$mean) - teste$Num_Customers)
    erros[i] <- erro
  }
  
  erros_validos <- erros[!is.na(erros) & erros > 0]
  return(list(mae = mean(erros_validos), rmse = sqrt(mean(erros_validos^2))))
}

# ============================================================
# COMPARAÇÃO DOS 3 MÉTODOS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("BACKTESTING ARIMA - COMPARAÇÃO DE MÉTODOS\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Resultados do Holdout (já existentes)
resultados_holdout <- readRDS("analise_modelos/resultados_arima.rds")

# Tabela de resultados
resultados <- data.frame(
  Loja = names(resultados_holdout),
  Holdout_MAE = sapply(resultados_holdout, function(x) round(x$mae, 2)),
  Growing_MAE = NA,
  Rolling_MAE = NA
)

# Calcular Growing e Rolling para cada loja
for(loja in names(dados)) {
  cat("\nProcessando", loja, "...\n")
  
  growing <- backtesting_growing(dados[[loja]], loja)
  rolling <- backtesting_rolling(dados[[loja]], loja)
  
  resultados$Growing_MAE[resultados$Loja == loja] <- round(growing$mae, 2)
  resultados$Rolling_MAE[resultados$Loja == loja] <- round(rolling$mae, 2)
}

# Mostrar tabela
cat("\n", paste(rep("=", 70), collapse=""), "\n")
print(resultados)

# ============================================================
# GRÁFICO
# ============================================================

df_plot <- data.frame(
  Loja = rep(resultados$Loja, 3),
  Metodo = c(rep("Holdout", 4), rep("Growing", 4), rep("Rolling", 4)),
  MAE = c(resultados$Holdout_MAE, resultados$Growing_MAE, resultados$Rolling_MAE)
)

p <- ggplot(df_plot, aes(x = Loja, y = MAE, fill = Metodo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Backtesting ARIMA - Comparação de Métodos",
       x = "Loja", y = "MAE (clientes)") +
  theme_minimal() +
  scale_fill_manual(values = c("Holdout" = "red", "Growing" = "blue", "Rolling" = "green")) +
  geom_text(aes(label = round(MAE, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)

print(p)

# Guardar
ggsave("analise_modelos/backtesting_comparacao.png", p, width = 10, height = 6)
saveRDS(resultados, "analise_modelos/backtesting_resultados.rds")

cat("\n✅ Resultados guardados!\n")
cat("   - backtesting_resultados.rds\n")
cat("   - backtesting_comparacao.png\n")