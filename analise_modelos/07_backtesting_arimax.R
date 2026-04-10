# 07_backtesting_arimax.R - Backtesting para ARIMAX (Growing e Rolling)

source("scripts/utils.R")
dados <- carregar_dados_lojas("dados/")

library(forecast)
library(ggplot2)

# ============================================================
# BACKTESTING GROWING WINDOW para ARIMAX
# ============================================================

backtesting_growing_arimax <- function(dados_loja, nome_loja, n_testes = 30) {
  
  n <- nrow(dados_loja)
  tamanho_inicial <- floor(n * 0.6)  # começa com 60% dos dados
  erros <- c()
  
  for(i in 1:n_testes) {
    fim_treino <- tamanho_inicial + i - 1
    if(fim_treino + 1 > n) break
    
    treino <- dados_loja[1:fim_treino, ]
    teste <- dados_loja[fim_treino + 1, ]
    
    # Série temporal
    serie_treino <- ts(treino$Num_Customers, frequency = 7)
    
    # Variáveis exógenas
    xreg_treino <- as.matrix(treino[, c("Num_Employees", "Pct_On_Sale", "TouristEvent_bin")])
    xreg_teste <- as.matrix(teste[, c("Num_Employees", "Pct_On_Sale", "TouristEvent_bin")])
    
    # ARIMAX
    modelo <- auto.arima(serie_treino, 
                         xreg = xreg_treino,
                         seasonal = TRUE)
    
    # Previsão
    previsao <- forecast(modelo, xreg = xreg_teste, h = 1)
    
    erro <- abs(as.numeric(previsao$mean) - teste$Num_Customers)
    erros <- c(erros, erro)
  }
  
  return(list(mae = mean(erros), rmse = sqrt(mean(erros^2))))
}

# ============================================================
# BACKTESTING ROLLING WINDOW para ARIMAX
# ============================================================

backtesting_rolling_arimax <- function(dados_loja, nome_loja, n_testes = 30) {
  
  n <- nrow(dados_loja)
  tamanho_janela <- floor(n * 0.6)  # janela fixa de 60%
  erros <- c()
  
  for(i in 1:n_testes) {
    inicio_treino <- i
    fim_treino <- inicio_treino + tamanho_janela - 1
    if(fim_treino + 1 > n) break
    
    treino <- dados_loja[inicio_treino:fim_treino, ]
    teste <- dados_loja[fim_treino + 1, ]
    
    # Série temporal
    serie_treino <- ts(treino$Num_Customers, frequency = 7)
    
    # Variáveis exógenas
    xreg_treino <- as.matrix(treino[, c("Num_Employees", "Pct_On_Sale", "TouristEvent_bin")])
    xreg_teste <- as.matrix(teste[, c("Num_Employees", "Pct_On_Sale", "TouristEvent_bin")])
    
    # ARIMAX
    modelo <- auto.arima(serie_treino, 
                         xreg = xreg_treino,
                         seasonal = TRUE)
    
    # Previsão
    previsao <- forecast(modelo, xreg = xreg_teste, h = 1)
    
    erro <- abs(as.numeric(previsao$mean) - teste$Num_Customers)
    erros <- c(erros, erro)
  }
  
  return(list(mae = mean(erros), rmse = sqrt(mean(erros^2))))
}

# ============================================================
# EXECUTAR PARA TODAS AS LOJAS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("BACKTESTING ARIMAX - GROWING vs ROLLING\n")
cat(paste(rep("=", 70), collapse=""), "\n")

# Carregar resultados do Holdout (já tens do ARIMAX)
resultados_holdout <- readRDS("analise_modelos/resultados_arimax.rds")

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
  
  growing <- backtesting_growing_arimax(dados[[loja]], loja)
  rolling <- backtesting_rolling_arimax(dados[[loja]], loja)
  
  resultados$Growing_MAE[resultados$Loja == loja] <- round(growing$mae, 2)
  resultados$Rolling_MAE[resultados$Loja == loja] <- round(rolling$mae, 2)
}

# Mostrar tabela
cat("\n", paste(rep("=", 70), collapse=""), "\n")
print(resultados)

# ============================================================
# MÉDIAS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("MÉDIAS GLOBAIS:\n")
cat("Holdout MAE médio:", round(mean(resultados$Holdout_MAE), 2), "clientes\n")
cat("Growing MAE médio:", round(mean(resultados$Growing_MAE, na.rm = TRUE), 2), "clientes\n")
cat("Rolling MAE médio:", round(mean(resultados$Rolling_MAE, na.rm = TRUE), 2), "clientes\n")

# ============================================================
# COMPARAÇÃO COM ARIMA
# ============================================================

# Carregar resultados do backtesting do ARIMA
resultados_arima <- readRDS("analise_modelos/backtesting_resultados.rds")

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("COMPARAÇÃO ARIMA vs ARIMAX (Rolling Window)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

comparacao <- data.frame(
  Loja = resultados$Loja,
  ARIMA_Rolling = resultados_arima$Rolling_MAE,
  ARIMAX_Rolling = resultados$Rolling_MAE,
  Melhoria = paste0(round((resultados_arima$Rolling_MAE - resultados$Rolling_MAE) / 
                            resultados_arima$Rolling_MAE * 100, 1), "%")
)

print(comparacao)

# ============================================================
# GRÁFICO 1: Backtesting ARIMAX
# ============================================================

df_plot1 <- data.frame(
  Loja = rep(resultados$Loja, 3),
  Metodo = c(rep("Holdout", 4), rep("Growing", 4), rep("Rolling", 4)),
  MAE = c(resultados$Holdout_MAE, resultados$Growing_MAE, resultados$Rolling_MAE)
)

p1 <- ggplot(df_plot1, aes(x = Loja, y = MAE, fill = Metodo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Backtesting ARIMAX - Comparação de Métodos",
       x = "Loja", y = "MAE (clientes)") +
  theme_minimal() +
  scale_fill_manual(values = c("Holdout" = "red", "Growing" = "blue", "Rolling" = "green")) +
  geom_text(aes(label = round(MAE, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)

print(p1)

# ============================================================
# GRÁFICO 2: ARIMA vs ARIMAX (Rolling Window)
# ============================================================

df_plot2 <- data.frame(
  Loja = rep(comparacao$Loja, 2),
  Modelo = c(rep("ARIMA", 4), rep("ARIMAX", 4)),
  MAE = c(comparacao$ARIMA_Rolling, comparacao$ARIMAX_Rolling)
)

p2 <- ggplot(df_plot2, aes(x = Loja, y = MAE, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "ARIMA vs ARIMAX - Rolling Window",
       x = "Loja", y = "MAE (clientes)") +
  theme_minimal() +
  scale_fill_manual(values = c("ARIMA" = "red", "ARIMAX" = "blue")) +
  geom_text(aes(label = round(MAE, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)

print(p2)

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

ggsave("analise_modelos/backtesting_arimax_comparacao.png", p1, width = 10, height = 6)
ggsave("analise_modelos/arima_vs_arimax_rolling.png", p2, width = 10, height = 6)
saveRDS(resultados, "analise_modelos/backtesting_arimax_resultados.rds")

cat("\n✅ Resultados guardados!\n")
cat("   - backtesting_arimax_resultados.rds\n")
cat("   - backtesting_arimax_comparacao.png\n")
cat("   - arima_vs_arimax_rolling.png\n")