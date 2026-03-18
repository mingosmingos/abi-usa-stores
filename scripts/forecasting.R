# ==================================================
# forecasting.R - Modelos de previsão de clientes
# ==================================================

library(forecast)
library(rminer)
library(ggplot2)
library(tidyverse)

#' Prepara dados para modelação com lags
#' @param dados Dataframe da loja
#' @param max_lag Máximo lag a criar
#' @return Dataframe com features
preparar_dados_lags <- function(dados, max_lag = 7) {
  df <- dados %>%
    arrange(Date) %>%
    mutate(
      # Lags da variável alvo
      Lag1 = lag(Num_Customers, 1),
      Lag2 = lag(Num_Customers, 2),
      Lag3 = lag(Num_Customers, 3),
      Lag7 = lag(Num_Customers, 7)
    )
  
  # Adicionar médias móveis
  df <- df %>%
    mutate(
      MediaMovel3 = zoo::rollmean(Num_Customers, 3, fill = NA, align = "right"),
      MediaMovel7 = zoo::rollmean(Num_Customers, 7, fill = NA, align = "right")
    )
  
  return(df)
}

#' Modelo ARIMA para previsão
#' @param dados Dataframe com dados históricos
#' @param horizonte Dias a prever (1-7)
#' @return Vetor com previsões
modelo_arima <- function(dados, horizonte = 7) {
  # Criar série temporal com frequência semanal
  serie <- ts(dados$Num_Customers, frequency = 7)
  
  # Auto ARIMA para escolher melhor modelo
  modelo <- auto.arima(serie, 
                       seasonal = TRUE,
                       stepwise = FALSE,
                       approximation = FALSE)
  
  # Fazer previsão
  previsao <- forecast(modelo, h = horizonte)
  
  # Retornar valores previstos
  return(as.numeric(previsao$mean))
}

#' Modelo ETS (Exponential Smoothing)
#' @param dados Dataframe com dados históricos
#' @param horizonte Dias a prever (1-7)
#' @return Vetor com previsões
modelo_ets <- function(dados, horizonte = 7) {
  serie <- ts(dados$Num_Customers, frequency = 7)
  modelo <- ets(serie)
  previsao <- forecast(modelo, h = horizonte)
  return(as.numeric(previsao$mean))
}

#' Modelo de Suavização Exponencial Simples
#' @param dados Dataframe com dados históricos
#' @param horizonte Dias a prever (1-7)
#' @return Vetor com previsões
modelo_ses <- function(dados, horizonte = 7) {
  serie <- ts(dados$Num_Customers, frequency = 7)
  modelo <- ses(serie, h = horizonte)
  return(as.numeric(modelo$mean))
}

#' Modelo HOLT-Winters (Tendência + Sazonalidade)
#' @param dados Dataframe com dados históricos
#' @param horizonte Dias a prever (1-7)
#' @return Vetor com previsões
modelo_hw <- function(dados, horizonte = 7) {
  serie <- ts(dados$Num_Customers, frequency = 7)
  modelo <- hw(serie, seasonal = "additive")
  previsao <- forecast(modelo, h = horizonte)
  return(as.numeric(previsao$mean))
}

#' Modelo de Regressão Linear Múltipla
#' @param dados Dataframe com dados históricos
#' @param horizonte Dias a prever (1-7)
#' @return Vetor com previsões
modelo_lm <- function(dados, horizonte = 7) {
  # Preparar dados com lags
  df <- preparar_dados_lags(dados) %>%
    filter(!is.na(Lag7))  # Remover NAs
  
  # Treinar modelo
  modelo <- lm(Num_Customers ~ Num_Employees + Pct_On_Sale + 
                 TouristEvent_bin + Lag1 + Lag2 + Lag7,
               data = df)
  
  # Para previsão, usar últimos valores conhecidos
  ultimos <- tail(df, 1)
  previsoes <- numeric(horizonte)
  
  for(i in 1:horizonte) {
    # Atualizar lags para cada passo
    novos_dados <- data.frame(
      Num_Employees = ultimos$Num_Employees,
      Pct_On_Sale = ultimos$Pct_On_Sale,
      TouristEvent_bin = ultimos$TouristEvent_bin,
      Lag1 = if(i == 1) ultimos$Num_Customers else previsoes[i-1],
      Lag2 = if(i <= 2) ultimos$Lag1 else previsoes[i-2],
      Lag7 = if(i <= 7) ultimos$Lag7 else NA
    )
    
    previsoes[i] <- predict(modelo, novos_dados)
  }
  
  return(previsoes)
}

#' Modelo Random Forest com rminer
#' @param dados Dataframe com dados históricos
#' @param horizonte Dias a prever (1-7)
#' @return Vetor com previsões
modelo_rf <- function(dados, horizonte = 7) {
  # Preparar dados
  df <- preparar_dados_lags(dados) %>%
    filter(!is.na(Lag7))
  
  # Features para o modelo
  features <- c("Num_Employees", "Pct_On_Sale", "TouristEvent_bin",
                "DiaSemana", "Lag1", "Lag2", "Lag3", "Lag7",
                "MediaMovel3", "MediaMovel7")
  
  # Treinar com rminer
  modelo <- fit(Num_Customers ~ ., 
                data = df[, c("Num_Customers", features)],
                model = "randomForest",
                search = "heuristic")  # Heuristic para acelerar
  
  # Prever (simplificado - em produção seria multi-step)
  ultimo <- tail(df, 1)
  previsao <- predict(modelo, ultimo[, features])
  
  return(rep(as.numeric(previsao), horizonte))
}

#' Modelo Neural Network (nnetar)
#' @param dados Dataframe com dados históricos
#' @param horizonte Dias a prever (1-7)
#' @return Vetor com previsões
modelo_nnetar <- function(dados, horizonte = 7) {
  serie <- ts(dados$Num_Customers, frequency = 7)
  modelo <- nnetar(serie, P = 1, P = 1, size = 5)
  previsao <- forecast(modelo, h = horizonte)
  return(as.numeric(previsao$mean))
}

#' Avaliar modelos com walk-forward validation
#' @param dados Dataframe completo
#' @param teste_dias Número de dias para teste
#' @return Lista com resultados
avaliar_modelos <- function(dados, teste_dias = 30) {
  
  n <- nrow(dados)
  treino <- dados[1:(n - teste_dias), ]
  teste <- dados[(n - teste_dias + 1):n, ]
  
  # Lista de modelos a testar
  modelos <- list(
    ARIMA = modelo_arima,
    ETS = modelo_ets,
    SES = modelo_ses,
    HW = modelo_hw,
    LM = modelo_lm,
    RF = modelo_rf,
    NNETAR = modelo_nnetar
  )
  
  resultados <- list()
  matriz_erros <- data.frame()
  
  for(nome_modelo in names(modelos)) {
    cat("A avaliar modelo:", nome_modelo, "\n")
    
    previsoes <- numeric(teste_dias)
    erros <- numeric(teste_dias)
    
    for(i in 1:teste_dias) {
      # Dados até ao ponto i-1 do teste
      dados_ate_i <- rbind(treino, teste[1:(i-1), ])
      
      # Prever
      tryCatch({
        pred <- modelos[[nome_modelo]](dados_ate_i, 1)
        previsoes[i] <- pred[1]
        erros[i] <- abs(previsoes[i] - teste$Num_Customers[i])
      }, error = function(e) {
        warning("Erro no modelo ", nome_modelo, ": ", e$message)
        previsoes[i] <- NA
        erros[i] <- NA
      })
    }
    
    # Calcular métricas
    valores_reais <- teste$Num_Customers[1:teste_dias]
    na_idx <- complete.cases(previsoes, valores_reais)
    
    if(sum(na_idx) > 0) {
      mae <- mean(abs(previsoes[na_idx] - valores_reais[na_idx]))
      rmse <- sqrt(mean((previsoes[na_idx] - valores_reais[na_idx])^2))
      mape <- mean(abs((previsoes[na_idx] - valores_reais[na_idx]) / 
                         valores_reais[na_idx])) * 100
      
      resultados[[nome_modelo]] <- list(
        previsoes = previsoes,
        erros = erros,
        mae = mae,
        rmse = rmse,
        mape = mape
      )
      
      # Adicionar à matriz de erros
      matriz_erros <- rbind(matriz_erros, data.frame(
        Modelo = nome_modelo,
        MAE = round(mae, 2),
        RMSE = round(rmse, 2),
        MAPE = round(mape, 2),
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Ordenar por MAE
  matriz_erros <- matriz_erros[order(matriz_erros$MAE), ]
  
  return(list(
    resultados = resultados,
    metricas = matriz_erros,
    teste = teste
  ))
}

#' Plotar comparação de modelos
#' @param resultados Resultados da avaliação
#' @return Gráfico ggplot
plot_comparacao_modelos <- function(resultados) {
  df <- data.frame()
  
  for(modelo in names(resultados$resultados)) {
    res <- resultados$resultados[[modelo]]
    temp <- data.frame(
      Modelo = modelo,
      Dia = 1:length(res$previsoes),
      Previsao = res$previsoes,
      Real = resultados$teste$Num_Customers[1:length(res$previsoes)],
      Erro = res$erros
    )
    df <- rbind(df, temp)
  }
  
  p <- ggplot(df, aes(x = Dia)) +
    geom_line(aes(y = Real, color = "Real"), size = 1) +
    geom_line(aes(y = Previsao, color = Modelo), alpha = 0.7) +
    labs(title = "Comparação de Modelos de Previsão",
         x = "Dia", y = "Número de Clientes") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1")
  
  return(p)
}

