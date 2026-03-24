# ============================================================
# PROJETO: Forecasting com VAR (versão corrigida)
# ============================================================

# 1. BIBLIOTECAS
# ============================================================
library(vars)
library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)

# 2. FUNÇÕES REUTILIZÁVEIS
# ============================================================

# Carregar e preparar dados de todas as lojas (com tratamento de NAs)
load_stores <- function(files) {
  df_list <- list()
  for (i in seq_along(files)) {
    df <- read.csv(files[i], stringsAsFactors = FALSE)
    df$Store <- names(files)[i]
    df$TouristEventNum <- ifelse(df$TouristEvent == "Yes", 1, 0)
    df$Date <- as.Date(df$Date)
    
    # Tratar NAs na coluna Pct_On_Sale (substituir pela média da loja)
    df$Pct_On_Sale[is.na(df$Pct_On_Sale)] <- mean(df$Pct_On_Sale, na.rm = TRUE)
    
    df_list[[i]] <- df
  }
  bind_rows(df_list) %>% arrange(Store, Date)
}

# Calcular métricas de erro
metrics <- function(actual, pred) {
  actual <- actual[complete.cases(actual, pred)]
  pred <- pred[complete.cases(actual, pred)]
  if (length(actual) == 0) return(c(RMSE = NA, MAE = NA, MAPE = NA))
  c(RMSE = sqrt(mean((actual - pred)^2)),
    MAE = mean(abs(actual - pred)),
    MAPE = mean(abs((actual - pred)/actual)) * 100)
}

# Preparar dados para VAR (4 variáveis) - REMOVE LINHAS COM ZEROS
prepare_var <- function(df) {
  # Selecionar colunas
  result <- df[, c("Num_Customers", "Num_Employees", "Pct_On_Sale", "TouristEventNum")]
  
  # Remover linhas com valores zero em Num_Customers (dias sem clientes, como Natal)
  # E remover NAs se ainda existirem
  result <- result[result$Num_Customers > 0, ]
  result <- na.omit(result)
  
  return(result)
}

# Treinar VAR e prever h dias
var_forecast <- function(train, h = 7) {
  # Verificar se há dados suficientes
  if (nrow(train) < 10) return(rep(NA, h))
  
  # Selecionar lag máximo seguro
  max_lag <- min(10, floor(nrow(train) / 3))
  if (max_lag < 1) max_lag <- 1
  
  # Selecionar lag ótimo
  lag_selection <- tryCatch(
    VARselect(train, lag.max = max_lag, type = "const"),
    error = function(e) return(NULL)
  )
  
  if (is.null(lag_selection)) return(rep(NA, h))
  
  lag <- lag_selection$selection["AIC(n)"]
  if (is.na(lag) || lag < 1) lag <- 1
  if (lag > 5) lag <- 5
  
  # Estimar modelo
  model <- tryCatch(
    VAR(train, p = lag, type = "const"),
    error = function(e) return(NULL)
  )
  
  if (is.null(model)) return(rep(NA, h))
  
  # Prever
  pred <- tryCatch(
    predict(model, n.ahead = h)$fcst$Num_Customers[, "fcst"],
    error = function(e) return(rep(NA, h))
  )
  
  # Garantir que as previsões não são negativas
  pred <- pmax(pred, 0)
  
  return(pred)
}

# Avaliar com hold-out (última semana)
evaluate_holdout <- function(df, store, h = 7) {
  data <- df %>% filter(Store == store) %>% arrange(Date)
  
  # Remover linhas com Num_Customers = 0 antes de dividir
  data <- data[data$Num_Customers > 0, ]
  
  n <- nrow(data)
  if (n <= h + 10) {
    cat("Dados insuficientes para", store, "\n")
    return(NULL)
  }
  
  train <- prepare_var(data[1:(n - h), ])
  test_actual <- data[(n - h + 1):n, ]$Num_Customers
  
  # Verificar se train tem dados suficientes
  if (nrow(train) < 10) {
    cat("Treino insuficiente para", store, "\n")
    return(NULL)
  }
  
  pred <- var_forecast(train, h)
  
  # Baseline: Seasonal Naive (últimos 7 valores do treino original, ignorando zeros)
  train_original <- data[1:(n - h), ]$Num_Customers
  train_original <- train_original[train_original > 0]
  if (length(train_original) >= h) {
    base <- rep(tail(train_original, h), length.out = h)
  } else {
    base <- rep(mean(train_original), h)
  }
  
  list(store = store, pred = pred, actual = test_actual, 
       metrics = metrics(test_actual, pred), baseline = metrics(test_actual, base))
}

# Backtesting com rolling window
backtest_var <- function(df, store, h = 7, n_iter = 20) {
  data <- df %>% filter(Store == store) %>% arrange(Date)
  
  # Remover dias com Num_Customers = 0
  data <- data[data$Num_Customers > 0, ]
  
  var_data <- prepare_var(data)
  n <- nrow(var_data)
  
  results <- list()
  successful <- 0
  
  for (i in 1:n_iter) {
    train_end <- min(n - h - n_iter + i, n - h)
    if (train_end < 10) next
    
    train <- var_data[1:train_end, ]
    test_actual <- var_data[(train_end + 1):(train_end + h), "Num_Customers"]
    
    # Verificar se há dados suficientes
    if (nrow(train) < 10 || length(test_actual) < h) next
    
    pred <- tryCatch(var_forecast(train, h), error = function(e) rep(NA, h))
    
    if (!is.null(pred) && !any(is.na(pred))) {
      # Baseline: última semana de treino
      if (train_end >= h) {
        base <- rep(var_data[(train_end - h + 1):train_end, "Num_Customers"], length.out = h)
      } else {
        base <- rep(mean(train$Num_Customers), h)
      }
      
      results[[length(results) + 1]] <- metrics(test_actual, pred)
      successful <- successful + 1
    }
  }
  
  if (length(results) == 0) {
    return(NULL)
  }
  
  metrics_df <- do.call(rbind, results)
  list(store = store, 
       metrics = apply(metrics_df, 2, median, na.rm = TRUE), 
       n = successful)
}

# 3. EXECUÇÃO
# ============================================================

# Carregar dados
files <- c("baltimore.csv", "lancaster.csv", "philadelphia.csv", "richmond.csv")
names(files) <- gsub(".csv", "", files)
data <- load_stores(files)
stores <- unique(data$Store)

cat("Dados carregados com sucesso!\n")
cat("Total de registos:", nrow(data), "\n")
cat("Registos por loja:\n")
print(table(data$Store))

# FASE I - Hold-out (última semana)
cat("\n========== FASE I (Hold-out) ==========\n")
results1 <- list()

for (s in stores) {
  cat("\n---", s, "---\n")
  r <- evaluate_holdout(data, s, 7)
  if (!is.null(r)) {
    results1[[s]] <- r
    print(r$metrics)
    cat("Baseline RMSE:", round(r$baseline["RMSE"], 2), "\n")
    if (!is.na(r$metrics["RMSE"]) && !is.na(r$baseline["RMSE"])) {
      cat("Melhoria RMSE:", round((1 - r$metrics["RMSE"]/r$baseline["RMSE"])*100, 1), "%\n")
    }
    
    # Gráfico apenas se houver previsões válidas
    if (!any(is.na(r$pred))) {
      plot_df <- data.frame(Dia = 1:7, Actual = r$actual, VAR = r$pred)
      p <- ggplot(plot_df, aes(x = Dia)) +
        geom_line(aes(y = Actual, color = "Actual"), linewidth = 1) +
        geom_line(aes(y = VAR, color = "VAR"), linewidth = 1, linetype = "dashed") +
        labs(title = s, y = "Clientes") + 
        theme_minimal() + 
        scale_color_manual(values = c("Actual" = "black", "VAR" = "blue"))
      print(p)
    }
  }
}

# FASE II - Backtesting com rolling window
cat("\n\n========== FASE II (Backtesting) ==========\n")
results2 <- list()

for (s in stores) {
  cat("\n---", s, "---\n")
  r <- backtest_var(data, s, 7, 20)
  if (!is.null(r)) {
    results2[[s]] <- r
    cat("RMSE mediano:", round(r$metrics["RMSE"], 2), "\n")
    cat("MAE mediano:", round(r$metrics["MAE"], 2), "\n")
    cat("MAPE mediano:", round(r$metrics["MAPE"], 2), "%\n")
    cat("Iterações bem-sucedidas:", r$n, "/20\n")
  } else {
    cat("Não foi possível realizar backtesting\n")
  }
}

# Tabela resumo
cat("\n\n========== RESUMO BACKTESTING ==========\n")
summary_df <- do.call(rbind, lapply(names(results2), function(s) {
  r <- results2[[s]]
  if (!is.null(r)) {
    data.frame(Loja = s, 
               RMSE = round(r$metrics["RMSE"], 2), 
               MAE = round(r$metrics["MAE"], 2), 
               MAPE = round(r$metrics["MAPE"], 2),
               Iteracoes = r$n)
  }
}))
print(summary_df)

# Exportar previsões para otimização
cat("\n\n========== EXPORTAR PREVISÕES ==========\n")

generate_all_forecasts <- function(df, n_weeks = 20, h = 7) {
  forecasts <- list()
  
  for (store in stores) {
    cat("Processando", store, "...\n")
    data <- df %>% filter(Store == store) %>% arrange(Date)
    
    # Remover dias com Num_Customers = 0
    data <- data[data$Num_Customers > 0, ]
    
    var_data <- prepare_var(data)
    n <- nrow(var_data)
    
    if (n < 30) {
      cat("  Dados insuficientes para", store, "\n")
      next
    }
    
    for (i in 1:n_weeks) {
      train_end <- n - (n_weeks - i) * h - h
      if (train_end < 10) next
      if (train_end + h > n) next
      
      train <- var_data[1:train_end, ]
      
      pred <- tryCatch(var_forecast(train, h), error = function(e) rep(NA, h))
      
      if (!is.null(pred) && !any(is.na(pred))) {
        forecasts[[length(forecasts) + 1]] <- data.frame(
          Week_Start = as.character(data$Date[train_end + 1]), 
          Store = store, 
          Day = 1:h, 
          Forecast = round(pred, 0),
          Week_ID = i
        )
      }
    }
  }
  
  if (length(forecasts) == 0) {
    return(data.frame())
  }
  
  do.call(rbind, forecasts)
}

forecasts_df <- generate_all_forecasts(data, 20, 7)

if (nrow(forecasts_df) > 0) {
  write.csv(forecasts_df, "forecasts_var.csv", row.names = FALSE)
  cat("\nPrevisões salvas em 'forecasts_var.csv' com", nrow(forecasts_df), "linhas\n")
  cat("Exemplo das primeiras previsões:\n")
  print(head(forecasts_df, 10))
} else {
  cat("Não foi possível gerar previsões\n")
}

cat("\n========== FIM ==========\n")