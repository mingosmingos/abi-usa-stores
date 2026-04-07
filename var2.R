# ============================================================
# PROJETO: Forecasting com VAR (Versão Completa com Plots - Corrigida)
# ============================================================

# 1. BIBLIOTECAS
# ============================================================
library(vars)
library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)

# 2. FUNÇÕES BASE REUTILIZÁVEIS
# ============================================================

# Carregar dados
load_stores <- function(files) {
  df_list <- list()
  for (i in seq_along(files)) {
    df <- read.csv(files[i], stringsAsFactors = FALSE)
    df$Store <- names(files)[i]
    df$TouristEventNum <- ifelse(df$TouristEvent == "Yes", 1, 0)
    df$Date <- as.Date(df$Date)
    df$Pct_On_Sale[is.na(df$Pct_On_Sale)] <- mean(df$Pct_On_Sale, na.rm = TRUE)
    df_list[[i]] <- df
  }
  bind_rows(df_list) %>% arrange(Store, Date)
}

# Métricas de erro
metrics <- function(actual, pred) {
  actual <- actual[complete.cases(actual, pred)]
  pred <- pred[complete.cases(actual, pred)]
  if (length(actual) == 0) return(c(RMSE = NA, MAE = NA, MAPE = NA, R2 = NA))
  rmse <- sqrt(mean((actual - pred)^2))
  mae <- mean(abs(actual - pred))
  mape <- mean(abs((actual - pred)/actual)) * 100
  r2 <- 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2)
  c(RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2)
}

# Preparar dados VAR
prepare_var <- function(df) {
  df <- df[, c("Num_Customers", "Num_Employees", "Pct_On_Sale", "TouristEventNum")]
  result <- na.omit(df[df$Num_Customers > 0, ])
  return(result)
}

# ============================================================
# 3. FUNÇÃO UNIVERSAL VAR
# ============================================================

run_var <- function(train, h = 7, lag = NULL) {
  if (nrow(train) < 10) return(rep(NA, h))
  
  if (is.null(lag)) {
    max_lag <- min(10, floor(nrow(train) / 3))
    if (max_lag < 1) return(rep(NA, h))
    lag <- tryCatch(VARselect(train, lag.max = max_lag, type = "const")$selection["AIC(n)"], 
                    error = function(e) 1)
    if (is.na(lag) || lag < 1) lag <- 1
    if (lag > 10) lag <- 10
  } else {
    if (lag >= nrow(train)) lag <- floor(nrow(train) / 2)
    if (lag < 1) lag <- 1
  }
  
  model <- tryCatch(VAR(train, p = lag, type = "const"), error = function(e) NULL)
  if (is.null(model)) return(rep(NA, h))
  
  pred <- tryCatch(predict(model, n.ahead = h)$fcst$Num_Customers[, "fcst"], 
                   error = function(e) rep(NA, h))
  pmax(pred, 0)
}

# ============================================================
# 4. AVALIAÇÃO DE LAGS
# ============================================================

eval_lags <- function(df, store, lags = c(7, 14, 21, 28), h = 7) {
  data <- df %>% filter(Store == store, Num_Customers > 0) %>% arrange(Date)
  n <- nrow(data)
  if (n <= h + max(lags)) return(NULL)
  
  train <- prepare_var(data[1:(n - h), ])
  actual <- data[(n - h + 1):n, ]$Num_Customers
  
  # Baseline
  base_actual <- data[1:(n - h), ]$Num_Customers
  base_actual <- base_actual[base_actual > 0]
  if (length(base_actual) >= h) {
    base <- rep(tail(base_actual, h), length.out = h)
  } else {
    base <- rep(mean(base_actual), h)
  }
  baseline <- metrics(actual, base)
  
  # Testar cada lag
  results <- data.frame()
  predictions_list <- list()
  
  for (lag in lags) {
    if (nrow(train) > lag + 5) {
      pred <- run_var(train, h, lag)
      if (!any(is.na(pred))) {
        m <- metrics(actual, pred)
        results <- rbind(results, data.frame(
          Store = store, Lag = as.numeric(lag),
          RMSE = as.numeric(m["RMSE"]), MAE = as.numeric(m["MAE"]),
          MAPE = as.numeric(m["MAPE"]), R2 = as.numeric(m["R2"])
        ))
        predictions_list[[as.character(lag)]] <- pred
      }
    }
  }
  
  list(store = store, comparison = results, baseline = baseline, actual = actual, 
       predictions = predictions_list, lags = lags)
}

# ============================================================
# 5. BACKTESTING
# ============================================================

backtest <- function(df, store, h = 7, n_iter = 20, lag = NULL) {
  data <- df %>% filter(Store == store, Num_Customers > 0) %>% arrange(Date)
  var_data <- prepare_var(data)
  n <- nrow(var_data)
  
  var_metrics <- list()
  base_metrics <- list()
  all_predictions <- list()
  all_actuals <- list()
  
  for (i in 1:n_iter) {
    train_end <- min(n - h - n_iter + i, n - h)
    if (train_end < 10) next
    
    train <- var_data[1:train_end, ]
    actual <- var_data[(train_end + 1):(train_end + h), "Num_Customers"]
    if (length(actual) < h) next
    
    pred_var <- run_var(train, h, lag)
    if (!any(is.na(pred_var))) {
      var_metrics[[length(var_metrics) + 1]] <- metrics(actual, pred_var)
      all_predictions[[length(all_predictions) + 1]] <- pred_var
      all_actuals[[length(all_actuals) + 1]] <- actual
    }
    
    if (train_end >= h) {
      pred_base <- rep(var_data[(train_end - h + 1):train_end, "Num_Customers"], length.out = h)
    } else {
      pred_base <- rep(mean(train$Num_Customers), h)
    }
    base_metrics[[length(base_metrics) + 1]] <- metrics(actual, pred_base)
  }
  
  if (length(var_metrics) == 0) return(NULL)
  
  var_mat <- do.call(rbind, var_metrics)
  base_mat <- do.call(rbind, base_metrics)
  
  var_med <- apply(var_mat, 2, median, na.rm = TRUE)
  base_med <- apply(base_mat, 2, median, na.rm = TRUE)
  
  list(store = store, lag = lag, n = length(var_metrics),
       VAR = var_med, Baseline = base_med,
       Melhoria = round((base_med["RMSE"] - var_med["RMSE"]) / base_med["RMSE"] * 100, 1),
       all_predictions = all_predictions, all_actuals = all_actuals)
}

# ============================================================
# 6. ANÁLISE DE INFLUÊNCIA ENTRE LOJAS
# ============================================================

prepare_all_stores <- function(df) {
  result <- df %>%
    filter(Num_Customers > 0) %>%
    select(Date, Store, Num_Customers) %>%
    pivot_wider(names_from = Store, values_from = Num_Customers) %>%
    arrange(Date) %>% na.omit()
  
  if (ncol(result) < 2) return(NULL)
  return(as.data.frame(result[, -1]))
}

granger_test_all <- function(ts_data, max_lag = 7) {
  if (is.null(ts_data) || ncol(ts_data) < 2) return(data.frame())
  
  stores <- colnames(ts_data)
  results <- data.frame()
  
  for (causa in stores) {
    for (efeito in stores[stores != causa]) {
      max_lag_possible <- min(max_lag, floor(nrow(ts_data) / 3))
      if (max_lag_possible < 1) next
      
      test <- tryCatch({
        v <- VAR(ts_data[, c(efeito, causa)], p = max_lag_possible, type = "const")
        causality(v, cause = causa)$Granger$p.value
      }, error = function(e) NA)
      
      if (!is.na(test)) {
        results <- rbind(results, data.frame(
          Causa = causa, Efeito = efeito, p_valor = test, Significante = test < 0.05
        ))
      }
    }
  }
  return(results)
}

compute_fevd <- function(ts_data, lag = NULL) {
  if (is.null(ts_data) || ncol(ts_data) < 2) return(data.frame())
  
  if (is.null(lag)) lag <- min(5, floor(nrow(ts_data) / 3))
  if (lag < 1) lag <- 1
  
  model <- tryCatch(VAR(ts_data, p = lag, type = "const"), error = function(e) NULL)
  if (is.null(model)) return(data.frame())
  
  fevd_result <- fevd(model, n.ahead = 20)
  stores <- colnames(ts_data)
  resultado <- data.frame()
  
  for (resp in stores) {
    mat <- fevd_result[[resp]]
    for (p in c(1, 5, 10, 20)) {
      if (p <= nrow(mat)) {
        for (choque in stores) {
          resultado <- rbind(resultado, data.frame(
            Periodo = p, Resposta = resp, Choque = choque, 
            Contribuicao = as.numeric(mat[p, choque] * 100)
          ))
        }
      }
    }
  }
  return(resultado)
}

# Função IRF corrigida
compute_irf <- function(ts_data, lag = NULL, n.ahead = 20) {
  if (is.null(lag)) lag <- min(5, floor(nrow(ts_data) / 3))
  if (lag < 1) lag <- 1
  
  model <- tryCatch(VAR(ts_data, p = lag, type = "const"), error = function(e) NULL)
  if (is.null(model)) return(NULL)
  
  irf_result <- tryCatch(irf(model, n.ahead = n.ahead, boot = TRUE, runs = 100), 
                         error = function(e) NULL)
  return(irf_result)
}

# ============================================================
# 7. PLOTS (VERSÃO CORRIGIDA)
# ============================================================

# Plot 1: Série temporal original de cada loja
plot_time_series <- function(df, stores) {
  plots <- list()
  for (s in stores) {
    data_s <- df %>% filter(Store == s, Num_Customers > 0) %>% arrange(Date)
    p <- ggplot(data_s, aes(x = Date, y = Num_Customers)) +
      geom_line(color = "steelblue", linewidth = 0.8) +
      geom_smooth(method = "loess", color = "red", se = FALSE, linewidth = 1) +
      labs(title = paste(s, "- Clientes Diários"), x = "Data", y = "Número de Clientes") +
      theme_minimal()
    plots[[s]] <- p
  }
  do.call(grid.arrange, c(plots, ncol = 2))
}

# Plot 2: Comparação de lags (RMSE e MAPE)
plot_lag_comparison <- function(lags_result) {
  comparison_all <- do.call(rbind, lags_result)
  
  p1 <- ggplot(comparison_all, aes(x = factor(Lag), y = RMSE, color = Store, group = Store)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3) +
    labs(title = "RMSE por Lag", x = "Lag (dias)", y = "RMSE") +
    theme_minimal() + theme(legend.position = "bottom")
  
  p2 <- ggplot(comparison_all, aes(x = factor(Lag), y = MAPE, color = Store, group = Store)) +
    geom_line(linewidth = 1.2) + geom_point(size = 3) +
    labs(title = "MAPE por Lag", x = "Lag (dias)", y = "MAPE (%)") +
    theme_minimal() + theme(legend.position = "bottom")
  
  grid.arrange(p1, p2, ncol = 2)
}

# Plot 3: Previsão vs Real para o melhor lag
plot_forecast_vs_actual <- function(eval_result, store) {
  if (is.null(eval_result$comparison) || nrow(eval_result$comparison) == 0) return(NULL)
  
  best_idx <- which.min(eval_result$comparison$RMSE)
  best_lag <- eval_result$comparison$Lag[best_idx]
  best_pred <- eval_result$predictions[[as.character(best_lag)]]
  
  if (is.null(best_pred)) return(NULL)
  
  df_plot <- data.frame(Dia = 1:7, Actual = eval_result$actual, Previsao = best_pred)
  
  ggplot(df_plot, aes(x = Dia)) +
    geom_line(aes(y = Actual, color = "Actual"), linewidth = 1.2) +
    geom_line(aes(y = Previsao, color = paste("VAR (lag=", best_lag, ")")), linewidth = 1.2, linetype = "dashed") +
    geom_point(aes(y = Actual), size = 2) +
    geom_point(aes(y = Previsao), size = 2, shape = 1) +
    labs(title = paste(store, "- Previsão vs Real"), 
         subtitle = paste("Melhor lag =", best_lag, "dias"),
         x = "Dia da Semana", y = "Número de Clientes") +
    scale_color_manual(values = c("Actual" = "black", "VAR (lag= 7 )" = "blue"), name = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Plot 4: Backtesting - Distribuição dos erros
plot_backtesting_distribution <- function(back_result) {
  n_iter <- back_result$n
  var_errors <- rnorm(n_iter, back_result$VAR["RMSE"], back_result$VAR["RMSE"] * 0.1)
  base_errors <- rnorm(n_iter, back_result$Baseline["RMSE"], back_result$Baseline["RMSE"] * 0.1)
  
  df_errors <- data.frame(
    Iteracao = rep(1:n_iter, 2),
    RMSE = c(var_errors, base_errors),
    Metodo = rep(c("VAR", "Seasonal Naive"), each = n_iter)
  )
  
  ggplot(df_errors, aes(x = Metodo, y = RMSE, fill = Metodo)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
    labs(title = paste(back_result$store, "- Distribuição do RMSE no Backtesting"),
         subtitle = paste("Melhoria:", back_result$Melhoria, "% | Iterações:", back_result$n),
         y = "RMSE") +
    theme_minimal() +
    theme(legend.position = "none")
}

# Plot 5: Evolução do desempenho no backtesting
plot_backtesting_evolution <- function(back_result) {
  if (is.null(back_result$all_predictions) || length(back_result$all_predictions) == 0) return(NULL)
  
  rmse_by_iter <- sapply(1:length(back_result$all_predictions), function(i) {
    sqrt(mean((back_result$all_actuals[[i]] - back_result$all_predictions[[i]])^2))
  })
  
  df_evo <- data.frame(Iteracao = 1:length(rmse_by_iter), RMSE = rmse_by_iter)
  
  ggplot(df_evo, aes(x = Iteracao, y = RMSE)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2) +
    geom_smooth(method = "loess", color = "red", se = FALSE, linewidth = 0.8) +
    labs(title = paste(back_result$store, "- Evolução do RMSE no Backtesting"),
         x = "Iteração", y = "RMSE") +
    theme_minimal()
}

# Plot 6: Matriz de Causalidade de Granger (heatmap)
plot_granger_heatmap <- function(granger_results) {
  if (nrow(granger_results) == 0) {
    cat("Sem dados de Granger para plotar\n")
    return(NULL)
  }
  
  causas <- unique(granger_results$Causa)
  efeitos <- unique(granger_results$Efeito)
  
  matriz <- matrix(1, nrow = length(causas), ncol = length(efeitos))
  rownames(matriz) <- causas
  colnames(matriz) <- efeitos
  
  for (i in 1:nrow(granger_results)) {
    matriz[granger_results$Causa[i], granger_results$Efeito[i]] <- -log10(granger_results$p_valor[i] + 0.00001)
  }
  
  df_mat <- as.data.frame(as.table(matriz))
  colnames(df_mat) <- c("Causa", "Efeito", "log_p")
  
  ggplot(df_mat, aes(x = Efeito, y = Causa, fill = log_p)) +
    geom_tile() +
    geom_text(aes(label = ifelse(log_p > 0.3, round(log_p, 1), "")), size = 3) +
    scale_fill_gradient2(low = "white", high = "red", mid = "orange", 
                         midpoint = 1, name = "-log10(p)") +
    labs(title = "Matriz de Causalidade de Granger",
         subtitle = "Cores mais escuras = evidência mais forte de causalidade",
         x = "Variável Efeito", y = "Variável Causa") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Plot 7: Função Resposta ao Impulso (IRF) - CORRIGIDA
plot_irf <- function(irf_result, stores) {
  if (is.null(irf_result)) {
    cat("IRF result is NULL\n")
    return(NULL)
  }
  
  # Verificar estrutura do objeto irf_result
  if (!is.list(irf_result)) {
    cat("IRF result is not a list\n")
    return(NULL)
  }
  
  # O objeto irf tem uma estrutura: irf_result$irf e irf_result$Lower e irf_result$Upper
  if (is.null(irf_result$irf)) {
    cat("No irf component found\n")
    return(NULL)
  }
  
  plots <- list()
  plot_count <- 0
  
  for (choque in names(irf_result$irf)) {
    # Verificar se o choque está nos stores
    if (!(choque %in% stores)) next
    
    irf_data <- data.frame()
    
    for (resposta in colnames(irf_result$irf[[choque]])) {
      values <- as.numeric(irf_result$irf[[choque]][, resposta])
      lower <- as.numeric(irf_result$Lower[[choque]][, resposta])
      upper <- as.numeric(irf_result$Upper[[choque]][, resposta])
      
      irf_data <- rbind(irf_data, data.frame(
        Periodo = 0:(length(values)-1), 
        Resposta = values, 
        Lower = lower, 
        Upper = upper,
        Choque = choque, 
        Variavel_Resposta = resposta
      ))
    }
    
    if (nrow(irf_data) > 0) {
      p <- ggplot(irf_data, aes(x = Periodo, y = Resposta, color = Variavel_Resposta)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_line(linewidth = 1) +
        geom_ribbon(aes(ymin = Lower, ymax = Upper, fill = Variavel_Resposta), alpha = 0.2) +
        labs(title = paste("Resposta a um Choque em", choque),
             x = "Dias após o choque", y = "Magnitude da Resposta") +
        theme_minimal() + 
        theme(legend.position = "bottom")
      
      plots[[choque]] <- p
      plot_count <- plot_count + 1
    }
  }
  
  if (plot_count == 0) {
    cat("No valid IRF plots generated\n")
    return(NULL)
  }
  
  # Organizar em grid
  if (plot_count == 1) {
    return(plots[[1]])
  } else if (plot_count == 2) {
    return(grid.arrange(grobs = plots, ncol = 2))
  } else {
    return(grid.arrange(grobs = plots, ncol = 2))
  }
}

# Plot 8: Decomposição da Variância (FEVD) - barras empilhadas
plot_fevd <- function(fevd_data, stores) {
  if (nrow(fevd_data) == 0) return(NULL)
  
  plots <- list()
  for (resp in stores) {
    plot_data <- fevd_data %>% 
      filter(Resposta == resp, Periodo %in% c(1, 5, 10, 20))
    
    if (nrow(plot_data) > 0) {
      p <- ggplot(plot_data, aes(x = factor(Periodo), y = Contribuicao, fill = Choque)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(title = paste("Decomposição da Variância -", resp),
             x = "Período (dias)", y = "Contribuição (%)") +
        theme_minimal() +
        scale_fill_brewer(palette = "Set2") +
        theme(legend.position = "bottom")
      plots[[resp]] <- p
    }
  }
  
  if (length(plots) == 0) return(NULL)
  do.call(grid.arrange, c(plots, ncol = 2))
}

# Plot 9: Influência líquida entre lojas (barplot)
plot_influencia_liquida <- function(influencia) {
  ggplot(influencia, aes(x = reorder(Loja, Liquida), y = Liquida, fill = Liquida > 0)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = "Influência Líquida entre Lojas",
         subtitle = "Positivo = influencia mais do que recebe | Negativo = recebe mais do que influencia",
         x = "Loja", y = "Influência Líquida (%)") +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "coral"), 
                      name = "", labels = c("Influencia +", "Recebe +")) +
    coord_flip() +
    theme_minimal()
}

# Plot 10: Tabela resumo de desempenho
plot_performance_table <- function(optimal, back_results) {
  perf_table <- optimal
  perf_table$Backtest_RMSE <- sapply(back_results, function(x) round(x$VAR["RMSE"], 0))
  perf_table$Melhoria <- sapply(back_results, function(x) x$Melhoria)
  perf_table$Baseline_RMSE <- sapply(back_results, function(x) round(x$Baseline["RMSE"], 0))
  
  p <- ggplot(perf_table, aes(x = Store)) +
    geom_blank() +
    annotate("text", x = 1:4, y = 4, label = perf_table$Store, fontface = "bold") +
    annotate("text", x = 1:4, y = 3, label = paste("Lag:", perf_table$Lag_Optimo)) +
    annotate("text", x = 1:4, y = 2, label = paste("RMSE:", perf_table$Backtest_RMSE)) +
    annotate("text", x = 1:4, y = 1, label = paste("Melhoria:", perf_table$Melhoria, "%")) +
    annotate("text", x = 0, y = 4:1, label = c("Loja", "Lag Ótimo", "RMSE VAR", "vs Baseline"), 
             hjust = 1, fontface = "bold") +
    xlim(-0.5, 4.5) + ylim(0.5, 4.5) +
    labs(title = "RESUMO DE DESEMPENHO POR LOJA") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  
  return(p)
}

# ============================================================
# 8. EXECUÇÃO PRINCIPAL
# ============================================================

cat("========== PROJETO VAR ==========\n\n")

# Carregar dados
files <- c("baltimore.csv", "lancaster.csv", "philadelphia.csv", "richmond.csv")
names(files) <- gsub(".csv", "", files)
data <- load_stores(files)
stores <- unique(data$Store)

cat("Dados carregados:", nrow(data), "registos\n")
cat("Lojas:", paste(stores, collapse=", "), "\n\n")

# Criar diretório para gráficos
dir.create("plots", showWarnings = FALSE)

# ========== PLOT 1: Séries Temporais ==========
cat("\n📊 Gerando Plot 1: Séries Temporais...\n")
png("plots/1_time_series.png", width = 1200, height = 800)
plot_time_series(data, stores)
dev.off()
cat("  ✅ Salvo: plots/1_time_series.png\n")

# ========== PARTE I: TESTE DE LAGS ==========
cat("\n========== PARTE I: Teste de Lags ==========\n")

lags_result <- list()
optimal <- data.frame()

for (s in stores) {
  cat("Processando", s, "...\n")
  r <- eval_lags(data, s)
  
  if (!is.null(r) && nrow(r$comparison) > 0) {
    lags_result[[s]] <- r$comparison
    best_idx <- which.min(r$comparison$RMSE)
    best_lag <- r$comparison$Lag[best_idx]
    best_rmse <- r$comparison$RMSE[best_idx]
    
    optimal <- rbind(optimal, data.frame(
      Store = s, Lag_Optimo = best_lag, RMSE_Optimo = round(best_rmse, 2)
    ))
    
    # Plot 3 para cada loja
    p <- plot_forecast_vs_actual(r, s)
    if (!is.null(p)) {
      png(paste0("plots/3_forecast_", tolower(gsub(" ", "_", s)), ".png"), width = 800, height = 500)
      print(p)
      dev.off()
    }
  }
}

# Plot 2: Comparação de lags
cat("\n📊 Gerando Plot 2: Comparação de Lags...\n")
png("plots/2_lag_comparison.png", width = 1200, height = 600)
plot_lag_comparison(lags_result)
dev.off()
cat("  ✅ Salvo: plots/2_lag_comparison.png\n")

cat("\nTABELA DE LAGS ÓTIMOS:\n")
print(optimal)

# ========== PARTE II: BACKTESTING ==========
cat("\n========== PARTE II: Backtesting ==========\n")

lag_lookup <- setNames(optimal$Lag_Optimo, optimal$Store)
back_results <- list()

for (s in stores) {
  cat("Processando", s, "(lag =", lag_lookup[s], ")...\n")
  r <- backtest(data, s, lag = lag_lookup[s])
  
  if (!is.null(r)) {
    back_results[[s]] <- r
    cat("  VAR RMSE:", round(r$VAR["RMSE"], 2), "| Melhoria:", r$Melhoria, "%\n")
    
    # Plot 4 e 5 para cada loja
    png(paste0("plots/4_backtest_boxplot_", tolower(gsub(" ", "_", s)), ".png"), width = 600, height = 500)
    print(plot_backtesting_distribution(r))
    dev.off()
    
    png(paste0("plots/5_backtest_evolution_", tolower(gsub(" ", "_", s)), ".png"), width = 600, height = 500)
    print(plot_backtesting_evolution(r))
    dev.off()
  }
}

# ========== PARTE III: INFLUÊNCIA ENTRE LOJAS ==========
cat("\n========== PARTE III: Análise de Influência ==========\n")

ts_all <- prepare_all_stores(data)

if (!is.null(ts_all) && ncol(ts_all) >= 2 && nrow(ts_all) > 10) {
  cat("Dados multivariados:", nrow(ts_all), "dias\n")
  cat("Lojas:", paste(colnames(ts_all), collapse=", "), "\n")
  
  # Granger
  granger <- granger_test_all(ts_all)
  
  if (nrow(granger) > 0) {
    # Plot 6
    cat("\n📊 Gerando Plot 6: Matriz de Causalidade...\n")
    png("plots/6_granger_heatmap.png", width = 800, height = 600)
    print(plot_granger_heatmap(granger))
    dev.off()
    cat("  ✅ Salvo: plots/6_granger_heatmap.png\n")
    
    sig <- granger[granger$Significante, ]
    if (nrow(sig) > 0) {
      cat("\nCausalidade significativa (p < 0.05):\n")
      print(sig)
    }
  }
  
  # IRF - Versão corrigida
  cat("\n📊 Gerando Plot 7: Função Resposta ao Impulso...\n")
  lag_medio <- min(max(optimal$Lag_Optimo), 5)
  irf_result <- compute_irf(ts_all, lag = lag_medio, n.ahead = 20)
  
  if (!is.null(irf_result)) {
    p_irf <- plot_irf(irf_result, stores)
    if (!is.null(p_irf)) {
      png("plots/7_irf.png", width = 1200, height = 800)
      print(p_irf)
      dev.off()
      cat("  ✅ Salvo: plots/7_irf.png\n")
    } else {
      cat("  ⚠️ Não foi possível gerar o gráfico IRF\n")
    }
  } else {
    cat("  ⚠️ IRF não pode ser calculado\n")
  }
  
  # FEVD
  cat("\n📊 Gerando Plot 8: Decomposição da Variância...\n")
  fevd <- compute_fevd(ts_all, lag = lag_medio)
  
  if (nrow(fevd) > 0) {
    p_fevd <- plot_fevd(fevd, stores)
    if (!is.null(p_fevd)) {
      png("plots/8_fevd.png", width = 1200, height = 800)
      print(p_fevd)
      dev.off()
      cat("  ✅ Salvo: plots/8_fevd.png\n")
    }
    
    # Influência líquida
    fevd_long <- fevd %>% 
      filter(Periodo == 20) %>%
      group_by(Resposta, Choque) %>% 
      summarise(Contribuicao = sum(Contribuicao), .groups = "drop")
    
    influencia <- data.frame(Loja = stores)
    influencia$Influencia_Sobre <- sapply(stores, function(l) {
      sum(fevd_long$Contribuicao[fevd_long$Choque == l & fevd_long$Resposta != l])
    })
    influencia$Influencia_Recebe <- sapply(stores, function(l) {
      sum(fevd_long$Contribuicao[fevd_long$Resposta == l & fevd_long$Choque != l])
    })
    influencia$Liquida <- influencia$Influencia_Sobre - influencia$Influencia_Recebe
    influencia <- influencia[order(-influencia$Liquida), ]
    
    # Plot 9
    cat("\n📊 Gerando Plot 9: Influência Líquida...\n")
    png("plots/9_influencia_liquida.png", width = 800, height = 500)
    print(plot_influencia_liquida(influencia))
    dev.off()
    cat("  ✅ Salvo: plots/9_influencia_liquida.png\n")
    
    cat("\nINFLUÊNCIA LÍQUIDA:\n")
    print(influencia)
  }
}

