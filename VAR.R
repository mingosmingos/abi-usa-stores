# ============================================================
# PROJETO: Forecasting Multivariado com VAR
# Inclui:
#   - Seleção de lag ótimo (7, 14, 21, 28) por loja
#   - Causalidade de Granger entre lojas
#   - Fase I: Hold-out (última semana)
#   - Fase II: Backtesting com Growing Window
#   - Exportação de previsões para otimização
# ============================================================

# 1. BIBLIOTECAS
# ============================================================
library(vars)
library(rminer)
library(forecast)
library(dplyr)
library(ggplot2)

source("multi-utils.R")  # autoVAR, forecastVAR — ficheiro do professor

# ============================================================
# 2. FUNÇÕES AUXILIARES
# ============================================================

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

# NMAE normalizado pelo range do teste (como o professor usa)
calc_metrics <- function(actual, pred) {
  ok     <- complete.cases(actual, pred)
  actual <- as.numeric(actual[ok])
  pred   <- as.numeric(pred[ok])
  if (length(actual) == 0) return(c(NMAE = NA, RMSE = NA, MAE = NA))
  rng  <- max(actual) - min(actual)
  nmae <- if (rng > 0) mean(abs(actual - pred)) / rng else NA
  c(NMAE = round(nmae * 100, 2),
    RMSE = round(sqrt(mean((actual - pred)^2)), 2),
    MAE  = round(mean(abs(actual - pred)), 2))
}

# Série temporal com apenas dias com clientes (freq=7)
prepare_ts <- function(store_df, freq = 7) {
  d <- store_df %>%
    filter(Num_Customers > 0) %>%
    arrange(Date) %>%
    select(Num_Customers, Num_Employees, Pct_On_Sale, TouristEventNum) %>%
    na.omit()
  ts(d, frequency = freq)
}

# VAR com lag fixo (sem autoVAR) — usado na seleção de lag
var_fixed_lag <- function(mtr, lag, h) {
  model <- tryCatch(
    VAR(mtr, p = lag, type = "const"),
    error = function(e) NULL
  )
  if (is.null(model)) return(rep(NA, h))
  pred <- tryCatch(
    predict(model, n.ahead = h)$fcst$Num_Customers[, "fcst"],
    error = function(e) rep(NA, h)
  )
  pmax(as.numeric(pred), 0)
}

# ============================================================
# 3. SELEÇÃO DE LAG ÓTIMO (7, 14, 21, 28) POR LOJA
# ============================================================
# Estratégia: hold-out da última semana, escolhe o lag com menor NMAE

select_best_lag <- function(store_ts, store_name, lags = c(7, 14, 21, 28), h = 7) {
  n   <- nrow(store_ts)
  mtr <- ts(store_ts[1:(n - h), ], frequency = frequency(store_ts))
  test_actual <- as.numeric(store_ts[(n - h + 1):n, "Num_Customers"])
  
  results <- data.frame(
    Lag  = integer(),
    NMAE = numeric(),
    RMSE = numeric(),
    MAE  = numeric()
  )
  
  for (lag in lags) {
    # Garantir que o treino tem dados suficientes para este lag
    if (nrow(mtr) <= lag + h) {
      cat("    Lag", lag, "— dados insuficientes, a saltar.\n")
      next
    }
    
    pred <- var_fixed_lag(mtr, lag, h)
    
    if (anyNA(pred)) {
      cat("    Lag", lag, "— modelo falhou.\n")
      next
    }
    
    m <- calc_metrics(test_actual, pred)
    cat("    Lag", lag, "— NMAE:", m["NMAE"], "% | RMSE:", m["RMSE"], "\n")
    results <- rbind(results, data.frame(Lag = lag, NMAE = m["NMAE"],
                                         RMSE = m["RMSE"], MAE = m["MAE"]))
  }
  
  if (nrow(results) == 0) {
    cat("  [AVISO] Nenhum lag funcionou para", store_name, "\n")
    return(list(best_lag = 7, table = results))  # fallback
  }
  
  best_row <- results[which.min(results$NMAE), ]
  best_lag <- best_row$Lag
  
  cat("\n  >>> Melhor lag para", store_name, ":", best_lag,
      "(NMAE =", best_row$NMAE, "%)\n\n")
  
  # Gráfico comparativo dos lags
  # (previsões de cada lag vs actual)
  plot_df <- data.frame(Dia = 1:h, Actual = test_actual)
  for (lag in results$Lag) {
    pred_col <- var_fixed_lag(mtr, lag, h)
    if (!anyNA(pred_col)) {
      plot_df[[paste0("Lag", lag)]] <- pred_col
    }
  }
  
  # Formato longo para ggplot
  plot_long <- tidyr::pivot_longer(plot_df, cols = -Dia,
                                   names_to = "Serie", values_to = "Valor")
  # Destaque para o melhor lag
  plot_long$Tipo <- ifelse(plot_long$Serie == "Actual", "Actual",
                           ifelse(plot_long$Serie == paste0("Lag", best_lag), "Melhor", "Outro"))
  
  p <- ggplot(plot_long, aes(x = Dia, y = Valor,
                             color = Serie, linetype = Tipo, linewidth = Tipo)) +
    geom_line() +
    scale_linetype_manual(values = c("Actual" = "solid", "Melhor" = "dashed", "Outro" = "dotted")) +
    scale_linewidth_manual(values = c("Actual" = 1.2, "Melhor" = 1.1, "Outro" = 0.6)) +
    labs(title  = paste("Seleção de Lag —", store_name),
         subtitle = paste("Melhor lag:", best_lag, "(menor NMAE)"),
         x = "Dia", y = "Nº Clientes") +
    theme_minimal() +
    theme(legend.title = element_blank())
  print(p)
  
  # Tabela de resultados por lag
  rownames(results) <- NULL
  cat("  Tabela de lags para", store_name, ":\n")
  print(results)
  cat("\n")
  
  list(best_lag = best_lag, table = results)
}

# ============================================================
# 4. CAUSALIDADE DE GRANGER ENTRE LOJAS
# ============================================================
# Idea: criar uma série multivariada com os Num_Customers das 4 lojas
# e testar se cada loja "Granger-causa" cada outra

granger_between_stores <- function(data_all, stores, lag = 7) {
  cat("\n========== CAUSALIDADE DE GRANGER ENTRE LOJAS ==========\n")
  cat("(lag usado:", lag, "| p-value < 0.05 indica causalidade significativa)\n\n")
  
  # Construir data.frame com uma coluna por loja, alinhadas por data
  # Usar apenas dias com clientes > 0 em TODAS as lojas (interseção de datas)
  store_series <- lapply(stores, function(s) {
    data_all %>%
      filter(Store == s, Num_Customers > 0) %>%
      arrange(Date) %>%
      select(Date, Num_Customers) %>%
      rename(!!s := Num_Customers)
  })
  
  # Join por data (interseção)
  combined <- Reduce(function(a, b) inner_join(a, b, by = "Date"), store_series)
  
  if (nrow(combined) < lag + 20) {
    cat("[AVISO] Dados insuficientes para Granger com lag", lag, "\n")
    return(invisible(NULL))
  }
  
  # Matriz apenas com as colunas das lojas
  mat <- as.matrix(combined[, stores])
  
  # Estimar modelo VAR com o lag escolhido
  var_model <- tryCatch(
    VAR(mat, p = lag, type = "const"),
    error = function(e) { cat("[ERRO VAR Granger]", conditionMessage(e), "\n"); NULL }
  )
  
  if (is.null(var_model)) return(invisible(NULL))
  
  # Testar Granger para todos os pares: "causa" -> "efeito"
  # causality() da biblioteca vars testa se X causa Y (e instantaneous causality)
  pvalue_matrix <- matrix(NA, nrow = length(stores), ncol = length(stores),
                          dimnames = list(paste("Causa:", stores),
                                          paste("Efeito:", stores)))
  
  for (causa in stores) {
    other <- setdiff(stores, causa)
    res <- tryCatch(
      causality(var_model, cause = causa),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      pval <- res$Granger$p.value
      # pval é um p-value único para "causa causa TODAS as outras"
      # Guardar na linha da loja causa (para todas as outras em conjunto)
      for (efeito in other) {
        pvalue_matrix[paste("Causa:", causa), paste("Efeito:", efeito)] <- round(pval, 4)
      }
    }
  }
  
  # Mostrar matriz de p-values
  cat("Matriz de p-values (Granger):\n")
  print(pvalue_matrix)
  
  cat("\nInterpretação (p < 0.05 = causalidade significativa):\n")
  for (causa in stores) {
    res <- tryCatch(causality(var_model, cause = causa), error = function(e) NULL)
    if (!is.null(res)) {
      pval <- res$Granger$p.value
      other <- paste(setdiff(stores, causa), collapse = ", ")
      sig <- if (pval < 0.05) "SIM ✓" else "não"
      cat(sprintf("  %-15s -> {%s} : p=%.4f  [%s]\n", causa, other, pval, sig))
    }
  }
  
  # Também testar pares individuais com grangertest() do pacote lmtest
  # para saber QUAL loja específica influencia qual
  if (requireNamespace("lmtest", quietly = TRUE)) {
    cat("\nTeste de Granger por par (lmtest::grangertest):\n")
    cat(sprintf("  %-15s  %-15s  %s\n", "Causa", "Efeito", "p-value"))
    cat(strrep("-", 45), "\n")
    
    for (causa in stores) {
      for (efeito in setdiff(stores, causa)) {
        gt <- tryCatch(
          lmtest::grangertest(combined[[efeito]] ~ combined[[causa]], order = lag),
          error = function(e) NULL
        )
        if (!is.null(gt)) {
          pval <- gt[2, "Pr(>F)"]
          sig  <- if (!is.na(pval) && pval < 0.05) " *** significativo" else ""
          cat(sprintf("  %-15s -> %-15s : p=%.4f%s\n", causa, efeito, pval, sig))
        }
      }
    }
  } else {
    cat("\n[NOTA] Para testes par-a-par instala: install.packages('lmtest')\n")
  }
  
  invisible(pvalue_matrix)
}

# ============================================================
# 5. FASE I — Hold-out com melhor lag
# ============================================================

phase1_holdout <- function(store_ts, store_name, best_lag, h = 7) {
  n       <- nrow(store_ts)
  mtr     <- ts(store_ts[1:(n - h), ], frequency = frequency(store_ts))
  actual  <- as.numeric(store_ts[(n - h + 1):n, "Num_Customers"])
  
  pred <- var_fixed_lag(mtr, best_lag, h)
  
  # Seasonal Naive baseline
  train_customers <- as.numeric(mtr[, "Num_Customers"])
  baseline        <- tail(train_customers, h)
  
  m_var  <- calc_metrics(actual, pred)
  m_base <- calc_metrics(actual, baseline)
  
  cat("\n  Loja:", store_name, "| Lag:", best_lag, "\n")
  cat("  VAR  — NMAE:", m_var["NMAE"], "% | RMSE:", m_var["RMSE"], "\n")
  cat("  Naive— NMAE:", m_base["NMAE"], "% | RMSE:", m_base["RMSE"], "\n")
  
  if (!is.na(m_var["RMSE"]) && !is.na(m_base["RMSE"])) {
    melhoria <- round((1 - m_var["RMSE"] / m_base["RMSE"]) * 100, 1)
    cat("  Melhoria RMSE face ao Seasonal Naive:", melhoria, "%\n")
  }
  
  # Gráfico
  plot_df <- data.frame(Dia = 1:h, Actual = actual, VAR = pred, Naive = baseline)
  p <- ggplot(plot_df, aes(x = Dia)) +
    geom_line(aes(y = Actual, color = "Actual"),         linewidth = 1.1) +
    geom_line(aes(y = VAR,    color = "VAR"),            linewidth = 1,   linetype = "dashed") +
    geom_line(aes(y = Naive,  color = "Seasonal Naive"), linewidth = 0.8, linetype = "dotted") +
    scale_color_manual(values = c("Actual" = "black", "VAR" = "steelblue", "Seasonal Naive" = "tomato")) +
    labs(title    = paste("Fase I — Hold-out:", store_name),
         subtitle = paste("VAR com lag =", best_lag),
         x = "Dia", y = "Nº Clientes", color = "") +
    theme_minimal()
  print(p)
  
  list(store = store_name, lag = best_lag, pred = pred, actual = actual,
       metrics_var = m_var, metrics_base = m_base)
}

# ============================================================
# 6. FASE II — Backtesting com Growing Window e melhor lag
# ============================================================

phase2_growing <- function(store_ts, store_name, best_lag, h = 7, n_iter = 20) {
  n         <- nrow(store_ts)
  min_train <- best_lag + h + 5   # mínimo seguro para VAR com este lag
  
  needed <- min_train + n_iter * h
  if (n < needed) {
    n_iter <- max(1, floor((n - min_train) / h))
    cat("  [INFO] Reduzindo para", n_iter, "iterações em", store_name, "\n")
  }
  
  results_var  <- list()
  results_base <- list()
  
  for (i in 1:n_iter) {
    train_end  <- n - (n_iter - i) * h - h    # growing window
    test_start <- train_end + 1
    test_end   <- train_end + h
    
    if (train_end < min_train) next
    if (test_end > n)          next
    
    mtr    <- ts(store_ts[1:train_end, ], frequency = frequency(store_ts))
    actual <- as.numeric(store_ts[test_start:test_end, "Num_Customers"])
    
    if (length(actual) < h) next
    
    pred <- tryCatch(var_fixed_lag(mtr, best_lag, h), error = function(e) rep(NA, h))
    
    train_customers <- as.numeric(mtr[, "Num_Customers"])
    baseline        <- tail(train_customers, h)
    
    if (!anyNA(pred)) {
      results_var[[length(results_var) + 1]]   <- calc_metrics(actual, pred)
      results_base[[length(results_base) + 1]] <- calc_metrics(actual, baseline)
    }
  }
  
  if (length(results_var) == 0) {
    cat("  [AVISO] Nenhuma iteração válida para", store_name, "\n")
    return(NULL)
  }
  
  mat_var  <- do.call(rbind, results_var)
  mat_base <- do.call(rbind, results_base)
  med_var  <- apply(mat_var,  2, median, na.rm = TRUE)
  med_base <- apply(mat_base, 2, median, na.rm = TRUE)
  
  cat("\n  Loja:", store_name, "| Lag:", best_lag,
      "| Iterações válidas:", length(results_var), "/", n_iter, "\n")
  cat("  VAR  (mediana) — NMAE:", round(med_var["NMAE"],  2),
      "% | RMSE:", round(med_var["RMSE"],  2), "\n")
  cat("  Naive(mediana) — NMAE:", round(med_base["NMAE"], 2),
      "% | RMSE:", round(med_base["RMSE"], 2), "\n")
  
  list(store        = store_name,
       lag          = best_lag,
       metrics_var  = med_var,
       metrics_base = med_base,
       n_ok         = length(results_var))
}

# ============================================================
# 7. EXECUÇÃO PRINCIPAL
# ============================================================

files <- c(
  baltimore    = "baltimore.csv",
  lancaster    = "lancaster.csv",
  philadelphia = "philadelphia.csv",
  richmond     = "richmond.csv"
)
data_all <- load_stores(files)
stores   <- unique(data_all$Store)

cat("=== Dados carregados ===\n")
cat("Total de registos:", nrow(data_all), "\n")
print(table(data_all$Store))

# Séries temporais por loja
store_ts_list <- lapply(stores, function(s) {
  prepare_ts(data_all %>% filter(Store == s), freq = 7)
})
names(store_ts_list) <- stores

# ============================================================
# SELEÇÃO DE LAG
# ============================================================
cat("\n\n========== SELEÇÃO DE LAG ÓTIMO (7, 14, 21, 28) ==========\n")

best_lags <- list()
lag_tables <- list()

for (s in stores) {
  cat("\n--- Loja:", s, "---\n")
  res <- select_best_lag(store_ts_list[[s]], s, lags = c(7, 14, 21, 28), h = 7)
  best_lags[[s]]  <- res$best_lag
  lag_tables[[s]] <- res$table
}

# Tabela resumo dos melhores lags
cat("\n========== RESUMO: MELHOR LAG POR LOJA ==========\n")
lag_summary <- data.frame(
  Loja     = stores,
  MelhorLag = unlist(best_lags),
  row.names = NULL
)
# Juntar NMAE do melhor lag
for (i in seq_along(stores)) {
  s   <- stores[i]
  bl  <- best_lags[[s]]
  tbl <- lag_tables[[s]]
  nmae_best <- tbl$NMAE[tbl$Lag == bl]
  lag_summary$NMAE_Melhor[i] <- if (length(nmae_best) > 0) nmae_best else NA
}
print(lag_summary)

# ============================================================
# CAUSALIDADE DE GRANGER ENTRE LOJAS
# ============================================================
# Usar a mediana dos melhores lags como lag para o VAR entre lojas
lag_granger <- as.integer(median(unlist(best_lags)))
cat("\n(lag usado no VAR entre lojas:", lag_granger, ")\n")
granger_between_stores(data_all, stores, lag = lag_granger)

# ============================================================
# FASE I
# ============================================================
cat("\n\n========== FASE I (Hold-out — última semana) ==========\n")
results1 <- lapply(stores, function(s) {
  phase1_holdout(store_ts_list[[s]], s, best_lags[[s]], h = 7)
})
names(results1) <- stores

# ============================================================
# FASE II
# ============================================================
cat("\n\n========== FASE II (Backtesting — Growing Window) ==========\n")
results2 <- lapply(stores, function(s) {
  phase2_growing(store_ts_list[[s]], s, best_lags[[s]], h = 7, n_iter = 20)
})
names(results2) <- stores

# Tabela resumo Fase II
cat("\n\n========== TABELA RESUMO FASE II ==========\n")
summary_rows <- lapply(stores, function(s) {
  r <- results2[[s]]
  if (is.null(r)) return(NULL)
  data.frame(
    Loja         = s,
    Lag          = r$lag,
    NMAE_VAR     = round(r$metrics_var["NMAE"],  2),
    RMSE_VAR     = round(r$metrics_var["RMSE"],  2),
    NMAE_Naive   = round(r$metrics_base["NMAE"], 2),
    RMSE_Naive   = round(r$metrics_base["RMSE"], 2),
    Melhoria_pct = round((1 - r$metrics_var["RMSE"] / r$metrics_base["RMSE"]) * 100, 1),
    Iteracoes    = r$n_ok,
    row.names    = NULL
  )
})
summary_df <- do.call(rbind, Filter(Negate(is.null), summary_rows))
print(summary_df)

# ============================================================
# EXPORTAR PREVISÕES PARA OTIMIZAÇÃO
# ============================================================
cat("\n\n========== EXPORTAR PREVISÕES ==========\n")

all_rows <- list()
for (s in stores) {
  cat("  Processando", s, "...\n")
  sts       <- store_ts_list[[s]]
  n         <- nrow(sts)
  lag       <- best_lags[[s]]
  min_train <- lag + 7 + 5
  n_weeks   <- 20
  h         <- 7
  
  for (i in 1:n_weeks) {
    train_end <- n - (n_weeks - i) * h - h
    if (train_end < min_train) next
    if (train_end + h > n)     next
    
    mtr  <- ts(sts[1:train_end, ], frequency = frequency(sts))
    pred <- tryCatch(var_fixed_lag(mtr, lag, h), error = function(e) rep(NA, h))
    
    if (!anyNA(pred)) {
      all_rows[[length(all_rows) + 1]] <- data.frame(
        Store    = s,
        Week_ID  = i,
        Day      = 1:h,
        Lag_Used = lag,
        Forecast = round(pred, 0)
      )
    }
  }
}

if (length(all_rows) > 0) {
  forecasts_df <- do.call(rbind, all_rows)
  write.csv(forecasts_df, "forecasts_var.csv", row.names = FALSE)
  cat("Previsões guardadas em 'forecasts_var.csv' (", nrow(forecasts_df), "linhas)\n")
  print(head(forecasts_df, 14))
} else {
  cat("[AVISO] Não foi possível gerar previsões.\n")
}

cat("\n========== FIM ==========\n")