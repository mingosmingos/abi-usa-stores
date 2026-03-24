# ============================================================
# ANALISE COMPARATIVA DOS RESULTADOS DO VAR ENTRE LOJAS
# Script independente - carrega resultados do ficheiro CSV
# ============================================================

# 1. BIBLIOTECAS
# ============================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)

# 2. CARREGAR DADOS
# ============================================================
cat("========== ANALISE COMPARATIVA DOS RESULTADOS VAR ==========\n\n")

# Carregar previsões geradas pelo VAR
if (file.exists("forecasts_var.csv")) {
  forecasts <- read.csv("forecasts_var.csv", stringsAsFactors = FALSE)
  cat("Ficheiro forecasts_var.csv carregado com sucesso!\n")
  cat("Registos:", nrow(forecasts), "\n")
  cat("Lojas presentes:", paste(unique(forecasts$Store), collapse = ", "), "\n\n")
} else {
  cat("ERRO: ficheiro forecasts_var.csv nao encontrado!\n")
  cat("Execute primeiro o script VAR.R para gerar as previsoes.\n")
  stop()
}

# Se tivermos também os dados originais para comparar com real
if (file.exists("baltimore.csv")) {
  # Carregar dados originais para obter valores reais
  load_original <- function(file, store_name) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$Store <- store_name
    df$Date <- as.Date(df$Date)
    return(df)
  }
  
  baltimore <- load_original("baltimore.csv", "Baltimore")
  lancaster <- load_original("lancaster.csv", "Lancaster")
  philadelphia <- load_original("philadelphia.csv", "Philadelphia")
  richmond <- load_original("richmond.csv", "Richmond")
  
  original <- bind_rows(baltimore, lancaster, philadelphia, richmond)
  cat("Dados originais carregados para comparacao com valores reais.\n\n")
  has_original <- TRUE
} else {
  cat("AVISO: Dados originais nao encontrados. Comparacao limitada.\n")
  has_original <- FALSE
}

# ============================================================
# 3. ESTATISTICAS DAS PREVISOES POR LOJA
# ============================================================

cat("3.1 ESTATISTICAS DAS PREVISOES VAR\n")
cat("========================================\n")

forecast_stats <- forecasts %>%
  group_by(Store) %>%
  summarise(
    N_Previsoes = n(),
    Semanas_Unicas = n_distinct(Week_ID),
    Forecast_Media = mean(Forecast, na.rm = TRUE),
    Forecast_Mediana = median(Forecast, na.rm = TRUE),
    Forecast_Min = min(Forecast, na.rm = TRUE),
    Forecast_Max = max(Forecast, na.rm = TRUE),
    Forecast_Desvio = sd(Forecast, na.rm = TRUE)
  )

print(forecast_stats)

# ============================================================
# 4. GRAFICO 1: DISTRIBUICAO DAS PREVISOES POR LOJA
# ============================================================

cat("\n4.1 Gerando grafico: Distribuicao das previsoes...\n")

p1 <- ggplot(forecasts, aes(x = Store, y = Forecast, fill = Store)) +
  geom_boxplot() +
  labs(title = "Distribuicao das Previsoes VAR por Loja",
       subtitle = "Previsoes para as ultimas 20 semanas",
       x = "Loja", y = "Previsao de Clientes") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)

# ============================================================
# 5. GRAFICO 2: EVOLUCAO DAS PREVISOES AO LONGO DAS SEMANAS
# ============================================================

cat("\n5.1 Gerando grafico: Evolucao semanal das previsoes...\n")

weekly_avg <- forecasts %>%
  group_by(Store, Week_ID) %>%
  summarise(Forecast_Medio = mean(Forecast, na.rm = TRUE), .groups = "drop")

p2 <- ggplot(weekly_avg, aes(x = Week_ID, y = Forecast_Medio, color = Store)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(title = "Evolucao das Previsoes Medias por Semana",
       subtitle = "Cada ponto representa a media da semana para cada loja",
       x = "Semana (1 = mais recente, 20 = mais antiga)", 
       y = "Previsao Media de Clientes") +
  theme_minimal()

print(p2)

# ============================================================
# 6. GRAFICO 3: DENSIDADE DAS PREVISOES
# ============================================================

cat("\n6.1 Gerando grafico: Densidade das previsoes...\n")

p3 <- ggplot(forecasts, aes(x = Forecast, fill = Store)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidade das Previsoes VAR por Loja",
       x = "Previsao de Clientes", y = "Densidade") +
  theme_minimal()

print(p3)

# ============================================================
# 7. GRAFICO 4: HEATMAP DE PREVISOES POR DIA DA SEMANA
# ============================================================

cat("\n7.1 Gerando grafico: Heatmap por dia da semana...\n")

# Adicionar dia da semana (assumindo que Week_Start é segunda-feira)
forecasts$DayName <- factor(forecasts$Day, 
                            levels = 1:7,
                            labels = c("Segunda", "Terca", "Quarta", 
                                       "Quinta", "Sexta", "Sabado", "Domingo"))

p4 <- ggplot(forecasts, aes(x = Store, y = DayName, fill = Forecast)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", name = "Clientes") +
  labs(title = "Heatmap de Previsoes por Loja e Dia da Semana",
       x = "Loja", y = "Dia da Semana") +
  theme_minimal()

print(p4)

# ============================================================
# 8. GRAFICO 5: VARIANCIA DAS PREVISOES (se tivermos dados reais)
# ============================================================

if (has_original) {
  cat("\n8.1 Gerando grafico: Comparacao com valores reais...\n")
  
  # Tentar alinhar previsoes com valores reais
  # Para cada previsao, encontrar a semana correspondente nos dados originais
  forecasts$Week_Start_Date <- as.Date(forecasts$Week_Start)
  
  # Criar coluna de data para cada dia previsto
  forecasts$Forecast_Date <- forecasts$Week_Start_Date + (forecasts$Day - 1)
  
  # Juntar com dados reais - usar dplyr::select explicitamente
  original_filtered <- original %>%
    dplyr::select(Date, Store, Num_Customers) %>%
    dplyr::rename(Real = Num_Customers)
  
  comparison <- forecasts %>%
    left_join(original_filtered, by = c("Forecast_Date" = "Date", "Store" = "Store"))
  
  # Remover NAs
  comparison <- comparison %>% filter(!is.na(Real))
  
  if (nrow(comparison) > 0) {
    # Calcular erros
    comparison <- comparison %>%
      mutate(Erro = Forecast - Real,
             Erro_Abs = abs(Erro),
             Erro_Pct = (Erro / Real) * 100)
    
    # Estatisticas de erro por loja
    error_stats <- comparison %>%
      group_by(Store) %>%
      summarise(
        N = n(),
        RMSE = sqrt(mean(Erro^2, na.rm = TRUE)),
        MAE = mean(Erro_Abs, na.rm = TRUE),
        MAPE = mean(abs(Erro_Pct), na.rm = TRUE),
        Bias = mean(Erro, na.rm = TRUE)
      )
    
    cat("\nEstatisticas de Erro por Loja:\n")
    print(error_stats)
    
    # Grafico 5: RMSE por loja
    p5 <- ggplot(error_stats, aes(x = Store, y = RMSE, fill = Store)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(RMSE, 0)), vjust = -0.5, size = 4) +
      labs(title = "RMSE das Previsoes VAR por Loja",
           subtitle = "Quanto menor, melhor o desempenho do modelo",
           x = "Loja", y = "RMSE") +
      theme_minimal() +
      theme(legend.position = "none")
    
    print(p5)
    
    # Grafico 6: MAE por loja
    p6 <- ggplot(error_stats, aes(x = Store, y = MAE, fill = Store)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(MAE, 0)), vjust = -0.5, size = 4) +
      labs(title = "MAE das Previsoes VAR por Loja",
           subtitle = "Erro absoluto medio",
           x = "Loja", y = "MAE") +
      theme_minimal() +
      theme(legend.position = "none")
    
    print(p6)
    
    # Grafico 7: MAPE por loja
    p7 <- ggplot(error_stats, aes(x = Store, y = MAPE, fill = Store)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(MAPE, 1)), vjust = -0.5, size = 4) +
      labs(title = "MAPE das Previsoes VAR por Loja (%)",
           subtitle = "Erro percentual medio - quanto menor, melhor",
           x = "Loja", y = "MAPE (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    print(p7)
    
    # Grafico 8: Scatter plot Previsao vs Real
    p8 <- ggplot(comparison, aes(x = Real, y = Forecast, color = Store)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
      labs(title = "Previsao VAR vs Valores Reais",
           subtitle = "Pontos na linha diagonal indicam previsoes perfeitas",
           x = "Valores Reais (Clientes)", y = "Previsao VAR (Clientes)") +
      theme_minimal()
    
    print(p8)
    
    # Grafico 9: Boxplot dos erros por loja
    p9 <- ggplot(comparison, aes(x = Store, y = Erro, fill = Store)) +
      geom_boxplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Distribuicao dos Erros por Loja",
           subtitle = "Erro = Previsao - Real (valores positivos = sobrestimacao)",
           x = "Loja", y = "Erro (clientes)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    print(p9)
    
  } else {
    cat("Nao foi possivel alinhar previsoes com dados reais.\n")
  }
}

# ============================================================
# 9. GRAFICO 10: TABELA RESUMO
# ============================================================

cat("\n9.1 Gerando tabela resumo...\n")

# Criar tabela resumo final
if (has_original && exists("error_stats") && nrow(error_stats) > 0) {
  summary_table <- forecast_stats %>%
    left_join(error_stats, by = "Store") %>%
    dplyr::select(Store, N_Previsoes, Forecast_Media, RMSE, MAE, MAPE, Bias)
  
  cat("\nTABELA RESUMO FINAL:\n")
  print(summary_table)
  
  # Salvar tabela
  write.csv(summary_table, "comparacao_resultados_var.csv", row.names = FALSE)
  cat("\nTabela salva em 'comparacao_resultados_var.csv'\n")
} else {
  write.csv(forecast_stats, "comparacao_resultados_var.csv", row.names = FALSE)
  cat("\nTabela salva em 'comparacao_resultados_var.csv'\n")
}

# ============================================================
# 10. GRAFICO 11: RANKING DE DESEMPENHO
# ============================================================

if (has_original && exists("error_stats") && nrow(error_stats) > 0) {
  cat("\n10.1 Gerando ranking de desempenho...\n")
  
  # Ranking por RMSE (melhor = menor RMSE)
  ranking <- error_stats %>%
    arrange(RMSE) %>%
    mutate(Rank_RMSE = row_number(),
           Rank_MAE = rank(MAE),
           Rank_MAPE = rank(MAPE))
  
  cat("\nRANKING POR RMSE (1 = melhor desempenho):\n")
  ranking %>%
    dplyr::select(Store, RMSE, Rank_RMSE) %>%
    arrange(Rank_RMSE) %>%
    print()
  
  # Grafico de ranking
  ranking_long <- ranking %>%
    dplyr::select(Store, RMSE, MAE, MAPE) %>%
    pivot_longer(cols = c(RMSE, MAE, MAPE), names_to = "Metrica", values_to = "Valor")
  
  p10 <- ggplot(ranking_long, aes(x = reorder(Store, Valor), y = Valor, fill = Metrica)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Comparacao de Metricas de Erro por Loja",
         subtitle = "Valores mais baixos indicam melhor desempenho",
         x = "Loja", y = "Valor do Erro") +
    theme_minimal() +
    scale_fill_manual(values = c("RMSE" = "#1f77b4", "MAE" = "#ff7f0e", "MAPE" = "#2ca02c"))
  
  print(p10)
}

# ============================================================
# 11. RESUMO FINAL NO CONSOLE
# ============================================================

cat("\n\n========== RESUMO FINAL ==========\n")
cat("Lojas analisadas:", paste(unique(forecasts$Store), collapse = ", "), "\n")
cat("Total de previsoes:", nrow(forecasts), "\n")

if (has_original && exists("error_stats") && nrow(error_stats) > 0) {
  cat("\nDESEMPENHO DO VAR POR LOJA:\n")
  cat("----------------------------------------\n")
  for (i in 1:nrow(error_stats)) {
    cat(sprintf("%s: RMSE=%.0f, MAE=%.0f, MAPE=%.1f%%\n",
                error_stats$Store[i],
                error_stats$RMSE[i],
                error_stats$MAE[i],
                error_stats$MAPE[i]))
  }
  
  best <- error_stats %>% filter(RMSE == min(RMSE))
  cat("\nMELHOR DESEMPENHO:", best$Store[1], 
      "(RMSE =", round(best$RMSE[1], 0), ")\n")
  
  worst <- error_stats %>% filter(RMSE == max(RMSE))
  cat("PIOR DESEMPENHO:", worst$Store[1], 
      "(RMSE =", round(worst$RMSE[1], 0), ")\n")
}

cat("\n========== FIM ==========\n")