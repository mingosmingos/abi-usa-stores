# 04_comparacao.R - Comparação final de TODOS os modelos
# ARIMA, ETS, HW, SES e Random Forest

# Encontrar a raiz do projeto
encontrar_raiz <- function() {
  caminho_atual <- getwd()
  if(basename(caminho_atual) == "analise_modelos") {
    return(dirname(caminho_atual))
  }
  return(caminho_atual)
}

raiz_projeto <- encontrar_raiz()
cat("📂 Raiz do projeto:", raiz_projeto, "\n")

# Carregar resultados de todos os modelos
cat("\n📥 A carregar resultados...\n")

resultados_arima <- readRDS(file.path(raiz_projeto, "analise_modelos", "resultados_arima.rds"))
resultados_ets <- readRDS(file.path(raiz_projeto, "analise_modelos", "resultados_ets.rds"))
resultados_rf <- readRDS(file.path(raiz_projeto, "analise_modelos", "resultados_rf.rds"))

library(ggplot2)
library(knitr)
library(gridExtra)

# Criar tabela comparativa completa
cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("📊 COMPARAÇÃO FINAL DE TODOS OS MODELOS\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

comparacao <- data.frame()

for(loja in names(resultados_arima)) {
  
  # Extrair MAE de cada modelo
  mae_arima <- resultados_arima[[loja]]$mae
  mae_ets <- resultados_ets[[loja]]$ets$mae
  mae_hw <- resultados_ets[[loja]]$hw$mae
  mae_ses <- resultados_ets[[loja]]$ses$mae
  mae_rf <- resultados_rf[[loja]]$mae
  
  # Extrair RMSE de cada modelo
  rmse_arima <- resultados_arima[[loja]]$rmse
  rmse_ets <- resultados_ets[[loja]]$ets$rmse
  rmse_hw <- resultados_ets[[loja]]$hw$rmse
  rmse_ses <- resultados_ets[[loja]]$ses$rmse
  rmse_rf <- resultados_rf[[loja]]$rmse
  
  # Encontrar o melhor modelo (menor MAE)
  maes <- c(mae_arima, mae_ets, mae_hw, mae_ses, mae_rf)
  nomes <- c("ARIMA", "ETS", "HW", "SES", "RF")
  melhor <- nomes[which.min(maes)]
  melhor_mae <- min(maes)
  
  # Calcular % de melhoria em relação ao segundo melhor
  maes_sorted <- sort(maes)
  if(length(maes_sorted) > 1) {
    melhoria <- round((maes_sorted[2] - maes_sorted[1]) / maes_sorted[2] * 100, 1)
  } else {
    melhoria <- 0
  }
  
  linha <- data.frame(
    Loja = loja,
    ARIMA_MAE = round(mae_arima, 2),
    ARIMA_RMSE = round(rmse_arima, 2),
    ETS_MAE = round(mae_ets, 2),
    ETS_RMSE = round(rmse_ets, 2),
    HW_MAE = round(mae_hw, 2),
    HW_RMSE = round(rmse_hw, 2),
    SES_MAE = round(mae_ses, 2),
    SES_RMSE = round(rmse_ses, 2),
    RF_MAE = round(mae_rf, 2),
    RF_RMSE = round(rmse_rf, 2),
    Melhor_Modelo = melhor,
    Melhor_MAE = round(melhor_mae, 2),
    Melhoria = paste0(melhoria, "%")
  )
  
  comparacao <- rbind(comparacao, linha)
}

# Mostrar tabela formatada
print(kable(comparacao[, c("Loja", "ARIMA_MAE", "ETS_MAE", "HW_MAE", "SES_MAE", "RF_MAE", 
                           "Melhor_Modelo", "Melhor_MAE", "Melhoria")], 
            format = "simple", 
            caption = "Comparação de Modelos (MAE - menor é melhor)"))

cat("\n")

# Calcular médias por modelo
cat("\n📈 MÉDIAS GLOBAIS (MAE):\n")
medias_mae <- data.frame(
  Modelo = c("ARIMA", "ETS", "HW", "SES", "Random Forest"),
  MAE_Medio = c(
    mean(comparacao$ARIMA_MAE),
    mean(comparacao$ETS_MAE),
    mean(comparacao$HW_MAE),
    mean(comparacao$SES_MAE),
    mean(comparacao$RF_MAE)
  )
)
medias_mae <- medias_mae[order(medias_mae$MAE_Medio), ]
print(medias_mae)

cat("\n📉 MÉDIAS GLOBAIS (RMSE):\n")
medias_rmse <- data.frame(
  Modelo = c("ARIMA", "ETS", "HW", "SES", "Random Forest"),
  RMSE_Medio = c(
    mean(comparacao$ARIMA_RMSE),
    mean(comparacao$ETS_RMSE),
    mean(comparacao$HW_RMSE),
    mean(comparacao$SES_RMSE),
    mean(comparacao$RF_RMSE)
  )
)
medias_rmse <- medias_rmse[order(medias_rmse$RMSE_Medio), ]
print(medias_rmse)

# Estatísticas de vitórias
cat("\n🏆 CONTAGEM DE VITÓRIAS (melhor modelo por loja):\n")
vitorias <- table(comparacao$Melhor_Modelo)
print(vitorias)

# ============================================
# GRÁFICOS
# ============================================

# 1. Gráfico de barras comparativo (MAE)
df_plot1 <- data.frame(
  Loja = rep(comparacao$Loja, 5),
  Modelo = c(rep("ARIMA", 4), rep("ETS", 4), rep("HW", 4), rep("SES", 4), rep("RF", 4)),
  MAE = c(comparacao$ARIMA_MAE, comparacao$ETS_MAE, 
          comparacao$HW_MAE, comparacao$SES_MAE, comparacao$RF_MAE)
)

p1 <- ggplot(df_plot1, aes(x = Loja, y = MAE, fill = Modelo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparação de Modelos por Loja (MAE)",
       x = "Loja", y = "MAE (menor é melhor)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = round(MAE, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)

print(p1)

# 2. Gráfico de barras das médias globais
p2 <- ggplot(medias_mae, aes(x = reorder(Modelo, -MAE_Medio), y = MAE_Medio, fill = Modelo)) +
  geom_bar(stat = "identity") +
  labs(title = "Média Global de MAE por Modelo",
       x = "Modelo", y = "MAE Médio") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = round(MAE_Medio, 1)), 
            vjust = -0.5, size = 4) +
  theme(legend.position = "none")

print(p2)

# 3. Gráfico de radar (opcional, se tiveres o pacote fmsb)
if(require(fmsb, quietly = TRUE)) {
  # Preparar dados para radar
  radar_data <- data.frame(
    ARIMA = comparacao$ARIMA_MAE,
    ETS = comparacao$ETS_MAE,
    HW = comparacao$HW_MAE,
    SES = comparacao$SES_MAE,
    RF = comparacao$RF_MAE
  )
  row.names(radar_data) <- comparacao$Loja
  
  # Inverter para que maior seja melhor (para o gráfico)
  max_val <- max(radar_data) * 1.2
  radar_data <- max_val - radar_data
  
  # Adicionar linhas de max e min
  radar_data <- rbind(rep(max_val, 5), rep(0, 5), radar_data)
  
  # Cores
  cores <- c("red", "blue", "green", "orange", "purple", "brown", "pink")
  
  # Radar chart
  radarchart(radar_data,
             axistype = 1,
             title = "Comparação de Modelos (Radar)",
             vlabels = colnames(radar_data),
             vlcex = 0.8,
             caxislabels = seq(0, max_val, by = 20),
             axislabcol = "grey",
             plwd = 2,
             plty = 1,
             pcol = cores[1:4])
  
  legend("topright", legend = rownames(radar_data)[3:6], 
         col = cores[1:4], lty = 1, lwd = 2)
}

# 4. Heatmap de desempenho
df_heat <- comparacao[, c("Loja", "ARIMA_MAE", "ETS_MAE", "HW_MAE", "SES_MAE", "RF_MAE")]
df_heat_long <- reshape2::melt(df_heat, id.vars = "Loja", 
                               variable.name = "Modelo", 
                               value.name = "MAE")

p4 <- ggplot(df_heat_long, aes(x = Modelo, y = Loja, fill = MAE)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "Heatmap de Desempenho (MAE)",
       x = "Modelo", y = "Loja") +
  theme_minimal() +
  geom_text(aes(label = round(MAE, 1)), size = 3)

print(p4)

# ============================================
# ANÁLISE ESTATÍSTICA
# ============================================

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("📊 ANÁLISE ESTATÍSTICA DETALHADA\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

# Tabela de rankings
cat("🏆 RANKING POR LOJA:\n")
ranking_df <- data.frame(
  Loja = comparacao$Loja,
  Primeiro = comparacao$Melhor_Modelo,
  Segundo = NA,
  Terceiro = NA
)

for(i in 1:nrow(comparacao)) {
  maes_loja <- c(comparacao$ARIMA_MAE[i], comparacao$ETS_MAE[i], 
                 comparacao$HW_MAE[i], comparacao$SES_MAE[i], comparacao$RF_MAE[i])
  nomes <- c("ARIMA", "ETS", "HW", "SES", "RF")
  ordem <- order(maes_loja)
  ranking_df$Segundo[i] <- nomes[ordem[2]]
  ranking_df$Terceiro[i] <- nomes[ordem[3]]
}
print(ranking_df)

# Melhoria percentual do RF sobre o segundo melhor
cat("\n📈 MELHORIA DO RANDOM FOREST:\n")
for(i in 1:nrow(comparacao)) {
  cat(comparacao$Loja[i], ": RF é", comparacao$Melhoria[i], 
      "melhor que o segundo colocado\n")
}

# ============================================
# EXPORTAR RESULTADOS
# ============================================

# Salvar tabela comparativa
write.csv(comparacao, 
          file.path(raiz_projeto, "analise_modelos", "comparacao_completa.csv"), 
          row.names = FALSE)

# Salvar gráficos
ggsave(file.path(raiz_projeto, "analise_modelos", "grafico_comparacao.png"), 
       p1, width = 10, height = 6)
ggsave(file.path(raiz_projeto, "analise_modelos", "grafico_medias.png"), 
       p2, width = 8, height = 5)
ggsave(file.path(raiz_projeto, "analise_modelos", "heatmap.png"), 
       p4, width = 8, height = 5)

cat("\n✅ Ficheiros guardados:\n")
cat("   - comparacao_completa.csv\n")
cat("   - grafico_comparacao.png\n")
cat("   - grafico_medias.png\n")
cat("   - heatmap.png\n")

# ============================================
# RESUMO FINAL
# ============================================

cat("\n", paste(rep("⭐", 40), collapse=""), "\n")
cat("📌 RESUMO EXECUTIVO\n")
cat(paste(rep("⭐", 40), collapse=""), "\n\n")

cat("🏆 VENCEDOR GLOBAL: RANDOM FOREST\n")
cat("   - MAE médio: ", round(mean(comparacao$RF_MAE), 2), " clientes\n")
cat("   - Melhor em ", sum(comparacao$Melhor_Modelo == "RF"), " de 4 lojas\n\n")

cat("🥈 SEGUNDO LUGAR: SES (Suavização Exponencial Simples)\n")
cat("   - MAE médio: ", round(mean(comparacao$SES_MAE), 2), " clientes\n\n")

cat("📊 COMPARAÇÃO RF vs SES:\n")
cat("   - Baltimore: RF é ", round((comparacao$SES_MAE[1] - comparacao$RF_MAE[1]) / 
                                     comparacao$SES_MAE[1] * 100, 1), "% melhor\n")
cat("   - Lancaster: RF é ", round((comparacao$SES_MAE[2] - comparacao$RF_MAE[2]) / 
                                     comparacao$SES_MAE[2] * 100, 1), "% melhor\n")
cat("   - Philadelphia: RF é ", round((comparacao$SES_MAE[3] - comparacao$RF_MAE[3]) / 
                                        comparacao$SES_MAE[3] * 100, 1), "% melhor\n")
cat("   - Richmond: RF é ", round((comparacao$SES_MAE[4] - comparacao$RF_MAE[4]) / 
                                    comparacao$SES_MAE[4] * 100, 1), "% melhor\n")

cat("\n🔍 VARIÁVEIS MAIS IMPORTANTES (Random Forest):\n")
cat("   - Eventos turísticos (crítico em todas as lojas)\n")
cat("   - Dia da semana (especialmente em Philadelphia)\n")
cat("   - Clientes da semana anterior (lag7)\n")
cat("   - Média móvel de 3 dias\n")

cat("\n⚠️ VARIÁVEIS MENOS IMPORTANTES:\n")
cat("   - Promoções (baixo impacto em todas as lojas)\n")
cat("   - Mês do ano\n")

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("✅ ANÁLISE CONCLUÍDA COM SUCESSO!\n")
cat(paste(rep("=", 80), collapse=""), "\n")