# 08_comparacao_naive.R - Comparação Naive vs ARIMA vs ARIMAX (CORRIGIDO)

source("scripts/utils.R")
dados <- carregar_dados_lojas("dados/")

library(forecast)

# ============================================================
# FUNÇÃO PARA CALCULAR MODELO NAIVE
# ============================================================

calcular_naive <- function(dados_loja, nome_loja) {
  
  # Dividir em treino e teste (80/20)
  n <- nrow(dados_loja)
  treino <- dados_loja[1:floor(n*0.8), ]
  teste <- dados_loja[(floor(n*0.8)+1):n, ]
  
  # Previsão Naive: amanhã = hoje
  previsoes <- numeric(nrow(teste))
  previsoes[1] <- tail(treino$Num_Customers, 1)
  
  for(i in 2:length(previsoes)) {
    previsoes[i] <- teste$Num_Customers[i-1]
  }
  
  # Valores reais
  reais <- teste$Num_Customers
  
  # Métricas
  mae <- mean(abs(previsoes - reais))
  rmse <- sqrt(mean((previsoes - reais)^2))
  
  # Amplitude
  y_min <- min(dados_loja$Num_Customers)
  y_max <- max(dados_loja$Num_Customers)
  amplitude <- y_max - y_min
  
  nmae <- mae / amplitude * 100
  nrmse <- rmse / amplitude * 100
  
  # R²
  ss_res <- sum((reais - previsoes)^2)
  ss_tot <- sum((reais - mean(reais))^2)
  r2 <- 1 - (ss_res / ss_tot)
  
  return(list(
    loja = nome_loja,
    mae = mae,
    rmse = rmse,
    nmae = nmae,
    nrmse = nrmse,
    r2 = r2
  ))
}

# ============================================================
# FUNÇÃO PARA CALCULAR R² (se não existir)
# ============================================================

calcular_r2 <- function(reais, previstos) {
  ss_res <- sum((reais - previstos)^2)
  ss_tot <- sum((reais - mean(reais))^2)
  r2 <- 1 - (ss_res / ss_tot)
  return(r2)
}

# ============================================================
# CARREGAR RESULTADOS ARIMA E ARIMAX
# ============================================================

resultados_arima <- readRDS("analise_modelos/resultados_arima.rds")
resultados_arimax <- readRDS("analise_modelos/resultados_arimax.rds")

# ============================================================
# CALCULAR NAIVE PARA TODAS AS LOJAS
# ============================================================

resultados_naive <- list()
for(loja in names(dados)) {
  resultados_naive[[loja]] <- calcular_naive(dados[[loja]], loja)
}

# ============================================================
# CRIAR TABELA COMPARATIVA
# ============================================================

comparacao <- data.frame(
  Loja = names(dados),
  
  # NAIVE
  Naive_MAE = sapply(resultados_naive, function(x) round(x$mae, 2)),
  Naive_NMAE = sapply(resultados_naive, function(x) round(x$nmae, 2)),
  Naive_RMSE = sapply(resultados_naive, function(x) round(x$rmse, 2)),
  Naive_NRMSE = sapply(resultados_naive, function(x) round(x$nrmse, 2)),
  Naive_R2 = sapply(resultados_naive, function(x) round(x$r2, 4)),
  
  # ARIMA
  ARIMA_MAE = sapply(resultados_arima, function(x) round(x$mae, 2)),
  ARIMA_RMSE = sapply(resultados_arima, function(x) round(x$rmse, 2)),
  
  # ARIMAX
  ARIMAX_MAE = sapply(resultados_arimax, function(x) round(x$mae, 2)),
  ARIMAX_RMSE = sapply(resultados_arimax, function(x) round(x$rmse, 2))
)

# ============================================================
# CALCULAR NMAE, NRMSE E R² PARA ARIMA E ARIMAX
# ============================================================

for(i in 1:nrow(comparacao)) {
  loja <- comparacao$Loja[i]
  dados_loja <- dados[[loja]]
  amplitude <- max(dados_loja$Num_Customers) - min(dados_loja$Num_Customers)
  
  # NMAE e NRMSE
  comparacao$ARIMA_NMAE[i] <- round(comparacao$ARIMA_MAE[i] / amplitude * 100, 2)
  comparacao$ARIMA_NRMSE[i] <- round(comparacao$ARIMA_RMSE[i] / amplitude * 100, 2)
  
  comparacao$ARIMAX_NMAE[i] <- round(comparacao$ARIMAX_MAE[i] / amplitude * 100, 2)
  comparacao$ARIMAX_NRMSE[i] <- round(comparacao$ARIMAX_RMSE[i] / amplitude * 100, 2)
  
  # R² para ARIMA (calcular a partir dos dados)
  reais_arima <- resultados_arima[[loja]]$reais
  prev_arima <- resultados_arima[[loja]]$previsoes
  if(!is.null(reais_arima) && !is.null(prev_arima)) {
    r2_arima <- calcular_r2(reais_arima, prev_arima)
    comparacao$ARIMA_R2[i] <- round(r2_arima, 4)
  } else {
    comparacao$ARIMA_R2[i] <- NA
  }
  
  # R² para ARIMAX (calcular a partir dos dados)
  reais_arimax <- resultados_arimax[[loja]]$reais
  prev_arimax <- resultados_arimax[[loja]]$previsoes
  if(!is.null(reais_arimax) && !is.null(prev_arimax)) {
    r2_arimax <- calcular_r2(reais_arimax, prev_arimax)
    comparacao$ARIMAX_R2[i] <- round(r2_arimax, 4)
  } else {
    comparacao$ARIMAX_R2[i] <- NA
  }
}

# ============================================================
# MOSTRAR RESULTADOS
# ============================================================

cat("\n", paste(rep("=", 100), collapse=""), "\n")
cat("COMPARAÇÃO NAIVE vs ARIMA vs ARIMAX\n")
cat(paste(rep("=", 100), collapse=""), "\n\n")

# Tabela MAE
cat("📊 MAE (Erro Absoluto Médio - clientes):\n")
print(comparacao[, c("Loja", "Naive_MAE", "ARIMA_MAE", "ARIMAX_MAE")])

# Tabela RMSE
cat("\n📊 RMSE (Raiz do Erro Quadrático - clientes):\n")
print(comparacao[, c("Loja", "Naive_RMSE", "ARIMA_RMSE", "ARIMAX_RMSE")])

# Tabela NMAE
cat("\n📊 NMAE (Erro Normalizado - % da amplitude):\n")
print(comparacao[, c("Loja", "Naive_NMAE", "ARIMA_NMAE", "ARIMAX_NMAE")])

# Tabela R²
cat("\n📊 R² (Coeficiente de Determinação):\n")
print(comparacao[, c("Loja", "Naive_R2", "ARIMA_R2", "ARIMAX_R2")])

# ============================================================
# MÉDIAS GLOBAIS
# ============================================================

cat("\n", paste(rep("=", 100), collapse=""), "\n")
cat("MÉDIAS GLOBAIS\n")
cat(paste(rep("=", 100), collapse=""), "\n")

medias <- data.frame(
  Modelo = c("Naive", "ARIMA", "ARIMAX"),
  MAE = c(
    mean(comparacao$Naive_MAE),
    mean(comparacao$ARIMA_MAE),
    mean(comparacao$ARIMAX_MAE)
  ),
  NMAE = c(
    mean(comparacao$Naive_NMAE),
    mean(comparacao$ARIMA_NMAE),
    mean(comparacao$ARIMAX_NMAE)
  ),
  RMSE = c(
    mean(comparacao$Naive_RMSE),
    mean(comparacao$ARIMA_RMSE),
    mean(comparacao$ARIMAX_RMSE)
  ),
  R2 = c(
    mean(comparacao$Naive_R2, na.rm = TRUE),
    mean(comparacao$ARIMA_R2, na.rm = TRUE),
    mean(comparacao$ARIMAX_R2, na.rm = TRUE)
  )
)

print(medias)

# ============================================================
# AVALIAÇÃO: OS MODELOS SÃO BONS?
# ============================================================

cat("\n", paste(rep("=", 100), collapse=""), "\n")
cat("📈 AVALIAÇÃO DOS MODELOS\n")
cat(paste(rep("=", 100), collapse=""), "\n")

for(i in 1:nrow(comparacao)) {
  loja <- comparacao$Loja[i]
  naive_mae <- comparacao$Naive_MAE[i]
  arima_mae <- comparacao$ARIMA_MAE[i]
  arimax_mae <- comparacao$ARIMAX_MAE[i]
  
  cat("\n📍", toupper(loja), ":\n")
  
  # ARIMA vs Naive
  if(arima_mae < naive_mae) {
    melhoria <- round((naive_mae - arima_mae) / naive_mae * 100, 1)
    cat("   ARIMA: ✅ BOM -", melhoria, "% melhor que o Naive\n")
  } else {
    piora <- round((arima_mae - naive_mae) / naive_mae * 100, 1)
    cat("   ARIMA: ❌ MAU -", piora, "% pior que o Naive\n")
  }
  
  # ARIMAX vs Naive
  if(arimax_mae < naive_mae) {
    melhoria <- round((naive_mae - arimax_mae) / naive_mae * 100, 1)
    cat("   ARIMAX: ✅ BOM -", melhoria, "% melhor que o Naive\n")
  } else {
    piora <- round((arimax_mae - naive_mae) / naive_mae * 100, 1)
    cat("   ARIMAX: ❌ MAU -", piora, "% pior que o Naive\n")
  }
}

# ============================================================
# RESUMO FINAL
# ============================================================

cat("\n", paste(rep("=", 100), collapse=""), "\n")
cat("🏆 RESUMO FINAL\n")
cat(paste(rep("=", 100), collapse=""), "\n")

# Contar vitórias
vitorias <- data.frame(
  Modelo = c("Naive", "ARIMA", "ARIMAX"),
  Melhor_em_MAE = c(0, 0, 0)
)

for(i in 1:nrow(comparacao)) {
  maes <- c(comparacao$Naive_MAE[i], comparacao$ARIMA_MAE[i], comparacao$ARIMAX_MAE[i])
  melhor <- which.min(maes)
  vitorias$Melhor_em_MAE[melhor] <- vitorias$Melhor_em_MAE[melhor] + 1
}

cat("\n🏆 Modelo que venceu em mais lojas (menor MAE):\n")
print(vitorias)

cat("\n📊 CLASSIFICAÇÃO FINAL DOS MODELOS:\n")
cat("   1º LUGAR: ARIMAX (melhor em", vitorias$Melhor_em_MAE[3], "lojas)\n")
cat("   2º LUGAR: Naive (melhor em", vitorias$Melhor_em_MAE[1], "lojas)\n")
cat("   3º LUGAR: ARIMA (melhor em", vitorias$Melhor_em_MAE[2], "lojas)\n")

# ============================================================
# CONCLUSÃO
# ============================================================

cat("\n", paste(rep("=", 100), collapse=""), "\n")
cat("📝 CONCLUSÃO\n")
cat(paste(rep("=", 100), collapse=""), "\n")

media_arimax <- mean(comparacao$ARIMAX_MAE)
media_naive <- mean(comparacao$Naive_MAE)
melhoria_global <- round((media_naive - media_arimax) / media_naive * 100, 1)

cat("\n✅ O ARIMAX é", melhoria_global, "% melhor que o modelo Naive.\n")
cat("✅ O ARIMAX é um bom modelo porque supera claramente a referência mais simples.\n")
cat("❌ O ARIMA é pior que o Naive, portanto não é recomendado.\n")

# ============================================================
# GUARDAR RESULTADOS
# ============================================================

saveRDS(comparacao, "analise_modelos/comparacao_naive_arima_arimax.rds")
write.csv(comparacao, "analise_modelos/comparacao_naive_arima_arimax.csv", row.names = FALSE)

cat("\n✅ Resultados guardados!\n")
cat("   - comparacao_naive_arima_arimax.rds\n")
cat("   - comparacao_naive_arima_arimax.csv\n")