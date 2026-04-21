# 13_comparar_O1_O2_O3.R - Comparar resultados dos 3 objetivos

source("scripts/utils.R")
dados <- carregar_dados_lojas("dados/")

# Carregar resultados
resultados_O1 <- readRDS("analise_modelos/resultados_otimizacao_O1.rds")  # do ficheiro 09
resultados_O2 <- readRDS("analise_modelos/resultados_otimizacao_O2.rds")
resultados_O3 <- readRDS("analise_modelos/resultados_otimizacao_O3.rds")

library(ggplot2)

# ============================================================
# CRIAR TABELA COMPARATIVA
# ============================================================

comparacao <- data.frame()

for(loja in names(dados)) {
  
  # O1
  comparacao <- rbind(comparacao, data.frame(
    Loja = toupper(loja),
    Objetivo = "O1",
    Lucro = resultados_O1[[loja]]$lucro_semanal,
    Unidades = resultados_O1[[loja]]$unidades_totais,
    RH = sum(resultados_O1[[loja]]$J) + sum(resultados_O1[[loja]]$X)
  ))
  
  # O2
  comparacao <- rbind(comparacao, data.frame(
    Loja = toupper(loja),
    Objetivo = "O2",
    Lucro = resultados_O2[[loja]]$lucro_semanal,
    Unidades = resultados_O2[[loja]]$unidades_totais,
    RH = sum(resultados_O2[[loja]]$J) + sum(resultados_O2[[loja]]$X)
  ))
  
  # O3
  comparacao <- rbind(comparacao, data.frame(
    Loja = toupper(loja),
    Objetivo = "O3",
    Lucro = resultados_O3[[loja]]$lucro_semanal,
    Unidades = resultados_O3[[loja]]$unidades_totais,
    RH = resultados_O3[[loja]]$total_RH
  ))
}

# ============================================================
# MOSTRAR TABELA
# ============================================================

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("COMPARAÇÃO O1 vs O2 vs O3\n")
cat(paste(rep("=", 80), collapse=""), "\n\n")

print(comparacao)

# ============================================================
# GRÁFICO 1: LUCRO
# ============================================================

p1 <- ggplot(comparacao, aes(x = Loja, y = Lucro, fill = Objetivo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparação de Lucro por Objetivo",
       x = "Loja", y = "Lucro Semanal ($)") +
  theme_minimal() +
  scale_fill_manual(values = c("O1" = "blue", "O2" = "green", "O3" = "orange")) +
  geom_text(aes(label = round(Lucro, 0)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)

print(p1)

# ============================================================
# GRÁFICO 2: UNIDADES
# ============================================================

p2 <- ggplot(comparacao, aes(x = Loja, y = Unidades, fill = Objetivo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 10000, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 0.5, y = 10500, label = "Limite O2/O3 (10,000)", color = "red", size = 3) +
  labs(title = "Unidades Vendidas por Objetivo",
       x = "Loja", y = "Unidades Totais") +
  theme_minimal() +
  scale_fill_manual(values = c("O1" = "blue", "O2" = "green", "O3" = "orange")) +
  geom_text(aes(label = format(Unidades, big.mark = ",")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)

print(p2)

# ============================================================
# GRÁFICO 3: RH
# ============================================================

p3 <- ggplot(comparacao, aes(x = Loja, y = RH, fill = Objetivo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Funcionários por Objetivo",
       x = "Loja", y = "Número de Funcionários (J+X)") +
  theme_minimal() +
  scale_fill_manual(values = c("O1" = "blue", "O2" = "green", "O3" = "orange")) +
  geom_text(aes(label = RH), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3)

print(p3)

# ============================================================
# GUARDAR
# ============================================================

ggsave("analise_modelos/comparacao_O1_O2_O3_lucro.png", p1, width = 10, height = 6)
ggsave("analise_modelos/comparacao_O1_O2_O3_unidades.png", p2, width = 10, height = 6)
ggsave("analise_modelos/comparacao_O1_O2_O3_rh.png", p3, width = 10, height = 6)
saveRDS(comparacao, "analise_modelos/comparacao_O1_O2_O3.rds")

cat("\n✅ Gráficos guardados:\n")
cat("   - comparacao_O1_O2_O3_lucro.png\n")
cat("   - comparacao_O1_O2_O3_unidades.png\n")
cat("   - comparacao_O1_O2_O3_rh.png\n")