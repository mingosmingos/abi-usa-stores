# 30_otimizacao_O3_FINAL.R
# O3 - Multiobjetivo: MINIMIZAR RH (principal) e MAXIMIZAR Lucro (secundário)
# Com restrição de 10.000 unidades
# Usa eval_plan_O3.R (fora da pasta analise_modelos)

caminho_base <- "~/GitHub/abi-usa-stores"

source(file.path(caminho_base, "scripts", "utils.R"))
source(file.path(caminho_base, "scripts", "optimization.R"))

# USAR eval_plan_O3.R (FORA da pasta analise_modelos)
source(file.path(caminho_base, "eval_plan_O3.R"))

if(!require(mco)) {
  install.packages("mco")
  library(mco)
}

if(!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# ============================================================
# PARÂMETROS
# ============================================================

MAX_J <- 20
MAX_X <- 15
MAX_PR <- 0.30
MAX_UNIDADES <- 10000

# Limites do vetor
lower <- rep(0, 84)
upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))

# Parâmetros do NSGA-II
popSize <- 100
generations <- 100
crossover_prob <- 0.8
mutation_prob <- 0.2

# ============================================================
# CARREGAR PREVISÕES (semana 21)
# ============================================================

previsoes_df <- read.csv(file.path(caminho_base, "all_store_predictions.csv"), 
                         stringsAsFactors = FALSE)
previsoes_semana21 <- previsoes_df[previsoes_df$Run == 21, ]

lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")

forecasts <- data.frame()
for(store in lojas) {
  for(d in 1:7) {
    pred_value <- previsoes_semana21[previsoes_semana21$Store == store & 
                                       previsoes_semana21$Step == d, "Num_Customers"]
    if(length(pred_value) > 0) {
      forecasts <- rbind(forecasts, data.frame(
        Store = store, Week_ID = 21, Day = d, Forecast = pred_value
      ))
    }
  }
}

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 PREVISÕES PARA A SEMANA 21\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("   Total registos:", nrow(forecasts), "\n")

# ============================================================
# FUNÇÃO PARA CALCULAR RH TOTAL
# ============================================================

calcular_RH <- function(sol) {
  if(is.matrix(sol)) sol <- as.numeric(sol)
  dim(sol) <- c(4, 7, 3)
  total_rh <- 0
  for(i in 1:4) {
    for(d in 1:7) {
      total_rh <- total_rh + max(0, round(sol[i, d, 1]))
      total_rh <- total_rh + max(0, round(sol[i, d, 2]))
    }
  }
  return(total_rh)
}

# ============================================================
# FUNÇÃO PARA VERIFICAR LIMITE DE UNIDADES
# ============================================================

verificar_unidades <- function(sol, forecasts, max_units) {
  dim(sol) <- c(4, 7, 3)
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  
  total_units <- 0
  for(s in 1:4) {
    store <- lojas[s]
    for(d in 1:7) {
      J <- max(0, round(sol[s, d, 1]))
      X <- max(0, round(sol[s, d, 2]))
      PR <- min(0.30, max(0, sol[s, d, 3]))
      C_pred <- forecasts[forecasts$Store == store & forecasts$Day == d, "Forecast"]
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      n_X <- min(X * 7, A)
      n_J <- A - n_X
      if(n_X > 0) {
        U_X <- round(F_X[s] * 10 / log(2 - PR))
        total_units <- total_units + n_X * U_X
      }
      if(n_J > 0) {
        U_J <- round(F_J[s] * 10 / log(2 - PR))
        total_units <- total_units + n_J * U_J
      }
    }
  }
  return(total_units <= max_units)
}

# ============================================================
# FUNÇÃO PARA REPARAR SOLUÇÃO (garantir limite de unidades)
# ============================================================

reparar_solucao <- function(sol, forecasts, week_id, max_units) {
  return(repair_solution_inplace(sol, forecasts, week_id, max_units))
}

# ============================================================
# FUNÇÃO MULTIOBJETIVO
# ============================================================
# Objetivo PRINCIPAL: MINIMIZAR RH
# Objetivo SECUNDÁRIO: MAXIMIZAR lucro (minimizar -lucro)
# Restrição: Unidades ≤ 10.000

multiobjetivo <- function(sol) {
  
  if(is.matrix(sol)) sol <- as.numeric(sol)
  
  # Reparar solução (garantir limite de unidades)
  sol_repaired <- reparar_solucao(sol, forecasts, 21, MAX_UNIDADES)
  
  # Calcular lucro usando eval_plan_O3
  # eval_plan_O3 retorna profit - weight_hr * total_HR
  # Para obter apenas o lucro, usamos weight_hr = 0
  profit <- eval_plan_O3(sol_repaired, forecasts, 21, MAX_UNIDADES, weight_hr = 0, verbose = FALSE)
  
  # Calcular RH
  rh <- calcular_RH(sol_repaired)
  
  # Objetivo 1: RH (MINIMIZAR) - PRIORITÁRIO
  obj1 <- rh
  
  # Objetivo 2: -lucro (minimizar = maximizar lucro) - SECUNDÁRIO
  obj2 <- -profit
  
  # Penalidade para soluções que violam limite (segurança)
  if(!verificar_unidades(sol_repaired, forecasts, MAX_UNIDADES)) {
    obj1 <- obj1 + 1e9
    obj2 <- obj2 + 1e9
  }
  
  return(c(obj1, obj2))
}

# ============================================================
# GERAR POPULAÇÃO INICIAL (BAIXO RH)
# ============================================================

set.seed(123)
populacao_inicial <- matrix(NA, nrow = popSize, ncol = 84)

for(i in 1:popSize) {
  for(j in 1:28) {
    populacao_inicial[i, j] <- runif(1, 0, 8)      # J 0-8
    populacao_inicial[i, j+28] <- runif(1, 0, 5)   # X 0-5
    populacao_inicial[i, j+56] <- runif(1, 0, 0.15) # PR baixo
  }
}

# ============================================================
# EXECUTAR NSGA-II
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 OTIMIZAÇÃO O3 - NSGA-II (MULTIOBJETIVO)\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("   Objetivo PRINCIPAL: MINIMIZAR RH\n")
cat("   Objetivo SECUNDÁRIO: MAXIMIZAR lucro\n")
cat("   Restrição: Unidades ≤ 10.000\n")
cat("\nParâmetros:\n")
cat("   Dimensão:", 84, "variáveis\n")
cat("   Objetivos:", 2, "\n")
cat("   População:", popSize, "\n")
cat("   Gerações:", generations, "\n")

resultado <- nsga2(
  fn = multiobjetivo,
  idim = 84,
  odim = 2,
  lower.bounds = lower,
  upper.bounds = upper,
  popsize = popSize,
  generations = generations,
  cprob = crossover_prob,
  mprob = mutation_prob,
  vectorized = FALSE
)

# ============================================================
# PROCESSAR RESULTADOS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 FRONTEIRA DE PARETO (RH vs Lucro)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

pareto <- resultado$pareto.optimal
objetivos_pareto <- resultado$value[pareto, ]

if(length(pareto) > 0 && sum(pareto) > 0) {
  
  # Converter para valores reais
  # obj1 = RH, obj2 = -lucro
  rh_pareto <- objetivos_pareto[, 1]
  lucros_pareto <- -objetivos_pareto[, 2]
  
  df_pareto <- data.frame(
    RH = round(rh_pareto, 0),
    Lucro = round(lucros_pareto, 2)
  )
  
  # Ordenar por RH (crescente)
  df_pareto <- df_pareto[order(df_pareto$RH), ]
  
  # Remover duplicados
  df_pareto <- unique(df_pareto)
  
  cat("\n🎯 Soluções na fronteira de Pareto (", nrow(df_pareto), " soluções):\n")
  for(i in 1:nrow(df_pareto)) {
    cat(sprintf("   RH = %2d → Lucro = $%6.0f\n", df_pareto$RH[i], df_pareto$Lucro[i]))
  }
  
  # ============================================================
  # SOLUÇÕES DE DESTAQUE
  # ============================================================
  
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("🏆 ANÁLISE DE COMPROMISSO\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  
  # 1. Menor RH possível
  menor_rh <- df_pareto[which.min(df_pareto$RH), ]
  cat("\n👥 MENOR RH POSSÍVEL:\n")
  cat("   RH =", menor_rh$RH, "funcionários\n")
  cat("   Lucro = $", round(menor_rh$Lucro, 2), "\n")
  
  # 2. Maior lucro possível
  maior_lucro <- df_pareto[which.max(df_pareto$Lucro), ]
  cat("\n💰 MAIOR LUCRO POSSÍVEL:\n")
  cat("   Lucro = $", round(maior_lucro$Lucro, 2), "\n")
  cat("   RH =", maior_lucro$RH, "funcionários\n")
  
  # 3. Solução com RH = 84 (igual ao O2)
  solucao_RH84 <- df_pareto[df_pareto$RH == 84, ]
  if(nrow(solucao_RH84) > 0) {
    cat("\n⚖️ SOLUÇÃO COM RH = 84 (comparação com O2):\n")
    cat("   Lucro = $", round(solucao_RH84$Lucro[1], 2), "\n")
  } else {
    # Solução mais próxima de RH=84
    idx_prox <- which.min(abs(df_pareto$RH - 84))
    cat("\n⚖️ SOLUÇÃO MAIS PRÓXIMA DE RH = 84:\n")
    cat("   RH =", df_pareto$RH[idx_prox], "funcionários\n")
    cat("   Lucro = $", round(df_pareto$Lucro[idx_prox], 2), "\n")
  }
  
  # 4. Comparação com O2
  o2_lucro <- 811
  o2_rh <- 84
  
  # Melhor solução com RH <= o2_rh
  solucoes_melhor_RH <- df_pareto[df_pareto$RH <= o2_rh, ]
  if(nrow(solucoes_melhor_RH) > 0) {
    melhor_RH <- solucoes_melhor_RH[which.min(solucoes_melhor_RH$RH), ]
    cat("\n📊 COMPARAÇÃO COM O2 (limite 10k unidades):\n")
    cat("   O2: RH =", o2_rh, "→ Lucro = $", o2_lucro, "\n")
    cat("   O3: RH =", melhor_RH$RH, "→ Lucro = $", round(melhor_RH$Lucro, 2), "\n")
    if(melhor_RH$RH < o2_rh) {
      cat("   ✅ O3 consegue REDUZIR RH para", melhor_RH$RH, "(menos", o2_rh - melhor_RH$RH, "funcionários)\n")
    }
  }
  
  # ============================================================
  # GRÁFICO (eixos normais, sem inversão)
  # ============================================================
  
  p <- ggplot(df_pareto, aes(x = RH, y = Lucro)) +
    geom_point(color = "blue", size = 3) +
    geom_line(color = "red", linetype = "dashed") +
    labs(title = "Fronteira de Pareto - O3 (Minimizar RH vs Maximizar Lucro)",
         subtitle = paste("Limite de 10.000 unidades |", nrow(df_pareto), "soluções não-dominadas"),
         x = "Número de Funcionários (RH)", 
         y = "Lucro ($)") +
    theme_minimal() +
    geom_text(aes(label = paste0(round(Lucro, 0))), 
              hjust = -0.2, vjust = -0.5, size = 3)
  
  print(p)
  
  ggsave(file.path(caminho_base, "analise_modelos", "fronteira_pareto_O3.png"), 
         p, width = 10, height = 6)
  
  # ============================================================
  # COMPARAÇÃO FINAL O1 vs O2 vs O3
  # ============================================================
  
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("📊 COMPARAÇÃO FINAL O1 vs O2 vs O3\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  
  o1_lucro <- 22097
  o1_rh <- 700
  
  comparacao <- data.frame(
    Objetivo = c("O1 (max lucro)", "O2 (limite 10k)", 
                 paste0("O3 (menor RH = ", menor_rh$RH, ")"),
                 paste0("O3 (maior lucro = ", round(maior_lucro$Lucro, 0), ")")),
    Lucro = c(o1_lucro, o2_lucro, round(menor_rh$Lucro, 2), round(maior_lucro$Lucro, 2)),
    RH = c(o1_rh, o2_rh, menor_rh$RH, maior_lucro$RH)
  )
  print(comparacao)
  
  # ============================================================
  # GUARDAR RESULTADOS
  # ============================================================
  
  resultados_path <- file.path(caminho_base, "analise_modelos", "resultados_O3_FINAL")
  if(!dir.exists(resultados_path)) dir.create(resultados_path)
  
  saveRDS(resultado, file.path(resultados_path, "nsga2_resultado.rds"))
  saveRDS(df_pareto, file.path(resultados_path, "fronteira_pareto.rds"))
  write.csv(df_pareto, file.path(resultados_path, "fronteira_pareto.csv"), row.names = FALSE)
  
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("✅ RESULTADOS GUARDADOS\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat("   Pasta:", resultados_path, "\n")
  cat("   - nsga2_resultado.rds\n")
  cat("   - fronteira_pareto.rds\n")
  cat("   - fronteira_pareto.csv\n")
  cat("   - fronteira_pareto_O3.png\n")
  
} else {
  cat("\n❌ Nenhuma solução de Pareto encontrada!\n")
}

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 FIM DA OTIMIZAÇÃO O3\n")
cat(paste(rep("=", 70), collapse=""), "\n")