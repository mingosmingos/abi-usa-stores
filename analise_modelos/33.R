# 36_otimizacao_O3_NSGA2_ADAPTADO.R
# O3 - Multiobjetivo com NSGA-II (adaptado ao teu eval_plan_O3.R)
# Usa eval_plan_O3 sem return_components (calcula rh e profit manualmente)

caminho_base <- "~/GitHub/abi-usa-stores"

source(file.path(caminho_base, "scripts", "utils.R"))
source(file.path(caminho_base, "scripts", "optimization.R"))
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
ALPHA <- 0.1

lower <- rep(0, 84)
upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))

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

cat("\n📊 Previsões carregadas:", nrow(forecasts), "registos\n")

# ============================================================
# FUNÇÃO PARA CALCULAR RH (MANUALMENTE)
# ============================================================

calcular_RH <- function(sol) {
  if(is.matrix(sol)) sol <- as.numeric(sol)
  dim(sol) <- c(4, 7, 3)
  rh <- 0
  for(i in 1:4) {
    for(d in 1:7) {
      rh <- rh + max(0, round(sol[i, d, 1]))  # J
      rh <- rh + max(0, round(sol[i, d, 2]))  # X
    }
  }
  return(rh)
}

# ============================================================
# FUNÇÃO PARA APLICAR REPAIR (USANDO A DO eval_plan_O3)
# ============================================================

reparar_solucao <- function(sol) {
  return(repair_solution_inplace(sol, forecasts, 21, MAX_UNIDADES))
}

# ============================================================
# FUNÇÃO PARA CALCULAR LUCRO (USANDO eval_plan_O3 COM alpha = 0)
# ============================================================
# Como eval_plan_O3 retorna -(HR - alpha * profit) para alpha=0.1,
# não conseguimos extrair apenas o lucro facilmente.
# Vamos usar eval_plan_O2 para o lucro (que já temos)

source(file.path(caminho_base, "eval_plan_O2.R"))

calcular_lucro <- function(sol) {
  sol_repaired <- reparar_solucao(sol)
  profit <- eval_plan_O2(sol_repaired, forecasts, 21, MAX_UNIDADES, verbose = FALSE)
  return(profit)
}

# ============================================================
# FUNÇÃO MULTIOBJETIVO (sem return_components)
# ============================================================

multiobjetivo <- function(sol) {
  
  if(is.matrix(sol)) sol <- as.numeric(sol)
  
  # Aplicar repair
  sol_repaired <- reparar_solucao(sol)
  
  # Calcular lucro (usando eval_plan_O2)
  profit <- calcular_lucro(sol_repaired)
  
  # Calcular RH
  rh <- calcular_RH(sol_repaired)
  
  # NSGA-II MINIMIZA ambos os objetivos
  # Objetivo 1: RH (minimizar)
  obj1 <- rh
  
  # Objetivo 2: -profit (minimizar = maximizar lucro)
  obj2 <- -profit
  
  return(c(obj1, obj2))
}

# ============================================================
# POPULAÇÃO INICIAL
# ============================================================

set.seed(123)
populacao_inicial <- matrix(NA, nrow = popSize, ncol = 84)

for(i in 1:popSize) {
  for(j in 1:28) {
    populacao_inicial[i, j] <- runif(1, 0, 8)
    populacao_inicial[i, j+28] <- runif(1, 0, 5)
    populacao_inicial[i, j+56] <- runif(1, 0, 0.15)
  }
}

# ============================================================
# EXECUTAR NSGA-II
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 OTIMIZAÇÃO O3 - NSGA-II (MULTIOBJETIVO)\n")
cat("   Adaptado ao teu eval_plan_O3.R\n")
cat("   Objetivo 1: MINIMIZAR RH\n")
cat("   Objetivo 2: MAXIMIZAR lucro\n")
cat(paste(rep("=", 70), collapse=""), "\n")

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
# RESULTADOS - FRONTEIRA DE PARETO
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 FRONTEIRA DE PARETO\n")
cat(paste(rep("=", 70), collapse=""), "\n")

pareto <- resultado$pareto.optimal
objetivos_pareto <- resultado$value[pareto, ]

if(length(pareto) > 0 && sum(pareto) > 0) {
  
  rh_pareto <- objetivos_pareto[, 1]
  lucros_pareto <- -objetivos_pareto[, 2]
  
  df_pareto <- data.frame(
    RH = round(rh_pareto, 0),
    Lucro = round(lucros_pareto, 2)
  )
  
  df_pareto <- df_pareto[order(df_pareto$RH), ]
  df_pareto <- unique(df_pareto)
  df_pareto <- df_pareto[df_pareto$Lucro > 0, ]
  
  # Calcular F = RH - 0.1 * Lucro
  df_pareto$F <- df_pareto$RH - ALPHA * df_pareto$Lucro
  
  cat("\n🎯 Soluções na fronteira de Pareto (", nrow(df_pareto), " soluções):\n")
  cat(sprintf("   %3s | %8s | %10s | %15s\n", "RH", "Lucro", "F = RH - 0.1*Lucro", "Melhor?"))
  cat("   ", paste(rep("-", 50), collapse=""), "\n")
  
  melhor_idx <- which.min(df_pareto$F)
  for(i in 1:nrow(df_pareto)) {
    estrela <- if(i == melhor_idx) "🏆 MELHOR" else ""
    cat(sprintf("   %3d | $%6.0f | %12.2f | %s\n", 
                df_pareto$RH[i], df_pareto$Lucro[i], df_pareto$F[i], estrela))
  }
  
  # ============================================================
  # SOLUÇÃO QUE MINIMIZA F
  # ============================================================
  
  melhor_rh <- df_pareto$RH[melhor_idx]
  melhor_lucro <- df_pareto$Lucro[melhor_idx]
  melhor_f <- df_pareto$F[melhor_idx]
  
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("🏆 MELHOR SOLUÇÃO SEGUNDO F = RH - 0.1 × LUCRO\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat("\n   📊 F =", round(melhor_f, 2))
  cat("\n   👥 RH =", melhor_rh, "funcionários")
  cat("\n   💰 Lucro = $", round(melhor_lucro, 2))
  
  # ============================================================
  # COMPARAÇÃO COM O2
  # ============================================================
  
  o2_lucro <- 858
  o2_rh <- 98
  o2_f <- o2_rh - ALPHA * o2_lucro
  
  cat("\n\n", paste(rep("=", 70), collapse=""), "\n")
  cat("📊 COMPARAÇÃO COM O2\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat("\n   O2: RH =", o2_rh, "| Lucro = $", o2_lucro, "| F =", round(o2_f, 2))
  cat("\n   O3: RH =", melhor_rh, "| Lucro = $", round(melhor_lucro, 2), "| F =", round(melhor_f, 2))
  
  if(melhor_f < o2_f) {
    cat("\n\n   ✅ O3 é MELHOR que O2 (F menor)")
  } else {
    cat("\n\n   ⚠️ O2 é melhor que esta solução O3")
  }
  
  # ============================================================
  # GRÁFICO
  # ============================================================
  
  p <- ggplot(df_pareto, aes(x = RH, y = Lucro)) +
    geom_point(aes(color = F), size = 3) +
    geom_line(color = "red", linetype = "dashed") +
    scale_color_gradient(low = "green", high = "red", name = "F = RH - 0.1×Lucro") +
    geom_point(data = df_pareto[melhor_idx, ], aes(x = RH, y = Lucro), 
               color = "gold", size = 5, shape = 18) +
    labs(title = "Fronteira de Pareto - O3 (Minimizar RH vs Maximizar Lucro)",
         subtitle = paste("Melhor F =", round(melhor_f, 2), "| RH =", melhor_rh, "| Lucro = $", round(melhor_lucro, 0)),
         x = "Número de Funcionários (RH)", 
         y = "Lucro ($)") +
    theme_minimal()
  
  print(p)
  
  ggsave(file.path(caminho_base, "analise_modelos", "fronteira_pareto_O3.png"), 
         p, width = 12, height = 7)
  
  # ============================================================
  # GUARDAR RESULTADOS
  # ============================================================
  
  resultados_path <- file.path(caminho_base, "analise_modelos", "resultados_O3_ADAPTADO")
  if(!dir.exists(resultados_path)) dir.create(resultados_path)
  
  saveRDS(resultado, file.path(resultados_path, "nsga2_resultado.rds"))
  saveRDS(df_pareto, file.path(resultados_path, "fronteira_pareto.rds"))
  write.csv(df_pareto, file.path(resultados_path, "fronteira_pareto.csv"), row.names = FALSE)
  
  cat("\n\n", paste(rep("=", 70), collapse=""), "\n")
  cat("✅ RESULTADOS GUARDADOS\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  cat("   Pasta:", resultados_path, "\n")
  
} else {
  cat("\n❌ Nenhuma solução de Pareto encontrada!\n")
}

cat("\n🎯 FIM DA OTIMIZAÇÃO O3\n")