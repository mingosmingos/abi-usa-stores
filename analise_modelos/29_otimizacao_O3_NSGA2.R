# 29_otimizacao_O3_NSGA2.R - CORRIGIDO
# O3 - Multiobjetivo: Maximizar Lucro + Minimizar RH

caminho_base <- "~/GitHub/abi-usa-stores"

source(file.path(caminho_base, "scripts", "utils.R"))
source(file.path(caminho_base, "scripts", "optimization.R"))
source(file.path(caminho_base, "eval_plan_O2.R"))

if(!require(mco)) {
  install.packages("mco")
  library(mco)
}

library(ggplot2)

# ============================================================
# PARÂMETROS
# ============================================================

MAX_J <- 20
MAX_X <- 15
MAX_PR <- 0.30
MAX_UNIDADES <- 10000

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

cat("\n📊 Previsões para a semana 21 carregadas\n")
cat("   Total registos:", nrow(forecasts), "\n")

# ============================================================
# FUNÇÕES AUXILIARES
# ============================================================

# Calcular RH total
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

# Calcular lucro (com repair)
calcular_lucro <- function(sol) {
  if(is.matrix(sol)) sol <- as.numeric(sol)
  sol_repaired <- repair_solution_inplace(sol, forecasts, 21, MAX_UNIDADES)
  profit <- eval_plan_O2(sol_repaired, forecasts, 21, MAX_UNIDADES, verbose = FALSE)
  return(profit)
}

# ============================================================
# FUNÇÃO MULTIOBJETIVO
# ============================================================
# O NSGA-II MINIMIZA ambos os objetivos
#   Objetivo 1: -lucro (minimizar = maximizar lucro)
#   Objetivo 2: RH (minimizar)

multiobjetivo <- function(sol) {
  
  # Garantir que sol é vetor
  if(is.matrix(sol)) sol <- as.numeric(sol)
  
  # Aplicar repair
  sol_repaired <- repair_solution_inplace(sol, forecasts, 21, MAX_UNIDADES)
  
  # Objetivo 1: -lucro (minimizar)
  profit <- eval_plan_O2(sol_repaired, forecasts, 21, MAX_UNIDADES, verbose = FALSE)
  obj1 <- -profit
  
  # Objetivo 2: RH (minimizar)
  obj2 <- calcular_RH(sol_repaired)
  
  return(c(obj1, obj2))
}

# ============================================================
# GERAR POPULAÇÃO INICIAL
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
cat("   Objetivo 1: Maximizar lucro (-lucro para minimizar)\n")
cat("   Objetivo 2: Minimizar RH\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("\nParâmetros:\n")
cat("   Dimensão:", 84, "variáveis\n")
cat("   Objetivos:", 2, "\n")
cat("   População:", popSize, "\n")
cat("   Gerações:", generations, "\n")

# Executar NSGA-II (com odim)
resultado <- nsga2(
  fn = multiobjetivo,
  idim = 84,           # dimensão de entrada (número de variáveis)
  odim = 2,            # dimensão de saída (número de objetivos)
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

# Extrair fronteira de Pareto
pareto <- resultado$pareto.optimal
objetivos_pareto <- resultado$value[pareto, ]

if(length(pareto) > 0 && sum(pareto) > 0) {
  
  # Converter para valores reais (lucro = -obj1)
  lucros_pareto <- -objetivos_pareto[, 1]
  rh_pareto <- objetivos_pareto[, 2]
  
  df_pareto <- data.frame(
    Lucro = round(lucros_pareto, 2),
    RH = round(rh_pareto, 0)
  )
  
  # Remover duplicados e ordenar
  df_pareto <- unique(df_pareto)
  df_pareto <- df_pareto[order(df_pareto$Lucro, decreasing = TRUE), ]
  
  cat("\n🎯 Soluções na fronteira de Pareto (", nrow(df_pareto), " soluções):\n")
  print(df_pareto)
  
  # ============================================================
  # SOLUÇÕES DE DESTAQUE
  # ============================================================
  
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("🏆 SOLUÇÕES DE DESTAQUE\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  
  # Solução com maior lucro
  melhor_lucro <- df_pareto[which.max(df_pareto$Lucro), ]
  cat("\n💰 MAIOR LUCRO:\n")
  cat("   Lucro: $", round(melhor_lucro$Lucro, 2), "\n")
  cat("   RH:", melhor_lucro$RH, "funcionários\n")
  
  # Solução com menor RH
  menor_rh <- df_pareto[which.min(df_pareto$RH), ]
  cat("\n👥 MENOR RH:\n")
  cat("   RH:", menor_rh$RH, "funcionários\n")
  cat("   Lucro: $", round(menor_rh$Lucro, 2), "\n")
  
  # Solução de compromisso
  if(nrow(df_pareto) > 1) {
    max_lucro <- max(df_pareto$Lucro)
    min_rh <- min(df_pareto$RH)
    
    df_pareto$Lucro_norm <- df_pareto$Lucro / max_lucro
    df_pareto$RH_norm <- df_pareto$RH / min_rh
    df_pareto$Distancia <- sqrt((1 - df_pareto$Lucro_norm)^2 + (1 - df_pareto$RH_norm)^2)
    
    compromisso <- df_pareto[which.min(df_pareto$Distancia), ]
    cat("\n⚖️ SOLUÇÃO DE COMPROMISSO:\n")
    cat("   Lucro: $", round(compromisso$Lucro, 2), "\n")
    cat("   RH:", compromisso$RH, "funcionários\n")
  }
  
  # ============================================================
  # GRÁFICO
  # ============================================================
  
  p <- ggplot(df_pareto, aes(x = RH, y = Lucro)) +
    geom_point(color = "blue", size = 3) +
    geom_line(color = "red", linetype = "dashed") +
    labs(title = "Fronteira de Pareto - O3 (Lucro vs RH)",
         x = "Número de Funcionários (RH)", 
         y = "Lucro ($)") +
    theme_minimal()
  
  print(p)
  
  ggsave(file.path(caminho_base, "analise_modelos", "fronteira_pareto.png"), 
         p, width = 8, height = 6)
  
  # ============================================================
  # COMPARAÇÃO
  # ============================================================
  
  cat("\n", paste(rep("=", 70), collapse=""), "\n")
  cat("📊 COMPARAÇÃO O1 vs O2 vs O3\n")
  cat(paste(rep("=", 70), collapse=""), "\n")
  
  o1_lucro <- 22097
  o1_rh <- 700
  
  o2_lucro <- 811
  o2_rh <- 84
  
  comparacao <- data.frame(
    Objetivo = c("O1 (max lucro)", "O2 (limite 10k)", "O3 (Pareto - compromisso)"),
    Lucro = c(o1_lucro, o2_lucro, round(compromisso$Lucro, 2)),
    RH = c(o1_rh, o2_rh, compromisso$RH)
  )
  print(comparacao)
  
  # ============================================================
  # GUARDAR
  # ============================================================
  
  resultados_path <- file.path(caminho_base, "analise_modelos", "resultados_O3_NSGA2")
  if(!dir.exists(resultados_path)) dir.create(resultados_path)
  
  saveRDS(resultado, file.path(resultados_path, "nsga2_resultado.rds"))
  saveRDS(df_pareto, file.path(resultados_path, "fronteira_pareto.rds"))
  write.csv(df_pareto, file.path(resultados_path, "fronteira_pareto.csv"), row.names = FALSE)
  
  cat("\n✅ Resultados guardados em:", resultados_path, "\n")
  
} else {
  cat("\n❌ Nenhuma solução de Pareto encontrada!\n")
}