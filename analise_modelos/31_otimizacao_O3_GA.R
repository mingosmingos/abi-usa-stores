# 34_otimizacao_O3_GA_WITH_REPAIR.R
# O3 - GA com REPAIR para garantir limite de unidades

caminho_base <- "~/GitHub/abi-usa-stores"

source(file.path(caminho_base, "scripts", "utils.R"))
source(file.path(caminho_base, "scripts", "optimization.R"))
source(file.path(caminho_base, "eval_plan_O3.R"))

library(GA)

# ============================================================
# PARÂMETROS
# ============================================================

MAX_J <- 20
MAX_X <- 15
MAX_PR <- 0.30
MAX_UNIDADES <- 10000
PESO <- 0.1

lower <- rep(0, 84)
upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))

popSize <- 150
maxiter <- 150
pcrossover <- 0.8
pmutation <- 0.15
elitism <- 30
run <- 80

# ============================================================
# CARREGAR PREVISÕES
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
# FUNÇÃO PARA CALCULAR RH
# ============================================================

calcular_RH <- function(sol) {
  if(is.matrix(sol)) sol <- as.numeric(sol)
  dim(sol) <- c(4, 7, 3)
  rh <- 0
  for(i in 1:4) {
    for(d in 1:7) {
      rh <- rh + max(0, round(sol[i, d, 1]))
      rh <- rh + max(0, round(sol[i, d, 2]))
    }
  }
  return(rh)
}

# ============================================================
# FUNÇÃO DE REPARAÇÃO (garante unidades ≤ MAX_UNIDADES)
# ============================================================

reparar_solucao <- function(sol) {
  return(repair_solution_inplace(sol, forecasts, 21, MAX_UNIDADES))
}

# ============================================================
# FUNÇÃO DE FITNESS (com REPAIR obrigatório)
# ============================================================
# Objetivo: MINIMIZAR (RH - 0.1 × lucro)
# Como GA maximiza, retornamos -(RH - 0.1 × lucro)

fitness_O3 <- function(sol) {
  if(is.matrix(sol)) sol <- as.numeric(sol)
  
  # REPARAR SOLUÇÃO (garante unidades ≤ 10000)
  sol_repaired <- reparar_solucao(sol)
  
  # Calcular lucro (sem penalização de RH)
  profit <- eval_plan_O3(sol_repaired, forecasts, 21, MAX_UNIDADES, weight_hr = 0, verbose = FALSE)
  
  # Calcular RH
  rh <- calcular_RH(sol_repaired)
  
  # Objetivo: RH - 0.1 × lucro (QUEREMOS MINIMIZAR)
  objetivo <- rh - PESO * profit
  
  # GA maximiza, por isso retornamos -objetivo
  return(-objetivo)
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
# EXECUTAR GA
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 GA O3 - MINIMIZAR: RH -", PESO, "× lucro (COM REPAIR)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

ga_result <- ga(
  type = "real-valued",
  fitness = fitness_O3,
  lower = lower,
  upper = upper,
  popSize = popSize,
  maxiter = maxiter,
  pcrossover = pcrossover,
  pmutation = pmutation,
  elitism = elitism,
  run = run,
  monitor = TRUE,
  seed = 123,
  suggestions = populacao_inicial
)

# ============================================================
# RESULTADOS
# ============================================================

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 RESULTADOS GA O3 (COM REPAIR)\n")
cat(paste(rep("=", 70), collapse=""), "\n")

melhor_solucao <- ga_result@solution[1, ]
melhor_fitness <- ga_result@fitnessValue

# Aplicar repair à melhor solução
sol_final <- reparar_solucao(melhor_solucao)

# Calcular valores finais
profit_final <- eval_plan_O3(sol_final, forecasts, 21, MAX_UNIDADES, weight_hr = 0, verbose = FALSE)
rh_final <- calcular_RH(sol_final)
objetivo_final <- rh_final - PESO * profit_final

cat("\n💰 Lucro final: $", round(profit_final, 2), "\n")
cat("👥 RH final:", rh_final, "funcionários\n")
cat("📊 Objetivo (RH -", PESO, "× lucro):", round(objetivo_final, 2), "\n")

# Verificar unidades
unidades <- 0
dim(sol_final) <- c(4, 7, 3)
F_J <- c(1.00, 1.05, 1.10, 1.15)
F_X <- c(1.15, 1.20, 1.15, 1.25)

for(s in 1:4) {
  store <- lojas[s]
  for(d in 1:7) {
    J <- max(0, round(sol_final[s, d, 1]))
    X <- max(0, round(sol_final[s, d, 2]))
    PR <- min(0.30, max(0, sol_final[s, d, 3]))
    C_pred <- forecasts[forecasts$Store == store & forecasts$Day == d, "Forecast"]
    max_assisted <- 7 * X + 6 * J
    A <- min(max_assisted, C_pred)
    n_X <- min(X * 7, A)
    n_J <- A - n_X
    if(n_X > 0) {
      U_X <- round(F_X[s] * 10 / log(2 - PR))
      unidades <- unidades + n_X * U_X
    }
    if(n_J > 0) {
      U_J <- round(F_J[s] * 10 / log(2 - PR))
      unidades <- unidades + n_J * U_J
    }
  }
}

cat("📦 Unidades:", unidades, "/", MAX_UNIDADES, "\n")

if(unidades <= MAX_UNIDADES) {
  cat("✅ Restrição de", MAX_UNIDADES, "unidades RESPECTADA!\n")
} else {
  cat("❌ Restrição VIOLADA!\n")
}

# Guardar
resultados_path <- file.path(caminho_base, "analise_modelos", "resultados_O3_GA_REPAIR")
if(!dir.exists(resultados_path)) dir.create(resultados_path)

saveRDS(list(
  lucro = profit_final,
  rh = rh_final,
  objetivo = objetivo_final,
  peso = PESO,
  unidades = unidades,
  solucao = sol_final
), file.path(resultados_path, "resultado_O3_GA.rds"))

cat("\n✅ Resultados guardados em:", resultados_path, "\n")