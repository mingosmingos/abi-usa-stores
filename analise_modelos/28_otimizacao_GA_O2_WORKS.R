# 28_otimizacao_GA_O2_WORKS.R
# VERSÃO QUE FUNCIONA (com maximização, mas apresentamos -fitness para o stor)

caminho_base <- "~/GitHub/abi-usa-stores"

source(file.path(caminho_base, "scripts", "utils.R"))
source(file.path(caminho_base, "scripts", "optimization.R"))
source(file.path(caminho_base, "eval_plan_O2.R"))

library(GA)

MAX_J <- 20
MAX_X <- 15
MAX_PR <- 0.30
MAX_UNIDADES <- 10000

lower <- rep(0, 84)
upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))

popSize <- 150
maxiter <- 150
pcrossover <- 0.8
pmutation <- 0.15
elitism <- 30
run <- 80

# Carregar previsões semana 21
previsoes_df <- read.csv(file.path(caminho_base, "all_store_predictions.csv"), 
                         stringsAsFactors = FALSE)
previsoes_semana21 <- previsoes_df[previsoes_df$Run == 21, ]

lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")

# Preparar forecasts
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

# Função que REPARA e RETORNA LUCRO (positivo)
fitness_original <- function(sol) {
  if(is.matrix(sol)) sol <- as.numeric(sol)
  sol_repaired <- repair_solution_inplace(sol, forecasts, 21, MAX_UNIDADES)
  profit <- eval_plan_O2(sol_repaired, forecasts, 21, MAX_UNIDADES, verbose = FALSE)
  return(profit)  # GA maximiza por padrão
}

# População inicial (70% baixo RH)
set.seed(123)
populacao_inicial <- matrix(NA, nrow = popSize, ncol = 84)
for(i in 1:popSize) {
  if(i <= 0.7 * popSize) {
    for(j in 1:28) {
      populacao_inicial[i, j] <- runif(1, 0, 6)
      populacao_inicial[i, j+28] <- runif(1, 0, 3)
      populacao_inicial[i, j+56] <- runif(1, 0, 0.15)
    }
  } else {
    for(j in 1:28) {
      populacao_inicial[i, j] <- runif(1, 0, MAX_J)
      populacao_inicial[i, j+28] <- runif(1, 0, MAX_X)
      populacao_inicial[i, j+56] <- runif(1, 0, MAX_PR)
    }
  }
}

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 GA O2 - Semana 21 (MAXIMIZAÇÃO)\n")
cat("   Resultado final será convertido para minimização no output\n")
cat(paste(rep("=", 70), collapse=""), "\n")

ga_result <- ga(
  type = "real-valued",
  fitness = fitness_original,
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

melhor_solucao <- ga_result@solution[1, ]
melhor_lucro <- ga_result@fitnessValue

# Calcular unidades
calcular_unidades <- function(sol) {
  dim(sol) <- c(4, 7, 3)
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
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
  return(total_units)
}

unidades <- calcular_unidades(melhor_solucao)

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("📊 RESULTADO FINAL\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("\n💰 Lucro: $", round(melhor_lucro, 2), "\n")
cat("📦 Unidades:", unidades, "/", MAX_UNIDADES, "\n")
cat("📉 Para minimização (convenção do stor):", round(-melhor_lucro, 2), "\n")

if(unidades <= MAX_UNIDADES) {
  cat("\n✅ Restrição de", MAX_UNIDADES, "unidades RESPECTADA!\n")
} else {
  cat("\n❌ Restrição VIOLADA!\n")
}

# Guardar
resultados_path <- file.path(caminho_base, "analise_modelos", "resultados_GA_O2_final")
if(!dir.exists(resultados_path)) dir.create(resultados_path)
saveRDS(list(lucro = melhor_lucro, unidades = unidades, solucao = melhor_solucao), 
        file.path(resultados_path, "resultado_semana21.rds"))

cat("\n✅ Guardado em:", resultados_path, "\n")