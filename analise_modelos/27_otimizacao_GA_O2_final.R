# 27_otimizacao_GA_O2_final.R - VERSÃO QUE FUNCIONOU (com repair correto)

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

# Carregar previsões
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

# Função de fitness com repair
fitness_repair <- function(sol) {
  if(is.matrix(sol)) sol <- as.numeric(sol)
  
  # Aplicar repair (modifica a solução)
  sol_repaired <- repair_solution_inplace(sol, forecasts, 21, MAX_UNIDADES)
  
  # Atualizar globalmente para o GA ver a solução reparada
  assign(".LastRepairedSolution", sol_repaired, envir = .GlobalEnv)
  
  # Calcular lucro
  profit <- eval_plan_O2(sol_repaired, forecasts, 21, MAX_UNIDADES, verbose = FALSE)
  
  return(profit)  # Maximiza
}

# População inicial (100% baixo RH para garantir)
set.seed(123)
populacao_inicial <- matrix(NA, nrow = popSize, ncol = 84)
for(i in 1:popSize) {
  for(j in 1:28) {
    populacao_inicial[i, j] <- runif(1, 0, 5)      # J ≤ 5
    populacao_inicial[i, j+28] <- runif(1, 0, 3)   # X ≤ 3
    populacao_inicial[i, j+56] <- runif(1, 0, 0.15) # PR baixo
  }
}

cat("\n", paste(rep("=", 70), collapse=""), "\n")
cat("🎯 GA O2 - Semana 21\n")
cat(paste(rep("=", 70), collapse=""), "\n")

ga_result <- ga(
  type = "real-valued",
  fitness = fitness_repair,
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

# Melhor solução (usar a reparada se existir)
if(exists(".LastRepairedSolution")) {
  melhor_solucao <- .LastRepairedSolution
} else {
  melhor_solucao <- ga_result@solution[1, ]
}

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
cat("📊 RESULTADO\n")
cat(paste(rep("=", 70), collapse=""), "\n")
cat("\n💰 Lucro: $", round(melhor_lucro, 2), "\n")
cat("📦 Unidades:", unidades, "/", MAX_UNIDADES, "\n")

if(unidades <= MAX_UNIDADES) {
  cat("✅ Restrição RESPECTADA!\n")
} else {
  cat("❌ Restrição VIOLADA!\n")
}