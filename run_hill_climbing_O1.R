################################################################################
# run_hill_climbing_O1.R
# Script principal: carrega dados e executa Hill Climbing para O1
################################################################################

# Limpar ambiente
rm(list = ls())

# Carregar função auxiliar do hill climbing
source("hill.R")

# Carregar função de avaliação
source("eval_plan_O1.R")

# 1. CARREGAR PREVISÕES
cat("Carregando previsões...\n")
forecasts <- read.csv("forecasts_var_final.csv")
forecasts$Week_Start <- as.Date(forecasts$Week_Start)

# Ver semanas disponíveis
semanas_disponiveis <- unique(forecasts$Week_ID)
cat("Semanas disponíveis:", paste(semanas_disponiveis, collapse = ", "), "\n")

# 2. CONFIGURAÇÕES
week_id <- 20              # Semana a otimizar (1 a 20)
max_J <- 50                # Máximo de Juniors por dia/loja
max_X <- 30                # Máximo de Experts por dia/loja
max_iter <- 5000           # Número de iterações do Hill Climbing
report_every <- 500        # Reportar a cada N iterações
seed <- 123                # Semente para reprodutibilidade

# 3. DEFINIR LIMITES
# Ordem: [J1..J28, X1..X28, PR1..PR28]
lower <- rep(0, 84)
upper <- c(rep(max_J, 28), rep(max_X, 28), rep(0.30, 28))

# 4. FUNÇÃO DE MUDANÇA (perturbação multiplicativa)
change_fn <- function(par, lower, upper) {
  hchange(par, lower = lower, upper = upper, operator = "*", 
          dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
}

# 5. FUNÇÃO DE AVALIAÇÃO (fechamento)
eval_fn <- function(sol) {
  eval_plan_O1(sol, forecasts = forecasts, week_id = week_id, verbose = FALSE)
}

# 6. SOLUÇÃO INICIAL ALEATÓRIA
set.seed(seed)
s0 <- runif(84, min = lower, max = upper)

# 7. EXECUTAR HILL CLIMBING
cat("\n=== Executando Hill Climbing para O1 ===\n")
cat("Semana:", week_id, "\n")
cat("Iterações máximas:", max_iter, "\n")
cat("Limites: J <= ", max_J, ", X <= ", max_X, ", PR <= 0.30\n\n")

hc <- hclimbing(par = s0, fn = eval_fn, change = change_fn,
                lower = lower, upper = upper, type = "max",
                control = list(maxit = max_iter, REPORT = report_every, digits = 2))

# 8. RESULTADOS
cat("\n=== MELHOR SOLUÇÃO ENCONTRADA ===\n")
cat("Lucro total: $", round(hc$eval, 2), "\n\n")

# Extrair e mostrar o plano para cada loja
best_sol <- hc$sol
dim(best_sol) <- c(4, 7, 3)

store_names <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")

for(s in 1:4) {
  cat("\n", store_names[s], ":\n", sep="")
  cat("     Dia  |  J  |  X  |   PR  \n")
  cat("     -----|-----|-----|-------\n")
  for(d in 1:7) {
    J_val <- round(best_sol[s, d, 1])
    X_val <- round(best_sol[s, d, 2])
    PR_val <- round(best_sol[s, d, 3], 3)
    cat(sprintf("       %d   | %3d | %3d | %.3f\n", d, J_val, X_val, PR_val))
  }
}

# 9. DETALHES DO LUCRO
cat("\n=== DETALHES DO LUCRO POR LOJA/DIA ===\n")
eval_plan_O1(hc$sol, forecasts = forecasts, week_id = week_id, verbose = TRUE)

# 10. GUARDAR RESULTADOS (opcional)
saveRDS(hc, file = paste0("resultado_O1_semana_", week_id, ".rds"))
cat("\nResultado guardado em: resultado_O1_semana_", week_id, ".rds\n")

cat("\n=== FIM ===\n")