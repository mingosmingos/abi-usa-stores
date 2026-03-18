# ==================================================
# optimization.R - Otimização de recursos humanos e promoções
# ==================================================

library(GA)
library(tidyverse)

source("~/R Project/scripts/utils.R")

#' Calcula clientes assistidos (Slide 12)
#' @param J Número de funcionários Junior
#' @param X Número de funcionários Expert
#' @param C_previsto Número previsto de clientes
#' @param loja Nome da loja
#' @return Número de clientes que serão assistidos
calcular_clientes_assistidos <- function(J, X, C_previsto, loja) {
  # Cada X pode atender 7 clientes, cada J pode atender 6
  capacidade_X <- 7 * X
  capacidade_J <- 6 * J
  
  if(capacidade_X >= C_previsto) {
    # X conseguem atender todos
    return(C_previsto)
  } else {
    # X atendem todos os que conseguem, J atendem o resto
    clientes_restantes <- C_previsto - capacidade_X
    return(capacidade_X + min(capacidade_J, clientes_restantes))
  }
}

#' Calcula unidades vendidas por cliente (Slide 15)
#' @param PR Percentagem de promoção (0-0.3)
#' @param F Fator de ajuda do funcionário
#' @param loja_params Parâmetros da loja
#' @return Número de unidades vendidas por cliente
calcular_unidades <- function(PR, F, loja_params) {
  PR <- min(max(PR, 0), 0.3)  # Garantir limites
  unidades <- round(F * 10 / log(2 - PR))
  return(max(unidades, 0))
}

#' Calcula lucro por cliente (Slide 15)
#' @param unidades Número de unidades vendidas
#' @param PR Percentagem de promoção
#' @return Lucro por cliente
calcular_lucro_cliente <- function(unidades, PR) {
  omega <- 1  # Valor padrão (não especificado)
  lucro <- round(unidades * omega * (1 - PR) * 1.07)
  return(lucro)
}

#' Calcula custo de funcionários para um dia
#' @param J Número de Junior
#' @param X Número de Expert
#' @param data Data
#' @return Custo total dos funcionários
calcular_custo_funcionarios <- function(J, X, data) {
  weekend <- is_weekend(data)
  
  if(weekend) {
    custo_total <- J * custos$junior$weekend + X * custos$expert$weekend
  } else {
    custo_total <- J * custos$junior$weekday + X * custos$expert$weekday
  }
  
  return(custo_total)
}

#' Calcula lucro diário de uma loja
#' @param J Número de Junior
#' @param X Número de Expert
#' @param PR Percentagem de promoção
#' @param C_previsto Clientes previstos
#' @param data Data
#' @param loja Nome da loja
#' @return Lucro do dia
calcular_lucro_diario <- function(J, X, PR, C_previsto, data, loja) {
  loja_params <- parametros_lojas[[loja]]
  
  # Calcular clientes assistidos
  A <- calcular_clientes_assistidos(J, X, C_previsto, loja)
  
  # Distribuir clientes entre X e J
  clientes_X <- min(7 * X, A)
  clientes_J <- max(0, A - clientes_X)
  
  # Calcular lucro
  lucro_total <- 0
  
  if(clientes_X > 0) {
    unidades_X <- calcular_unidades(PR, loja_params$Fx, loja_params)
    lucro_cliente_X <- calcular_lucro_cliente(unidades_X, PR)
    lucro_total <- lucro_total + (lucro_cliente_X * clientes_X)
  }
  
  if(clientes_J > 0) {
    unidades_J <- calcular_unidades(PR, loja_params$Fj, loja_params)
    lucro_cliente_J <- calcular_lucro_cliente(unidades_J, PR)
    lucro_total <- lucro_total + (lucro_cliente_J * clientes_J)
  }
  
  # Subtrair custos
  lucro_total <- lucro_total - calcular_custo_funcionarios(J, X, data)
  
  return(lucro_total)
}

#' Calcula lucro semanal de uma loja
#' @param plano Lista com J, X, PR para 7 dias
#' @param C_previstos Vetor com previsões para 7 dias
#' @param loja Nome da loja
#' @return Lucro semanal
calcular_lucro_semanal <- function(plano, C_previstos, loja) {
  datas <- seq.Date(Sys.Date(), by = "day", length.out = 7)
  lucros_diarios <- numeric(7)
  unidades_totais <- 0
  
  for(dia in 1:7) {
    lucros_diarios[dia] <- calcular_lucro_diario(
      J = plano$J[dia],
      X = plano$X[dia],
      PR = plano$PR[dia],
      C_previsto = C_previstos[dia],
      data = datas[dia],
      loja = loja
    )
    
    # Calcular unidades vendidas (para O2)
    A <- calcular_clientes_assistidos(plano$J[dia], plano$X[dia], 
                                      C_previstos[dia], loja)
    loja_params <- parametros_lojas[[loja]]
    
    clientes_X <- min(7 * plano$X[dia], A)
    clientes_J <- max(0, A - clientes_X)
    
    if(clientes_X > 0) {
      unidades_X <- calcular_unidades(plano$PR[dia], loja_params$Fx, loja_params)
      unidades_totais <- unidades_totais + (unidades_X * clientes_X)
    }
    
    if(clientes_J > 0) {
      unidades_J <- calcular_unidades(plano$PR[dia], loja_params$Fj, loja_params)
      unidades_totais <- unidades_totais + (unidades_J * clientes_J)
    }
  }
  
  lucro_semanal <- sum(lucros_diarios) - parametros_lojas[[loja]]$Ws
  
  return(list(
    lucro = lucro_semanal,
    unidades = unidades_totais,
    lucros_diarios = lucros_diarios
  ))
}

#' Função de fitness para O1 (maximizar lucro)
fitness_o1 <- function(x, C_previstos, loja) {
  # x[1:7] = J, x[8:14] = X, x[15:21] = PR (0-30)
  
  J <- round(pmax(x[1:7], 0))
  X <- round(pmax(x[8:14], 0))
  PR <- pmin(pmax(x[15:21] / 100, 0), 0.3)
  
  # Limitar máximo de funcionários (realista)
  J <- pmin(J, 50)
  X <- pmin(X, 30)
  
  plano <- list(J = J, X = X, PR = PR)
  
  resultado <- calcular_lucro_semanal(plano, C_previstos, loja)
  
  # Penalizar lucro negativo
  if(resultado$lucro < 0) {
    return(resultado$lucro * 10)  # Penalidade pesada
  }
  
  return(resultado$lucro)
}

#' Função de fitness para O2 (limite 10000 unidades)
fitness_o2 <- function(x, C_previstos_list, lojas) {
  # x combinado para todas as lojas
  n_lojas <- length(lojas)
  lucro_total <- 0
  unidades_total <- 0
  
  for(i in 1:n_lojas) {
    loja <- lojas[i]
    idx <- (i-1)*21 + 1:21
    
    J <- round(pmax(x[idx[1:7]], 0))
    X <- round(pmax(x[idx[8:14]], 0))
    PR <- pmin(pmax(x[idx[15:21]] / 100, 0), 0.3)
    
    J <- pmin(J, 50)
    X <- pmin(X, 30)
    
    plano <- list(J = J, X = X, PR = PR)
    
    resultado <- calcular_lucro_semanal(plano, C_previstos_list[[loja]], loja)
    
    lucro_total <- lucro_total + resultado$lucro
    unidades_total <- unidades_total + resultado$unidades
  }
  
  # Penalidade se ultrapassar limite
  if(unidades_total > 10000) {
    lucro_total <- lucro_total - 100000 * (unidades_total - 10000)
  }
  
  return(lucro_total)
}

#' Função de fitness para O3 (O2 + minimizar RH)
fitness_o3 <- function(x, C_previstos_list, lojas) {
  n_lojas <- length(lojas)
  lucro_total <- 0
  unidades_total <- 0
  total_hr <- 0
  
  for(i in 1:n_lojas) {
    loja <- lojas[i]
    idx <- (i-1)*21 + 1:21
    
    J <- round(pmax(x[idx[1:7]], 0))
    X <- round(pmax(x[idx[8:14]], 0))
    PR <- pmin(pmax(x[idx[15:21]] / 100, 0), 0.3)
    
    J <- pmin(J, 50)
    X <- pmin(X, 30)
    
    total_hr <- total_hr + sum(J) + sum(X)
    
    plano <- list(J = J, X = X, PR = PR)
    
    resultado <- calcular_lucro_semanal(plano, C_previstos_list[[loja]], loja)
    
    lucro_total <- lucro_total + resultado$lucro
    unidades_total <- unidades_total + resultado$unidades
  }
  
  # Penalidades
  if(unidades_total > 10000) {
    lucro_total <- lucro_total - 100000 * (unidades_total - 10000)
  }
  
  # Bónus por poucos funcionários (para O3)
  lucro_total <- lucro_total - total_hr * 100  # Penalizar muitos funcionários
  
  return(lucro_total)
}

#' Otimizar recursos com Algoritmo Genético
#' @param C_previstos_list Lista com previsões para cada loja
#' @param objetivo "O1", "O2" ou "O3"
#' @return Planos otimizados
otimizar_recursos <- function(C_previstos_list, objetivo = "O1") {
  
  lojas <- names(C_previstos_list)
  
  if(objetivo == "O1") {
    # Otimizar cada loja independentemente
    planos <- list()
    
    for(loja in lojas) {
      cat("Otimizando", loja, "para", objetivo, "\n")
      
      # Limites: J (0-30), X (0-20), PR (0-30)
      lower <- c(rep(0, 14), rep(0, 7))
      upper <- c(rep(30, 7), rep(20, 7), rep(30, 7))
      
      # Executar GA
      resultado <- ga(
        type = "real-valued",
        fitness = function(x) fitness_o1(x, C_previstos_list[[loja]], loja),
        lower = lower,
        upper = upper,
        popSize = 100,
        maxiter = 100,
        run = 50,
        monitor = FALSE,
        parallel = FALSE
      )
      
      # Decodificar melhor solução
      melhor <- resultado@solution[1,]
      
      planos[[loja]] <- list(
        J = round(pmin(pmax(melhor[1:7], 0), 30)),
        X = round(pmin(pmax(melhor[8:14], 0), 20)),
        PR = pmin(pmax(melhor[15:21] / 100, 0), 0.3),
        fitness = resultado@fitnessValue
      )
    }
    
    return(planos)
    
  } else {
    # Otimização conjunta para O2 ou O3
    n_lojas <- length(lojas)
    
    lower <- rep(c(rep(0, 14), rep(0, 7)), n_lojas)
    upper <- rep(c(rep(30, 7), rep(20, 7), rep(30, 7)), n_lojas)
    
    # Escolher função fitness
    fitness_func <- switch(objetivo,
                           "O2" = fitness_o2,
                           "O3" = fitness_o3)
    
    resultado <- ga(
      type = "real-valued",
      fitness = function(x) fitness_func(x, C_previstos_list, lojas),
      lower = lower,
      upper = upper,
      popSize = 200,
      maxiter = 200,
      run = 100,
      monitor = FALSE,
      parallel = FALSE
    )
    
    # Decodificar soluções
    melhor <- resultado@solution[1,]
    planos <- list()
    
    for(i in 1:n_lojas) {
      loja <- lojas[i]
      idx <- (i-1)*21 + 1:21
      
      planos[[loja]] <- list(
        J = round(pmin(pmax(melhor[idx[1:7]], 0), 30)),
        X = round(pmin(pmax(melhor[idx[8:14]], 0), 20)),
        PR = pmin(pmax(melhor[idx[15:21]] / 100, 0), 0.3)
      )
    }
    
    planos$fitness_total <- resultado@fitnessValue
    return(planos)
  }
}

#' Formatar plano para output
#' @param plano Lista com o plano
#' @param loja Nome da loja
#' @return Dataframe formatado
formatar_plano <- function(plano, loja) {
  dias_semana <- c("Segunda", "Terça", "Quarta", "Quinta", 
                   "Sexta", "Sábado", "Domingo")
  
  df <- data.frame(
    Dia = dias_semana,
    Funcionarios_J = plano$J,
    Funcionarios_X = plano$X,
    Total_Func = plano$J + plano$X,
    Promocao = paste0(round(plano$PR * 100, 1), "%")
  )
  
  return(df)
}

