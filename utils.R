# ==================================================
# utils.R - Funções utilitárias para o projeto
# ==================================================

library(tidyverse)
library(lubridate)

# Parâmetros das lojas (Slide 13)
parametros_lojas <- list(
  baltimore = list(
    nome = "Baltimore",
    estado = "MD",
    Fj = 1.00,
    Fx = 1.15,
    Ws = 700
  ),
  lancaster = list(
    nome = "Lancaster",
    estado = "PA",
    Fj = 1.05,
    Fx = 1.20,
    Ws = 730
  ),
  philadelphia = list(
    nome = "Philadelphia",
    estado = "PA",
    Fj = 1.10,
    Fx = 1.15,
    Ws = 760
  ),
  richmond = list(
    nome = "Richmond",
    estado = "VA",
    Fj = 1.15,
    Fx = 1.25,
    Ws = 800
  )
)

# Custos diários por tipo de funcionário (Slide 12)
custos <- list(
  junior = list(
    weekday = 60,
    weekend = 70
  ),
  expert = list(
    weekday = 80,
    weekend = 95
  )
)

#' Verifica se uma data é fim de semana
#' @param data Data a verificar
#' @return TRUE se for sábado ou domingo
is_weekend <- function(data) {
  wday <- wday(data, week_start = 1)  # 1 = segunda, 7 = domingo
  return(wday >= 6)  # 6 = sábado, 7 = domingo
}

#' Carrega dados de todas as lojas
#' @param pasta_dados Caminho para a pasta com os CSVs
#' @return Lista com dataframes de cada loja
carregar_dados_lojas <- function(pasta_dados = "../dados/") {
  lojas <- c("baltimore", "lancaster", "philadelphia", "richmond")
  dados <- list()
  
  for (loja in lojas) {
    arquivo <- file.path(pasta_dados, paste0(loja, ".csv"))
    if(file.exists(arquivo)) {
      df <- read.csv(arquivo, header = TRUE, stringsAsFactors = FALSE)
      df$Date <- as.Date(df$Date)
      df$TouristEvent <- as.factor(df$TouristEvent)
      df$Store <- loja
      
      # Adicionar colunas úteis
      df <- df %>%
        mutate(
          DiaSemana = wday(Date, week_start = 1, label = TRUE),
          FimSemana = is_weekend(Date),
          TouristEvent_bin = ifelse(TouristEvent == "Yes", 1, 0),
          Ano = year(Date),
          Mes = month(Date),
          Dia = day(Date)
        )
      
      dados[[loja]] <- df
      cat("Carregado:", loja, "-", nrow(df), "linhas\n")
    } else {
      warning("Ficheiro não encontrado: ", arquivo)
    }
  }
  
  return(dados)
}

#' Formata valores monetários
#' @param x Valor numérico
#' @return String formatada
format_money <- function(x) {
  paste0("$", format(round(x, 2), big.mark = ","))
}

#' Formata percentagens
#' @param x Valor numérico (0-1)
#' @return String formatada
format_percent <- function(x) {
  paste0(round(x * 100, 1), "%")
}

#' Cria um resumo estatístico dos dados
#' @param dados Lista de dataframes das lojas
#' @return Dataframe com resumo
resumo_estatistico <- function(dados) {
  lojas <- names(dados)
  resumo_df <- data.frame()  # Alterado de 'resumo' para 'resumo_df'
  
  for(loja in lojas) {
    df <- dados[[loja]]
    resumo_loja <- data.frame(
      Loja = loja,
      Estado = parametros_lojas[[loja]]$estado,
      Periodo = paste(min(df$Date), "a", max(df$Date)),
      Total_Dias = nrow(df),
      Media_Clientes = round(mean(df$Num_Customers), 2),
      Media_Func = round(mean(df$Num_Employees), 2),
      Media_Sales = round(mean(df$Sales), 2),
      Media_Promocao = round(mean(df$Pct_On_Sale, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )
    resumo_df <- rbind(resumo_df, resumo_loja)  # Alterado de 'resumo' para 'resumo_df'
  }
  
  return(resumo_df)  # Alterado de 'resumo' para 'resumo_df'
}