# app_rapida.R - Versão rápida para teste

source("scripts/utils.R")
source("scripts/forecasting.R")
source("scripts/optimization.R")

library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(DT)

ui <- navbarPage(
  title = "IDS System - Versão Rápida",
  theme = shinytheme("cosmo"),
  
  tabPanel("Principal",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Configurações"),
               
               selectInput("loja", "Loja:",
                           choices = c("Baltimore" = "baltimore",
                                       "Lancaster" = "lancaster",
                                       "Philadelphia" = "philadelphia",
                                       "Richmond" = "richmond"),
                           selected = "baltimore"),
               
               selectInput("modelo", "Modelo:",
                           choices = c("ARIMA" = "arima",
                                       "ETS" = "ets",
                                       "Holt-Winters" = "hw"),
                           selected = "arima"),
               
               sliderInput("horizonte", "Horizonte (dias):",
                           min = 1, max = 7, value = 7),
               
               radioButtons("objetivo", "Objetivo:",
                            choices = c("O1 - Maximizar Lucro" = "O1",
                                        "O2 - O1 + Limite 10k" = "O2"),
                            selected = "O1"),
               
               actionButton("executar", "Executar", 
                            class = "btn-primary"),
               
               hr(),
               
               h5("Info Loja"),
               textOutput("info_loja")
             ),
             
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel("Previsões",
                          br(),
                          h4("Previsão de Clientes"),
                          plotlyOutput("plot_previsao", height = "300px"),
                          br(),
                          DTOutput("tabela_previsao")
                 ),
                 
                 tabPanel("Plano",
                          br(),
                          h4("Plano Otimizado"),
                          DTOutput("tabela_plano"),
                          br(),
                          verbatimTextOutput("resumo_lucro")
                 )
               )
             )
           )
  )
)

server <- function(input, output, session) {
  
  # Valores reativos
  valores <- reactiveValues(
    dados = NULL,
    previsoes = NULL,
    plano = NULL
  )
  
  # Carregar dados
  observe({
    withProgress(message = 'A carregar dados...', {
      dados_lojas <- carregar_dados_lojas("dados/")
      valores$dados <- dados_lojas
    })
  })
  
  # Info da loja
  output$info_loja <- renderText({
    req(valores$dados, input$loja)
    dados_loja <- valores$dados[[input$loja]]
    if(!is.null(dados_loja)) {
      paste("Período:", min(dados_loja$Date), "a", max(dados_loja$Date),
            "\nDias:", nrow(dados_loja),
            "\nMédia clientes:", round(mean(dados_loja$Num_Customers), 0))
    } else {
      "Dados não disponíveis"
    }
  })
  
  # Executar
  observeEvent(input$executar, {
    req(valores$dados, input$loja)
    
    withProgress(message = 'A processar...', value = 0, {
      
      dados_loja <- valores$dados[[input$loja]]
      
      incProgress(0.3, detail = "Previsão...")
      
      # Modelo
      modelo <- switch(input$modelo,
                       "arima" = modelo_arima,
                       "ets" = modelo_ets,
                       "hw" = modelo_hw
      )
      
      previsoes <- modelo(dados_loja, input$horizonte)
      
      datas <- seq.Date(
        from = max(dados_loja$Date) + 1,
        by = "day",
        length.out = input$horizonte
      )
      
      valores$previsoes <- data.frame(
        Data = datas,
        Previsao = round(previsoes, 0)
      )
      
      incProgress(0.6, detail = "Otimização...")
      
      # Otimização rápida
      previsoes_list <- list()
      previsoes_list[[input$loja]] <- previsoes
      
      # Usar GA com menos iterações para ser rápido
      planos <- otimizar_recursos(previsoes_list, input$objetivo)
      valores$plano <- planos
      
      incProgress(1, detail = "Concluído!")
    })
  })
  
  # Tabela previsões
  output$tabela_previsao <- renderDT({
    req(valores$previsoes)
    
    valores$previsoes %>%
      mutate(Data = format(Data, "%d/%m/%Y")) %>%
      datatable(options = list(pageLength = 7, dom = 't'),
                rownames = FALSE)
  })
  
  # Gráfico previsões
  output$plot_previsao <- renderPlotly({
    req(valores$previsoes)
    
    p <- ggplot(valores$previsoes, aes(x = Data, y = Previsao)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 3) +
      labs(title = paste("Previsão -", input$loja),
           x = "Data", y = "Clientes") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Tabela plano
  output$tabela_plano <- renderDT({
    req(valores$plano)
    
    if(input$objetivo == "O1") {
      plano <- valores$plano[[input$loja]]
    } else {
      plano <- valores$plano[[input$loja]]
    }
    
    dias <- c("Seg", "Ter", "Qua", "Qui", "Sex", "Sáb", "Dom")[1:input$horizonte]
    
    df <- data.frame(
      Dia = dias,
      Junior = plano$J[1:input$horizonte],
      Expert = plano$X[1:input$horizonte],
      Total = plano$J[1:input$horizonte] + plano$X[1:input$horizonte],
      Promoção = paste0(round(plano$PR[1:input$horizonte] * 100, 1), "%")
    )
    
    datatable(options = list(pageLength = 7, dom = 't'),
              rownames = FALSE)
  })
  
  # Resumo lucro
  output$resumo_lucro <- renderPrint({
    req(valores$previsoes, valores$plano)
    
    if(input$objetivo == "O1") {
      plano <- valores$plano[[input$loja]]
      
      resultado <- calcular_lucro_semanal(
        plano = plano,
        C_previstos = valores$previsoes$Previsao,
        loja = input$loja
      )
      
      cat("=== RESUMO ===\n")
      cat("Lucro Semanal:", format_money(resultado$lucro), "\n")
      cat("Unidades:", format(resultado$unidades, big.mark = ","), "\n")
    }
  })
}

shinyApp(ui = ui, server = server)