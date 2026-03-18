# ==================================================
# app.R - Aplicação Shiny do Sistema de Suporte à Decisão
# ==================================================

library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(DT)
library(tidyverse)

# Carregar scripts
source("scripts/utils.R")
source("scripts/forecasting.R")
source("scripts/optimization.R")

# UI - Interface do Utilizador
ui <- navbarPage(
  title = "Intelligent Decision Support System - USA Stores",
  theme = shinytheme("cosmo"),
  
  # Aba Principal
  tabPanel("Sistema Principal",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("⚙️ Configurações"),
               
               selectInput("loja", "Selecionar Loja:",
                           choices = c("Baltimore" = "baltimore",
                                       "Lancaster" = "lancaster",
                                       "Philadelphia" = "philadelphia",
                                       "Richmond" = "richmond"),
                           selected = "baltimore"),
               
               selectInput("modelo", "Modelo de Previsão:",
                           choices = c("ARIMA" = "arima",
                                       "ETS" = "ets",
                                       "Holt-Winters" = "hw",
                                       "Random Forest" = "rf",
                                       "Neural Network" = "nnetar"),
                           selected = "arima"),
               
               sliderInput("horizonte", "Horizonte de Previsão (dias):",
                           min = 1, max = 7, value = 7),
               
               radioButtons("objetivo", "Objetivo de Otimização:",
                            choices = c(
                              "O1 - Maximizar Lucro" = "O1",
                              "O2 - O1 + Limite 10k unidades" = "O2",
                              "O3 - O2 + Minimizar RH" = "O3"
                            ),
                            selected = "O1"),
               
               actionButton("executar", "🚀 Executar Sistema", 
                            class = "btn-primary btn-lg btn-block"),
               
               hr(),
               
               h5("📊 Informações da Loja"),
               textOutput("info_loja"),
               br(),
               
               conditionalPanel(
                 condition = "input.executar > 0",
                 downloadButton("download_plano", "📥 Download Plano", 
                                class = "btn-success btn-block")
               )
             ),
             
             mainPanel(
               width = 9,
               tabsetPanel(
                 tabPanel("📈 Previsões",
                          br(),
                          fluidRow(
                            column(12,
                                   h4("Previsão de Clientes para os Próximos Dias"),
                                   plotlyOutput("plot_previsao", height = "400px"),
                                   br(),
                                   DTOutput("tabela_previsao")
                            )
                          )
                 ),
                 
                 tabPanel("📋 Plano Otimizado",
                          br(),
                          fluidRow(
                            column(12,
                                   h4("Plano Semanal de Recursos"),
                                   DTOutput("tabela_plano"),
                                   br(),
                                   h4("Resumo Financeiro"),
                                   verbatimTextOutput("resumo_financeiro"),
                                   br(),
                                   plotlyOutput("plot_alocacao", height = "300px")
                            )
                          )
                 ),
                 
                 tabPanel("📊 Análise Comparativa",
                          br(),
                          fluidRow(
                            column(12,
                                   h4("Comparação de Modelos de Previsão"),
                                   plotlyOutput("plot_comparacao", height = "400px"),
                                   br(),
                                   h4("Métricas de Desempenho"),
                                   DTOutput("tabela_metricas")
                            )
                          )
                 ),
                 
                 tabPanel("💰 Análise Financeira",
                          br(),
                          fluidRow(
                            column(12,
                                   h4("Projeção de Lucro Diário"),
                                   plotlyOutput("plot_lucro_diario", height = "300px"),
                                   br(),
                                   h4("Análise de Sensibilidade"),
                                   plotlyOutput("plot_sensibilidade", height = "300px")
                            )
                          )
                 )
               )
             )
           )
  ),
  
  # Aba de Documentação
  tabPanel("📚 Documentação",
           fluidRow(
             column(8, offset = 2,
                    br(),
                    h2("Documentação do Sistema"),
                    hr(),
                    h4("Objetivos"),
                    p("Este sistema implementa um modelo de suporte à decisão para 
          otimização de recursos humanos e promoções em 4 lojas nos EUA."),
                    
                    h4("Funcionalidades"),
                    tags$ul(
                      tags$li("Previsão de clientes usando múltiplos modelos (ARIMA, ETS, Holt-Winters, Random Forest, Neural Network)"),
                      tags$li("Otimização de recursos humanos (funcionários Junior e Expert)"),
                      tags$li("Otimização de percentagens de promoção (0-30%)"),
                      tags$li("Três objetivos de otimização: O1 (maximizar lucro), O2 (O1 + limite 10k unidades), O3 (O2 + minimizar RH)"),
                      tags$li("Interface interativa para visualização de resultados"),
                      tags$li("Download de planos otimizados em formato CSV")
                    ),
                    
                    h4("Como usar"),
                    tags$ol(
                      tags$li("Selecione a loja no painel lateral"),
                      tags$li("Escolha o modelo de previsão"),
                      tags$li("Defina o horizonte de previsão (1-7 dias)"),
                      tags$li("Selecione o objetivo de otimização"),
                      tags$li("Clique em 'Executar Sistema'"),
                      tags$li("Explore os resultados nas diferentes abas")
                    ),
                    
                    h4("Equipa"),
                    p("Projeto desenvolvido no âmbito da unidade curricular..."),
                    
                    h4("Video Demonstração"),
                    p("Link do YouTube: https://youtu.be/..."),
                    
                    br(),
                    br()
             )
           )
  )
)

# Server - Lógica do Servidor
server <- function(input, output, session) {
  
  # Valores reativos
  valores <- reactiveValues(
    dados = NULL,
    previsoes = NULL,
    plano_otimizado = NULL,
    resultados_modelos = NULL
  )
  
  # Carregar dados iniciais
  observe({
    withProgress(message = 'A carregar dados...', {
      # Carregar dados das lojas
      # Nota: Ajustar o caminho conforme necessário
      dados_lojas <- carregar_dados_lojas("dados/")
      valores$dados <- dados_lojas
    })
  })
  
  # Informações da loja selecionada
  output$info_loja <- renderText({
    req(valores$dados, input$loja)
    
    dados_loja <- valores$dados[[input$loja]]
    if(!is.null(dados_loja)) {
      paste("Período:", min(dados_loja$Date), "a", max(dados_loja$Date),
            "\nTotal dias:", nrow(dados_loja),
            "\nMédia clientes:", round(mean(dados_loja$Num_Customers), 0))
    } else {
      "Dados não disponíveis"
    }
  })
  
  # Executar sistema quando botão for clicado
  observeEvent(input$executar, {
    req(valores$dados, input$loja)
    
    withProgress(message = 'A processar...', value = 0, {
      
      # Obter dados da loja
      dados_loja <- valores$dados[[input$loja]]
      
      incProgress(0.2, detail = "A fazer previsões...")
      
      # Selecionar modelo de previsão
      modelo <- switch(input$modelo,
                       "arima" = modelo_arima,
                       "ets" = modelo_ets,
                       "hw" = modelo_hw,
                       "rf" = modelo_rf,
                       "nnetar" = modelo_nnetar
      )
      
      # Fazer previsão
      previsoes <- modelo(dados_loja, input$horizonte)
      
      # Criar datas para previsão
      datas_previsao <- seq.Date(
        from = max(dados_loja$Date) + 1,
        by = "day",
        length.out = input$horizonte
      )
      
      # Guardar previsões
      valores$previsoes <- data.frame(
        Data = datas_previsao,
        Previsao = round(previsoes, 0),
        DiaSemana = wday(datas_previsao, label = TRUE, week_start = 1)
      )
      
      incProgress(0.5, detail = "A otimizar recursos...")
      
      # Preparar lista de previsões para otimização
      # Para O2 e O3 precisamos de todas as lojas
      if(input$objetivo %in% c("O2", "O3")) {
        # Fazer previsões para todas as lojas
        lojas <- names(valores$dados)
        previsoes_list <- list()
        
        for(loja in lojas) {
          dados_loja_temp <- valores$dados[[loja]]
          pred_temp <- modelo(dados_loja_temp, input$horizonte)
          previsoes_list[[loja]] <- pred_temp
        }
        
        # Otimizar para todas as lojas
        planos <- otimizar_recursos(previsoes_list, input$objetivo)
        valores$plano_otimizado <- planos
        
      } else {
        # O1 - otimizar apenas para loja selecionada
        previsoes_list <- list()
        previsoes_list[[input$loja]] <- previsoes
        
        planos <- otimizar_recursos(previsoes_list, input$objetivo)
        valores$plano_otimizado <- planos
      }
      
      incProgress(0.8, detail = "A avaliar modelos...")
      
      # Avaliar modelos (apenas na primeira execução)
      if(is.null(valores$resultados_modelos)) {
        valores$resultados_modelos <- avaliar_modelos(dados_loja, teste_dias = 30)
      }
      
      incProgress(1, detail = "Concluído!")
    })
  })
  
  # Tabela de previsões
  output$tabela_previsao <- renderDT({
    req(valores$previsoes)
    
    valores$previsoes %>%
      mutate(
        Data = format(Data, "%d/%m/%Y"),
        Previsao = as.integer(Previsao)
      ) %>%
      datatable(
        options = list(
          pageLength = 7,
          dom = 't'
        ),
        rownames = FALSE,
        colnames = c("Data", "Clientes Previstos", "Dia Semana")
      )
  })
  
  # Gráfico de previsões
  output$plot_previsao <- renderPlotly({
    req(valores$previsoes)
    
    p <- ggplot(valores$previsoes, aes(x = Data, y = Previsao)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 3) +
      geom_text(aes(label = Previsao), vjust = -1, size = 3) +
      labs(title = paste("Previsão de Clientes -", input$loja),
           x = "Data", y = "Número de Clientes") +
      theme_minimal() +
      scale_x_date(date_labels = "%d/%m", date_breaks = "1 day")
    
    ggplotly(p)
  })
  
  # Tabela do plano otimizado
  output$tabela_plano <- renderDT({
    req(valores$plano_otimizado)
    
    if(input$objetivo == "O1") {
      plano <- valores$plano_otimizado[[input$loja]]
    } else {
      plano <- valores$plano_otimizado[[input$loja]]
    }
    
    dias_semana <- c("Segunda", "Terça", "Quarta", "Quinta", 
                     "Sexta", "Sábado", "Domingo")[1:input$horizonte]
    
    df <- data.frame(
      Dia = dias_semana,
      Junior = plano$J,
      Expert = plano$X,
      Total_Func = plano$J + plano$X,
      Promocao = paste0(round(plano$PR * 100, 1), "%")
    )
    
    datatable(
      df,
      options = list(
        pageLength = 7,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
  
  # Resumo financeiro
  output$resumo_financeiro <- renderPrint({
    req(valores$previsoes, valores$plano_otimizado)
    
    if(input$objetivo == "O1") {
      plano <- valores$plano_otimizado[[input$loja]]
      
      # Calcular lucro
      resultado <- calcular_lucro_semanal(
        plano = plano,
        C_previstos = valores$previsoes$Previsao,
        loja = input$loja
      )
      
      cat("=== RESUMO FINANCEIRO ===\n\n")
      cat("Loja:", input$loja, "\n")
      cat("Objetivo:", input$objetivo, "\n")
      cat("Lucro Semanal:", format_money(resultado$lucro), "\n")
      cat("Unidades Vendidas:", format(resultado$unidades, big.mark = ","), "\n\n")
      
      cat("Lucro por Dia:\n")
      for(i in 1:length(resultado$lucros_diarios)) {
        cat("  Dia", i, ":", format_money(resultado$lucros_diarios[i]), "\n")
      }
      
    } else {
      cat("=== RESUMO COMBINADO ===\n\n")
      cat("Objetivo:", input$objetivo, "\n")
      cat("Fitness Total:", format_money(valores$plano_otimizado$fitness_total), "\n")
    }
  })
  
  # Gráfico de alocação de recursos
  output$plot_alocacao <- renderPlotly({
    req(valores$plano_otimizado)
    
    if(input$objetivo == "O1") {
      plano <- valores$plano_otimizado[[input$loja]]
    } else {
      plano <- valores$plano_otimizado[[input$loja]]
    }
    
    dias <- 1:input$horizonte
    
    df <- data.frame(
      Dia = rep(dias, 2),
      Tipo = c(rep("Junior", input$horizonte), rep("Expert", input$horizonte)),
      Quantidade = c(plano$J[1:input$horizonte], plano$X[1:input$horizonte])
    )
    
    p <- ggplot(df, aes(x = factor(Dia), y = Quantidade, fill = Tipo)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Alocação de Funcionários por Dia",
           x = "Dia", y = "Número de Funcionários") +
      theme_minimal() +
      scale_fill_manual(values = c("Junior" = "lightblue", "Expert" = "darkblue"))
    
    ggplotly(p)
  })
  
  # Gráfico de lucro diário
  output$plot_lucro_diario <- renderPlotly({
    req(valores$previsoes, valores$plano_otimizado)
    
    if(input$objetivo == "O1") {
      plano <- valores$plano_otimizado[[input$loja]]
      
      resultado <- calcular_lucro_semanal(
        plano = plano,
        C_previstos = valores$previsoes$Previsao,
        loja = input$loja
      )
      
      df <- data.frame(
        Dia = 1:input$horizonte,
        Lucro = resultado$lucros_diarios
      )
      
      p <- ggplot(df, aes(x = Dia, y = Lucro)) +
        geom_col(fill = "forestgreen", alpha = 0.7) +
        geom_text(aes(label = format_money(Lucro)), vjust = -0.5, size = 3) +
        labs(title = "Lucro Diário Projetado",
             x = "Dia", y = "Lucro ($)") +
        theme_minimal() +
        scale_y_continuous(labels = scales::dollar)
      
      ggplotly(p)
    }
  })
  
  # Gráfico de comparação de modelos
  output$plot_comparacao <- renderPlotly({
    req(valores$resultados_modelos)
    
    p <- plot_comparacao_modelos(valores$resultados_modelos)
    ggplotly(p)
  })
  
  # Tabela de métricas
  output$tabela_metricas <- renderDT({
    req(valores$resultados_modelos)
    
    valores$resultados_modelos$metricas %>%
      datatable(
        options = list(
          pageLength = 7,
          dom = 't'
        ),
        rownames = FALSE
      )
  })
  
  # Gráfico de sensibilidade (placeholder)
  output$plot_sensibilidade <- renderPlotly({
    # Simular análise de sensibilidade
    promocoes <- seq(0, 0.3, by = 0.05)
    lucros <- sapply(promocoes, function(pr) {
      # Simulação simplificada
      mean_clientes <- 200
      unidades <- round(1.1 * 10 / log(2 - pr))
      unidades * mean_clientes * 1.07 * (1 - pr)
    })
    
    df <- data.frame(
      Promocao = promocoes * 100,
      Lucro = lucros
    )
    
    p <- ggplot(df, aes(x = Promocao, y = Lucro)) +
      geom_line(color = "darkred", size = 1) +
      geom_point(color = "darkred", size = 3) +
      labs(title = "Análise de Sensibilidade - Impacto da Promoção no Lucro",
           x = "Percentagem de Promoção (%)", 
           y = "Lucro Estimado ($)") +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar)
    
    ggplotly(p)
  })
  
  # Download do plano
  output$download_plano <- downloadHandler(
    filename = function() {
      paste0("plano_", input$loja, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(valores$plano_otimizado)
      
      if(input$objetivo == "O1") {
        plano <- valores$plano_otimizado[[input$loja]]
      } else {
        plano <- valores$plano_otimizado[[input$loja]]
      }
      
      dias_semana <- c("Segunda", "Terça", "Quarta", "Quinta", 
                       "Sexta", "Sábado", "Domingo")[1:input$horizonte]
      
      df <- data.frame(
        Dia = dias_semana,
        Data = format(seq.Date(Sys.Date(), by = "day", length.out = input$horizonte), "%Y-%m-%d"),
        Clientes_Previstos = valores$previsoes$Previsao,
        Funcionarios_Junior = plano$J[1:input$horizonte],
        Funcionarios_Expert = plano$X[1:input$horizonte],
        Promocao_Percent = round(plano$PR[1:input$horizonte] * 100, 1)
      )
      
      write.csv(df, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)    