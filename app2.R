################################################################################
# app.R - Versão 4.0
# Parâmetros IDÊNTICOS aos scripts run_hill_climbing_O1/O2/O3
################################################################################

library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(dplyr)

source("hill.R")
source("eval_plan_O1.R")
source("eval_plan_O2.R")
source("eval_plan_O3.R")

# Constantes (IGUAIS AOS SCRIPTS)
MAX_J <- 50
MAX_X <- 30
MAX_PR <- 0.30
MAX_SALES_UNITS <- 10000

# Carregar previsões
raw <- read.csv("all_store_predictions.csv")
forecasts <- data.frame(
  Store    = raw$Store,
  Week_ID  = raw$Run,
  Day      = raw$Step,
  Forecast = raw$Num_Customers
)

semanas_disponiveis <- sort(unique(forecasts$Week_ID))
store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")
store_names_print <- c("Baltimore", "Lancaster", "Philadelphia", "Richmond")

# ============================================================================
# FUNÇÕES IDÊNTICAS AOS SCRIPTS
# ============================================================================

# Solução inicial aleatória (como no script)
random_solution <- function() {
  lower <- rep(0, 84)
  upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))
  return(runif(84, min = lower, max = upper))
}

# Função de mudança (IGUAL à do script)
change_fn <- function(par, lower, upper) {
  hchange(par, lower = lower, upper = upper, operator = "*", 
          dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
}

# Executar Hill Climbing para O1 (IGUAL ao script)
run_optimization_O1 <- function(week_id, max_iter = 5000) {
  
  lower <- rep(0, 84)
  upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))
  
  eval_fn <- function(sol) {
    eval_plan_O1(sol, forecasts = forecasts, week_id = week_id, verbose = FALSE)
  }
  
  set.seed(123)  # Para reprodutibilidade
  s0 <- random_solution()
  
  hc <- hclimbing(par = s0, fn = eval_fn, change = change_fn,
                  lower = lower, upper = upper, type = "max",
                  control = list(maxit = max_iter, REPORT = 0, digits = 2))
  
  return(list(solution = hc$sol, profit = hc$eval))
}

# ============================================================================
# FUNÇÃO O2 - EXATAMENTE IGUAL AO run_hill_climbing_O2.R
# ============================================================================

run_optimization_O2 <- function(week_id, max_iter = 10000) {
  
  store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")
  max_sales_units <- 10000
  
  # 1. Cálculo de limites dinâmicos (como no script)
  max_J_dynamic <- numeric(28)
  max_X_dynamic <- numeric(28)
  
  idx <- 1
  for(s in 1:4) {
    fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      C_pred <- fc_store$Forecast[fc_store$Day == d]
      max_J_dynamic[idx] <- ceiling(C_pred * 1.5 / 6)
      max_X_dynamic[idx] <- ceiling(C_pred * 1.5 / 7)
      idx <- idx + 1
    }
  }
  
  max_J <- pmin(max_J_dynamic, 40)  # teto absoluto
  max_X <- pmin(max_X_dynamic, 25)
  max_PR <- rep(0.30, 28)
  
  lower <- rep(0, 84)
  upper <- c(max_J, max_X, max_PR)
  
  # 2. Solução inicial heurística (como no script)
  s0 <- array(0, dim = c(4, 7, 3))
  
  for(s in 1:4) {
    store <- store_names[s]
    fc_store <- forecasts[forecasts$Store == store & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      C_pred <- fc_store$Forecast[d]
      X_heur <- floor(C_pred / 7)
      rest <- C_pred - 7 * X_heur
      J_heur <- ceiling(rest / 6)
      if(J_heur < 0) J_heur <- 0
      
      idx <- (s-1)*7 + d
      X_heur <- min(X_heur, max_X[idx])
      J_heur <- min(J_heur, max_J[idx])
      
      s0[s, d, 1] <- J_heur
      s0[s, d, 2] <- X_heur
      s0[s, d, 3] <- 0.15
    }
  }
  s0 <- c(s0)
  
  # 3. Reparar solução inicial (garantir restrição)
  s0 <- repair_solution_inplace(s0, forecasts, week_id, max_sales_units)
  
  # 4. Função de mudança agressiva (como no script)
  change_fn_explore <- function(par, lower, upper) {
    new_par <- par
    for(i in 1:length(par)) {
      if(runif(1) < 0.3) {
        if(par[i] == 0) {
          new_par[i] <- runif(1, 0, upper[i] * 0.2)
        } else {
          factor <- rnorm(1, mean = 1, sd = 0.1)
          new_par[i] <- par[i] * factor
        }
        new_par[i] <- min(upper[i], max(lower[i], new_par[i]))
      }
    }
    return(new_par)
  }
  
  # 5. Função de avaliação
  eval_fn <- function(sol) {
    eval_plan_O2(sol, forecasts, week_id, max_sales_units, verbose = FALSE)
  }
  
  # 6. Executar Hill Climbing (COM A MESMA SEMENTE)
  set.seed(123)
  hc <- hclimbing(par = s0, fn = eval_fn, change = change_fn_explore,
                  lower = lower, upper = upper, type = "max",
                  control = list(maxit = max_iter, REPORT = 0, digits = 2))
  
  # 7. Verificação final (para debug)
  final_profit <- eval_plan_O2(hc$sol, forecasts, week_id, max_sales_units, verbose = FALSE)
  
  return(list(solution = hc$sol, profit = final_profit))
}

# ============================================================================
# FUNÇÃO O3 - EXATAMENTE IGUAL AO run_hill_climbing_O3.R
# ============================================================================

run_optimization_O3 <- function(week_id, max_iter = 8000, alpha = 0.1) {
  
  store_names <- c("baltimore", "lancaster", "philadelphia", "richmond")
  max_sales_units <- 10000
  
  # 1. Limites dinâmicos
  max_J <- numeric(28)
  max_X <- numeric(28)
  idx <- 1
  for(s in 1:4) {
    fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
    for(d in 1:7) {
      C_pred <- fc_store$Forecast[d]
      max_J[idx] <- min(ceiling(C_pred * 1.5 / 6), 40)
      max_X[idx] <- min(ceiling(C_pred * 1.5 / 7), 25)
      idx <- idx + 1
    }
  }
  
  max_PR <- rep(0.30, 28)
  lower <- rep(0, 84)
  upper <- c(max_J, max_X, max_PR)
  
  # 2. Solução inicial heurística
  build_s0 <- function() {
    s0 <- array(0, dim = c(4, 7, 3))
    for(s in 1:4) {
      fc_store <- forecasts[forecasts$Store == store_names[s] & forecasts$Week_ID == week_id, ]
      for(d in 1:7) {
        C_pred <- fc_store$Forecast[d]
        X_heur <- floor(C_pred / 7)
        rest <- C_pred - 7 * X_heur
        J_heur <- max(0, ceiling(rest / 6))
        X_heur <- min(X_heur, max_X[(s-1)*7 + d])
        J_heur <- min(J_heur, max_J[(s-1)*7 + d])
        s0[s, d, 1] <- J_heur
        s0[s, d, 2] <- X_heur
        s0[s, d, 3] <- 0.15
      }
    }
    s0 <- repair_solution_inplace(c(s0), forecasts, week_id, max_sales_units, max_J, max_X)
    return(s0)
  }
  
  # 3. Função de mudança mista
  change_fn_mixed <- function(par, lower, upper) {
    new_par <- par
    for(i in 1:length(par)) {
      if(runif(1) < 0.3) {
        if(par[i] == 0) {
          new_par[i] <- runif(1, 0, upper[i] * 0.2)
        } else {
          new_par[i] <- par[i] * rnorm(1, mean = 1, sd = 0.1)
        }
        new_par[i] <- min(upper[i], max(lower[i], new_par[i]))
      }
    }
    return(new_par)
  }
  
  # 4. Função de avaliação
  eval_fn_alpha <- function(sol) {
    res <- eval_plan_O3(sol, forecasts, week_id, max_sales_units,
                        verbose = FALSE, max_J = max_J, max_X = max_X,
                        alpha = alpha)
    return(res)
  }
  
  # 5. Executar Hill Climbing
  set.seed(123)
  s0 <- build_s0()
  
  hc <- hclimbing(par = s0, fn = eval_fn_alpha, change = change_fn_mixed,
                  lower = lower, upper = upper, type = "max",
                  control = list(maxit = max_iter, REPORT = 0, digits = 2))
  
  # 6. Obter componentes (sem verbose para não poluir o console)
  comps <- eval_plan_O3(hc$sol, forecasts, week_id, max_sales_units,
                        verbose = FALSE, max_J = max_J, max_X = max_X,
                        return_components = TRUE, alpha = alpha)
  
  return(list(solution = hc$sol, 
              profit = comps$total_profit,
              hr = comps$total_hr,
              sales_units = comps$total_sales_units))
}

# ============================================================================
# FUNÇÕES SANN (SIMULATED ANNEALING)
# ============================================================================

# SANN para O1 (baseado no SANN O1 Tracked.R corrigido)
run_sann_O1 <- function(week_id, max_iter = 5000, initial_temp = 5) {
  
  lower <- rep(0, 84)
  upper <- c(rep(MAX_J, 28), rep(MAX_X, 28), rep(MAX_PR, 28))
  
  # Neighbourhood generator (igual ao script)
  sann_gr <- function(par) {
    hchange(par, lower = lower, upper = upper, operator = "*",
            dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
  }
  
  # Função objetivo (USA eval_plan_O1)
  eval_fn <- function(sol) {
    -eval_plan_O1(sol, forecasts = forecasts, week_id = week_id, verbose = FALSE)
  }
  
  # Solução inicial aleatória (seed = 122 como no script)
  set.seed(122)
  s0 <- runif(84, min = lower, max = upper)
  
  control_sann <- list(maxit = max_iter, temp = initial_temp, trace = FALSE, tmax = 30)
  
  sann_result <- optim(par = s0,
                       fn = eval_fn,
                       gr = sann_gr,
                       method = "SANN",
                       control = control_sann)
  
  best_profit <- -sann_result$value
  
  return(list(solution = sann_result$par, profit = best_profit))
}

# ============================================================================
# SANN para O2 - EXATAMENTE IGUAL AO SCRIPT (limites fixos 50, 30)
# ============================================================================

run_sann_O2 <- function(week_id, max_iter = 5000, initial_temp = 50) {
  
  # LIMITES FIXOS (como no script SANN O2 Tracked.R)
  max_J <- 50
  max_X <- 30
  max_PR <- 0.30
  max_sales_units <- 10000
  
  lower <- rep(0, 84)
  upper <- c(rep(max_J, 28), rep(max_X, 28), rep(max_PR, 28))
  
  # Neighbourhood generator (igual ao script)
  sann_gr <- function(par) {
    hchange(par, lower = lower, upper = upper, operator = "*",
            dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
  }
  
  # Função de avaliação (igual ao script)
  eval_fn_sann <- function(sol) {
    -eval_plan_O2(sol, forecasts = forecasts, week_id = week_id, 
                  max_sales_units = max_sales_units, verbose = FALSE)
  }
  
  # Solução inicial ALEATÓRIA (igual ao script)
  set.seed(122)
  s0 <- runif(84, min = lower, max = upper)
  
  # Configuração do SANN (trace = FALSE para não poluir a app)
  control_sann <- list(maxit = max_iter, temp = initial_temp, trace = FALSE, tmax = 30)
  
  sann_result <- optim(par = s0, fn = eval_fn_sann, gr = sann_gr,
                       method = "SANN", control = control_sann)
  
  best_profit <- -sann_result$value
  
  # Aplicar reparação final para garantir restrição
  sol_repaired <- repair_solution_inplace(sann_result$par, forecasts, week_id, max_sales_units)
  final_profit <- eval_plan_O2(sol_repaired, forecasts, week_id, max_sales_units, verbose = FALSE)
  
  # Mostrar resultados na consola (para debug)
  cat(sprintf("\n=== SANN O2 Semana %d ===\n", week_id))
  cat(sprintf("Profit: $ %.2f\n", final_profit))
  cat(sprintf("Limites: max_J=%d, max_X=%d\n", max_J, max_X))
  
  return(list(solution = sol_repaired, profit = final_profit))
}


# ============================================================================
# SANN para O3 - EXATAMENTE IGUAL AO SCRIPT (limites fixos 50, 30)
# ============================================================================

run_sann_O3 <- function(week_id, max_iter = 5000, initial_temp = 50, alpha = 0.1) {
  
  # LIMITES FIXOS (como no script SANN O3 Tracked.R)
  max_J <- 50
  max_X <- 30
  max_PR <- 0.30
  max_sales_units <- 10000
  
  lower <- rep(0, 84)
  upper <- c(rep(max_J, 28), rep(max_X, 28), rep(max_PR, 28))
  
  # Neighbourhood generator (igual ao script)
  sann_gr <- function(par) {
    hchange(par, lower = lower, upper = upper, operator = "*",
            dist = rnorm, mean = 1, sd = 0.05, round = FALSE)
  }
  
  # Função de avaliação (igual ao script)
  # Nota: eval_plan_O3 retorna -(HR - profit * alpha) para maximização no hclimbing
  # Por isso, -eval_plan_O3 = HR - profit * alpha (que SANN minimiza)
  eval_fn_sann <- function(sol) {
    -eval_plan_O3(sol, forecasts = forecasts, week_id = week_id,
                  max_sales_units = max_sales_units, verbose = FALSE,
                  alpha = alpha)
  }
  
  # Solução inicial ALEATÓRIA (igual ao script)
  set.seed(123)
  s0 <- runif(84, min = lower, max = upper)
  
  # Configuração do SANN (trace = FALSE para não poluir a app)
  control_sann <- list(maxit = max_iter, temp = initial_temp, trace = FALSE, tmax = 20)
  
  sann_result <- optim(par = s0, fn = eval_fn_sann, gr = sann_gr,
                       method = "SANN", control = control_sann)
  
  # Obter componentes da melhor solução
  comps <- eval_plan_O3(sann_result$par, forecasts, week_id, max_sales_units,
                        verbose = FALSE, return_components = TRUE, alpha = alpha)
  
  # Aplicar reparação final para garantir restrição
  sol_repaired <- repair_solution_inplace(sann_result$par, forecasts, week_id, max_sales_units)
  comps_repaired <- eval_plan_O3(sol_repaired, forecasts, week_id, max_sales_units,
                                 verbose = FALSE, return_components = TRUE, alpha = alpha)
  
  # Se a solução reparada for melhor, usa-a
  if(comps_repaired$total_profit > comps$total_profit) {
    comps <- comps_repaired
    sann_result$par <- sol_repaired
  }
  
  # Mostrar resultados na consola (para debug)
  cat(sprintf("\n=== SANN O3 Semana %d ===\n", week_id))
  cat(sprintf("Profit: $ %.2f\n", comps$total_profit))
  cat(sprintf("HR total: %d\n", comps$total_hr))
  cat(sprintf("Sales units: %d / %d\n", comps$total_sales_units, max_sales_units))
  cat(sprintf("Objective (HR - profit*%.2f): %.2f\n", alpha, comps$total_hr - comps$total_profit * alpha))
  
  return(list(solution = sann_result$par, 
              profit = comps$total_profit,
              hr = comps$total_hr,
              sales_units = comps$total_sales_units))
}

# Processar solução para tabela (otimizada para ser rápida)
process_solution <- function(sol, week_id, objective, selected_store = NULL) {
  
  if(is.null(sol)) {
    return(list(details = data.frame(), total_profit = 0, total_sales_units = 0, total_hr = 0))
  }
  
  dim(sol) <- c(4, 7, 3)
  
  W_s <- c(700, 730, 760, 800)
  F_J <- c(1.00, 1.05, 1.10, 1.15)
  F_X <- c(1.15, 1.20, 1.15, 1.25)
  hr_cost_J_weekday <- 60
  hr_cost_J_weekend <- 70
  hr_cost_X_weekday <- 80
  hr_cost_X_weekend <- 95
  
  results <- data.frame()
  total_profit <- 0
  total_sales_units <- 0
  total_hr <- 0
  
  # Determinar quais lojas mostrar
  if(objective == "O1" && !is.null(selected_store)) {
    stores_to_show <- which(store_names_print == selected_store)
  } else {
    stores_to_show <- 1:4
  }
  
  for(s in stores_to_show) {
    store <- store_names[s]
    fc_store <- forecasts[forecasts$Store == store & forecasts$Week_ID == week_id, ]
    if(nrow(fc_store) != 7) next
    
    for(d in 1:7) {
      J <- max(0, round(sol[s, d, 1]))
      X <- max(0, round(sol[s, d, 2]))
      PR <- min(0.30, max(0, sol[s, d, 3]))
      
      C_pred <- fc_store$Forecast[fc_store$Day == d]
      max_assisted <- 7 * X + 6 * J
      A <- min(max_assisted, C_pred)
      
      is_weekend <- (d %in% c(1, 7))
      cost_J <- ifelse(is_weekend, hr_cost_J_weekend, hr_cost_J_weekday)
      cost_X <- ifelse(is_weekend, hr_cost_X_weekend, hr_cost_X_weekday)
      daily_cost_hr <- J * cost_J + X * cost_X
      
      n_assisted_by_X <- min(X * 7, A)
      n_assisted_by_J <- A - n_assisted_by_X
      
      daily_sales_units <- 0
      daily_revenue_raw <- 0
      
      if(n_assisted_by_X > 0) {
        U_X <- round(F_X[s] * 10 / log(2 - PR))
        P_X_raw <- U_X * (1 - PR) * 1.07
        daily_sales_units <- daily_sales_units + n_assisted_by_X * U_X
        daily_revenue_raw <- daily_revenue_raw + n_assisted_by_X * P_X_raw
      }
      if(n_assisted_by_J > 0) {
        U_J <- round(F_J[s] * 10 / log(2 - PR))
        P_J_raw <- U_J * (1 - PR) * 1.07
        daily_sales_units <- daily_sales_units + n_assisted_by_J * U_J
        daily_revenue_raw <- daily_revenue_raw + n_assisted_by_J * P_J_raw
      }
      
      daily_revenue <- round(daily_revenue_raw)
      daily_net_profit <- daily_revenue - daily_cost_hr
      
      weekly_profit_contrib <- daily_net_profit
      total_sales_units <- total_sales_units + daily_sales_units
      total_hr <- total_hr + J + X
      
      day_name <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[d]
      
      results <- rbind(results, data.frame(
        Store = store_names_print[s],
        Day = day_name,
        J = J,
        X = X,
        PR = sprintf("%.1f%%", PR * 100),
        Customers = round(C_pred),
        Assisted = round(A),
        Sold_Units = round(daily_sales_units),
        Revenue = sprintf("$%.0f", daily_revenue),
        HR_Cost = sprintf("$%.0f", daily_cost_hr),
        Daily_Profit = sprintf("$%.0f", daily_net_profit)
      ))
      
      total_profit <- total_profit + daily_net_profit
    }
    total_profit <- total_profit - W_s[s]
  }
  
  return(list(details = results, 
              total_profit = total_profit,
              total_sales_units = total_sales_units,
              total_hr = total_hr))
}

# ============================================================================
# UI
# ============================================================================

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(
    div(style = "text-align: center;",
        h1("Intelligent Decision Support System"),
        h4("USA Stores - Forecasting & Optimization")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      conditionalPanel(
        condition = "input.objective == 'O1'",
        selectInput("store", "Store",
                    choices = store_names_print,
                    selected = "Baltimore")
      ),
      
      selectInput("week", "Week",
                  choices = semanas_disponiveis,
                  selected = 20),
      
      radioButtons("objective", "Optimization Goal",
                   choices = c(
                     "O1: Maximize Profit (per store)" = "O1",
                     "O2: Maximize Profit (all stores, ≤10,000 units)" = "O2",
                     "O3: Maximize Profit + Minimize HR (all stores)" = "O3"
                   ),
                   selected = "O1"),
      
      radioButtons("method", "Optimization Method",
                   choices = c(
                     "Hill Climbing (HC)" = "HC",
                     "Simulated Annealing (SANN)" = "SANN"
                   ),
                   selected = "HC"),
      
      conditionalPanel(
        condition = "input.objective == 'O1'",
        numericInput("max_iter_O1", "Max Iterations", value = 5000, min = 500, max = 20000, step = 500)
      ),
      conditionalPanel(
        condition = "input.objective == 'O2'",
        numericInput("max_iter_O2", "Max Iterations", value = 10000, min = 1000, max = 50000, step = 1000)
      ),
      conditionalPanel(
        condition = "input.objective == 'O3'",
        numericInput("max_iter_O3", "Max Iterations", value = 8000, min = 1000, max = 50000, step = 1000)
      ),
      
      actionButton("run", "Generate Optimized Plan",
                   class = "btn-primary btn-lg",
                   style = "width: 100%; margin-top: 20px;"),
      
      hr(),
      div(style = "font-size: 12px; color: gray;",
          p("Hill Climbing Optimization (same parameters as scripts)"),
          p("Max J = 50, Max X = 30, Max PR = 30%")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Forecasts",
                 br(),
                 h4("Customer Forecasts for Selected Week"),
                 DTOutput("forecast_table"),
                 br(),
                 h4("Forecast Visualization"),
                 plotlyOutput("forecast_plot", height = "400px")
        ),
        
        tabPanel("Optimized Plan",
                 br(),
                 div(style = "background-color: #e8f4f8; padding: 15px; border-radius: 8px; margin-bottom: 20px;",
                     h4("Summary Results"),
                     fluidRow(
                       column(4, h5("Total Profit:"), h3(textOutput("total_profit"))),
                       column(4, h5("Total Sold Units:"), h3(textOutput("total_units"))),
                       column(4, h5("Total HR (J+X):"), h3(textOutput("total_hr")))
                     )
                 ),
                 br(),
                 h4("Detailed Weekly Plan"),
                 DTOutput("plan_table"),
                 br(),
                 h4("Profit by Store"),
                 plotlyOutput("profit_chart", height = "400px")
        ),
        
        tabPanel("About",
                 br(),
                 h4("Decision Support System for USA Stores"),
                 p("This system integrates:"),
                 tags$ul(
                   tags$li("Customer forecasts for 4 stores (104 weeks)"),
                   tags$li("Hill Climbing optimization for 3 objectives"),
                   tags$li("Parameters identical to run_hill_climbing_O1/O2/O3.R scripts")
                 )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  
  opt_results <- reactiveVal(NULL)
  is_running <- reactiveVal(FALSE)
  
  # Mostrar previsões
  output$forecast_table <- renderDT({
    store_sel <- tolower(input$store)
    fc_data <- forecasts[forecasts$Store == store_sel & forecasts$Week_ID == input$week, ]
    fc_data <- fc_data[order(fc_data$Day), ]
    
    day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    
    fc_display <- data.frame(
      Day = day_names[fc_data$Day],
      Forecast_Customers = round(fc_data$Forecast, 0)
    )
    
    datatable(fc_display, 
              options = list(pageLength = 7, dom = 't'),
              rownames = FALSE) %>%
      formatStyle("Forecast_Customers",
                  background = styleColorBar(fc_data$Forecast, "lightblue"),
                  backgroundSize = '100% 90%',
                  backgroundRepeat = 'no-repeat')
  })
  
  output$forecast_plot <- renderPlotly({
    store_sel <- tolower(input$store)
    fc_data <- forecasts[forecasts$Store == store_sel & forecasts$Week_ID == input$week, ]
    fc_data <- fc_data[order(fc_data$Day), ]
    
    day_names <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
    
    plot_ly() %>%
      add_trace(x = day_names, y = fc_data$Forecast,
                type = 'scatter', mode = 'lines+markers',
                name = 'Forecast',
                line = list(color = 'steelblue', width = 3),
                marker = list(size = 10, color = 'steelblue')) %>%
      layout(title = paste("Customer Forecast -", input$store),
             xaxis = list(title = "Day"),
             yaxis = list(title = "Number of Customers"))
  })
  
  # Executar otimização
  observeEvent(input$run, {
    if(is_running()) {
      showNotification("Optimization already running...", type = "warning")
      return()
    }
    
    is_running(TRUE)
    
    max_iter <- switch(input$objective,
                       O1 = input$max_iter_O1,
                       O2 = input$max_iter_O2,
                       O3 = input$max_iter_O3)
    
    showNotification(paste("Running", input$method, "-", input$objective, "optimization for", 
                           ifelse(input$objective == "O1", input$store, "all stores"),
                           "- this may take a few minutes..."), 
                     type = "default", duration = 5)
    
    withProgress(message = paste(input$method, '-', input$objective, 'optimizing...'), value = 0, {
      
      result <- NULL
      
      if(input$objective == "O1") {
        incProgress(0.2, detail = "Setting up O1 optimization")
        if(input$method == "HC") {
          result <- run_optimization_O1(as.numeric(input$week), max_iter)
        } else {
          result <- run_sann_O1(as.numeric(input$week), max_iter, initial_temp = 5)
        }
      } else if(input$objective == "O2") {
        incProgress(0.2, detail = "Setting up O2 optimization (this may take longer)")
        if(input$method == "HC") {
          result <- run_optimization_O2(as.numeric(input$week), max_iter)
        } else {
          result <- run_sann_O2(as.numeric(input$week), max_iter, initial_temp = 50)
        }
      } else {
        incProgress(0.2, detail = "Setting up O3 optimization")
        if(input$method == "HC") {
          result <- run_optimization_O3(as.numeric(input$week), max_iter, alpha = 0.1)
        } else {
          result <- run_sann_O3(as.numeric(input$week), max_iter, initial_temp = 50, alpha = 0.1)
        }
      }
      
      incProgress(0.7, detail = "Processing results")
      
      # Para O3, usar valores diretos da otimização
      if(input$objective == "O3") {
        processed <- process_solution(result$solution, as.numeric(input$week), 
                                      input$objective, input$store)
        
        opt_results(list(
          details = processed$details,
          total_profit = result$profit,
          total_sales_units = result$sales_units,
          total_hr = result$hr
        ))
      } else {
        processed <- process_solution(result$solution, as.numeric(input$week), 
                                      input$objective, input$store)
        
        opt_results(list(
          details = processed$details,
          total_profit = processed$total_profit,
          total_sales_units = processed$total_sales_units,
          total_hr = processed$total_hr
        ))
      }
      
      incProgress(1, detail = "Complete!")
    })
    
    showNotification(paste("Optimization completed! Profit: $", 
                           round(opt_results()$total_profit, 2)), 
                     type = "default", duration = 3)
    
    is_running(FALSE)
  })
  
  output$total_profit <- renderText({
    res <- opt_results()
    if(is.null(res)) return("Click 'Generate'")
    sprintf("$ %.2f", res$total_profit)
  })
  
  output$total_units <- renderText({
    res <- opt_results()
    if(is.null(res)) return("Click 'Generate'")
    format(round(res$total_sales_units), big.mark = ",")
  })
  
  output$total_hr <- renderText({
    res <- opt_results()
    if(is.null(res)) return("Click 'Generate'")
    res$total_hr
  })
  
  output$plan_table <- renderDT({
    res <- opt_results()
    if(is.null(res) || nrow(res$details) == 0) {
      return(datatable(data.frame(Message = "Click 'Generate Optimized Plan'")))
    }
    
    datatable(res$details,
              options = list(pageLength = 28, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle("Daily_Profit",
                  color = styleInterval(0, c("red", "darkgreen")))
  })
  
  output$profit_chart <- renderPlotly({
    res <- opt_results()
    if(is.null(res) || nrow(res$details) == 0) return(NULL)
    
    profit_by_store <- res$details %>%
      group_by(Store) %>%
      summarise(Profit = sum(as.numeric(gsub("\\$", "", Daily_Profit))))
    
    plot_ly() %>%
      add_trace(x = profit_by_store$Store, y = profit_by_store$Profit,
                type = 'bar', name = 'Profit',
                marker = list(color = c('#2ecc71', '#3498db', '#e74c3c', '#9b59b6'))) %>%
      layout(title = "Weekly Profit by Store",
             xaxis = list(title = "Store"),
             yaxis = list(title = "Profit ($)"))
  })
}

shinyApp(ui = ui, server = server)