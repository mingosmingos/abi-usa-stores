############################################################
### PACKAGES
############################################################
library(forecast)
library(rminer)
library(GA)
library(dplyr)
library(readr)
library(ggplot2)

############################################################
### 1. LOAD STORE DATA
############################################################
load_store <- function(path){
  df <- read_csv(path, show_col_types = FALSE)
  df$Date <- as.Date(df$Date)
  df <- df[order(df$Date), ]
  return(df)
}

baltimore     <- load_store("baltimore.csv")
lancaster     <- load_store("lancaster.csv")
philadelphia  <- load_store("philadelphia.csv")
richmond      <- load_store("richmond.csv")

stores <- list(
  baltimore=baltimore,
  lancaster=lancaster,
  philadelphia=philadelphia,
  richmond=richmond
)

############################################################
### 2. FORECASTING FUNCTIONS
############################################################

## ARIMA
forecast_arima <- function(df, h=7){
  ts_data <- ts(df$Num_Customers, frequency=7)
  fit <- auto.arima(ts_data)
  preds <- forecast(fit, h=h)$mean
  return(as.numeric(preds))
}

## rminer SVM multivariate
forecast_miner <- function(df, h=7){
  df2 <- df %>% mutate(TouristEvent = ifelse(TouristEvent=="Yes",1,0))
  
  cs <- CasesSeries(
    df2$Num_Customers,
    df2[,c("Num_Employees","Pct_On_Sale","TouristEvent","Sales")],
    window=h+7
  )
  
  model <- fit(Num_Customers ~ ., data=cs, model="svm")
  
  preds <- lforecast(model, cs, h=h)
  return(as.numeric(preds))
}

############################################################
### 3. STORE CONFIG (from project.pdf)
############################################################
store_cfg <- list(
  baltimore     = list(Fj=1.00, Fx=1.15, W=700),
  lancaster     = list(Fj=1.05, Fx=1.20, W=730),
  philadelphia  = list(Fj=1.10, Fx=1.15, W=760),
  richmond      = list(Fj=1.15, Fx=1.25, W=800)
)

############################################################
### 4. DAILY PROFIT FUNCTION
############################################################
compute_day_profit <- function(store, Cpred, J, X, PR){
  cfg <- store_cfg[[store]]
  Fj <- cfg$Fj ; Fx <- cfg$Fx
  
  assisted <- min(7*X + 6*J, Cpred)
  
  profit <- 0
  for(i in 1:assisted){
    F <- ifelse(i <= 7*X, Fx, Fj)
    U <- round(F * 10 / log(2 - PR))
    P <- round(U * (1 - PR) * 1.07)
    profit <- profit + P
  }
  
  hr_cost <- 80*J + 80*X
  
  return(list(profit=profit - hr_cost, units=assisted))
}

############################################################
### 5. WEEKLY PROFIT
############################################################
compute_week_profit <- function(store, preds, plan){
  total_profit <- 0
  total_units <- 0
  
  for(day in 1:7){
    r <- compute_day_profit(
      store,
      preds[day],
      plan$J[day],
      plan$X[day],
      plan$PR[day]
    )
    
    total_profit <- total_profit + r$profit
    total_units  <- total_units  + r$units
  }
  
  total_profit <- total_profit - store_cfg[[store]]$W
  
  return(list(profit=total_profit, units=total_units))
}

############################################################
### 6. OBJECTIVE FUNCTION FOR GA
############################################################
objective_function <- function(vec, store, preds, objective){
  J <- vec[1:7]
  X <- vec[8:14]
  PR <- vec[15:21]
  
  plan <- data.frame(
    J=round(J),
    X=round(X),
    PR=pmax(0, pmin(PR, 0.30))
  )
  
  result <- compute_week_profit(store, preds, plan)
  profit <- result$profit
  units  <- result$units
  
  if(objective=="O2" && units > 10000)
    return(-1e9)
  
  if(objective=="O3"){
    HRtotal <- sum(plan$J + plan$X)
    return(profit - 0.1 * HRtotal)
  }
  
  return(profit)
}

############################################################
### 7. OPTIMIZATION (GA)
############################################################
optimize_store <- function(store, preds, objective="O1"){
  GA <- ga(
    type="real-valued",
    fitness=function(v) objective_function(v, store, preds, objective),
    lower=c(rep(0,14), rep(0,7)),
    upper=c(rep(20,14), rep(0.30,7)),
    popSize=70,
    maxiter=50,
    run=40
  )
  
  sol <- GA@solution
  
  return(list(
    best=sol,
    score=objective_function(sol, store, preds, objective),
    GA=GA
  ))
}

############################################################
### 8. PLOTS
############################################################

plot_forecast <- function(df, preds, store){
  real_last <- tail(df, 30)
  fc <- data.frame(
    Date = max(df$Date) + 1:length(preds),
    Num_Customers = preds
  )
  
  ggplot() +
    geom_line(data=real_last, aes(Date, Num_Customers), color="blue", size=1.2) +
    geom_line(data=fc, aes(Date, Num_Customers), color="red", size=1.2) +
    labs(title=paste("Forecast for", store),
         subtitle="Blue = Real (last 30 days), Red = Forecast",
         y="Num Customers") +
    theme_minimal()
}

plot_plan <- function(plan, store){
  df <- data.frame(
    Day = 1:7,
    J = plan$J,
    X = plan$X,
    PR = plan$PR
  )
  
  ggplot(df, aes(Day)) +
    geom_line(aes(y=J, color="Junior HR"), size=1.2) +
    geom_line(aes(y=X, color="Expert HR"), size=1.2) +
    geom_line(aes(y=PR*10, color="Promoções (% x10)"), size=1.1, linetype="dashed") +
    labs(title=paste("Weekly Plan —", store),
         y="HR / Promotions (scaled)",
         subtitle="PR multiplied by 10 for visibility") +
    theme_minimal()
}

############################################################
### 9. PIPELINE for 4 stores
############################################################
full_system <- function(store_name, df, method="arima", objective="O1"){
  
  preds <- if(method=="arima") forecast_arima(df) else forecast_miner(df)
  
  opt <- optimize_store(store_name, preds, objective)
  
  sol <- opt$best
  
  plan <- data.frame(
    J=round(sol[1:7]),
    X=round(sol[8:14]),
    PR=sol[15:21]
  )
  
  return(list(
    store = store_name,
    preds = preds,
    plan = plan,
    score = opt$score
  ))
}

############################################################
### 10. RUN FOR ALL 4 STORES
############################################################
results <- list()

for(name in names(stores)){
  df <- stores[[name]]
  results[[name]] <- full_system(name, df, method="arima", objective="O1")
  
  print(paste("Store completed:", name))
  print(results[[name]]$score)
  
  print(plot_forecast(df, results[[name]]$preds, name))
  print(plot_plan(results[[name]]$plan, name))
}

############################################################
### results object now contains:
### results$baltimore
### results$lancaster
### results$philadelphia
### results$richmond
############################################################