library(forecast)
library(rminer)
library(vars)
library(GA)
library(dplyr)
library(readr)

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


forecast_arima <- function(df, h=7){
  ts_data <- ts(df$Num_Customers, frequency=7)
  fit <- auto.arima(ts_data)
  preds <- forecast(fit, h=h)$mean
  return(as.numeric(preds))
}

forecast_miner <- function(df, h=7){
  df2 <- df %>%
    mutate(TouristEvent = ifelse(TouristEvent=="Yes",1,0))
  
  data_cs <- CasesSeries(df2$Num_Customers, df2[,c("Num_Employees","Pct_On_Sale","TouristEvent","Sales")],
                         window=h+7)
  
  model <- fit(Num_Customers ~ ., data=data_cs, model="svm")
  
  preds <- lforecast(model, data_cs, h=h)
  return(as.numeric(preds))
}


store_cfg <- list(
  baltimore     = list(Fj=1.00, Fx=1.15, W=700),
  lancaster     = list(Fj=1.05, Fx=1.20, W=730),
  philadelphia  = list(Fj=1.10, Fx=1.15, W=760),
  richmond      = list(Fj=1.15, Fx=1.25, W=800)
)


compute_day_profit <- function(store, Cpred, J, X, PR){
  cfg <- store_cfg[[store]]
  Fj <- cfg$Fj; Fx <- cfg$Fx
  
  assisted <- min(7*X + 6*J, Cpred)
  
  profit <- 0
  for(i in 1:assisted){
    F <- ifelse(i <= 7*X, Fx, Fj)
    U <- round(F * 10 / log(2 - PR))
    P <- round(U * (1 - PR) * 1.07)
    profit <- profit + P
  }
  
  hr_cost <- 80*J + 80*X   # custo diário base (sem fim de semana)
  
  return(list(profit=profit - hr_cost, units=assisted))
}


compute_week_profit <- function(store, preds, plan){
  total_profit <- 0
  total_units  <- 0
  
  for(day in 1:7){
    J <- plan$J[day]
    X <- plan$X[day]
    PR <- plan$PR[day]
    
    r <- compute_day_profit(store, preds[day], J, X, PR)
    total_profit <- total_profit + r$profit
    total_units  <- total_units  + r$units
  }
  
  total_profit <- total_profit - store_cfg[[store]]$W
  
  return(list(profit=total_profit, units=total_units))
}


random_plan <- function(){
  data.frame(
    J = sample(0:20, 7, replace=TRUE),
    X = sample(0:20, 7, replace=TRUE),
    PR = runif(7, 0, 0.30)
  )
}


objective_function <- function(vec, store, preds, objective){
  J <- vec[1:7]
  X <- vec[8:14]
  PR <- vec[15:21]
  
  plan <- data.frame(J=round(J), X=round(X), PR=pmax(0,pmin(PR,0.3)))
  
  res <- compute_week_profit(store, preds, plan)
  profit <- res$profit
  units  <- res$units
  
  if(objective=="O2" && units > 10000){
    return(-1e9)   # penalização
  }
  if(objective=="O3"){
    hr_total <- sum(plan$J + plan$X)
    return(profit - 0.1 * hr_total)
  }
  return(profit)
}


optimize_store <- function(store, preds, objective="O1"){
  GA <- ga(
    type="real-valued",
    fitness=function(v) objective_function(v, store, preds, objective),
    lower=c(rep(0,14), rep(0,7)),
    upper=c(rep(20,14), rep(0.30,7)),
    popSize=80,
    maxiter=150,
    run=50
  )
  best <- GA@solution
  result <- objective_function(best, store, preds, objective)
  return(list(GA=GA, best=best, score=result))
}


full_system <- function(store, df, method="arima", objective="O1"){
  
  if(method=="arima"){
    preds <- forecast_arima(df)
  } else {
    preds <- forecast_miner(df)
  }
  
  opt <- optimize_store(store, preds, objective)
  
  list(
    store = store,
    preds = preds,
    best_solution = opt$best,
    score = opt$score
  )
}

# Exemplo:
result_baltimore <- full_system("baltimore", baltimore, method="arima", objective="O1")
print(result_baltimore)


