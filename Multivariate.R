library(vars)
library(rminer)
library(forecast)

source("multi-utils.R")

# ── Plot helper ───────────────────────────────────────────────────────────────
fshow2 <- function(Y, Pred1, Pred2, method, name1, name2) {
  par(mfrow = c(1, 2))

  metrics <- c("MAE", "RMSE", "R2")

  values <- round(mmetric(Y[, 1], Pred1, metric = metrics), 3)
  metric_str <- paste(names(values), values, sep = "=", collapse = ", ")
  main <- paste(method, " ", name1, " (", metric_str, ")", sep = "")
  mgraph(Y[, 1], Pred1,
    main = main, graph = "REG", Grid = 10,
    col = c("black", "blue"),
    leg = list(pos = "topleft", leg = c("target", method))
  )

  values <- round(mmetric(Y[, 2], Pred2, metric = metrics), 3)
  metric_str <- paste(names(values), values, sep = "=", collapse = ", ")
  main <- paste(method, " ", name2, " (", metric_str, ")", sep = "")
  mgraph(Y[, 2], Pred2,
    main = main, graph = "REG", Grid = 10,
    col = c("black", "blue"),
    leg = list(pos = "topleft", leg = c("target", method))
  )
}

# ── Pareto front helper ───────────────────────────────────────────────────────
pareto_front <- function(df) {
  df <- df[order(df$complexity, df$mae), ]
  pareto <- df[1, ]
  current_best_mae <- df$mae[1]
  for (i in 2:nrow(df)) {
    if (df$mae[i] < current_best_mae) {
      pareto <- rbind(pareto, df[i, ])
      current_best_mae <- df$mae[i]
    }
  }
  return(pareto)
}

# ── Data loading ──────────────────────────────────────────────────────────────
ba <- read.table("baltimore.csv", header = TRUE, sep = ",")
ba$TouristEvent <- ifelse(ba$TouristEvent == "Yes", 1, 0)
ba <- na.omit(ba)
summary(ba)

K <- 7
LTS <- K
ne <- ba$Num_Employees
nc <- ba$Num_Customers
pct <- ba$Pct_On_Sale
even <- ba$TouristEvent
sa <- ba$Sales

hd <- holdout(nc, ratio = LTS, mode = "order")

# Endogenous variables
cdata <- cbind(nc, sa)
mtr2 <- ts(cdata[hd$tr, ], frequency = K)
Y <- cdata[hd$ts, ]

# Exogenous variables
exogen <- cbind(ne, pct)
exotr <- exogen[hd$tr, ]
exots <- exogen[hd$ts, ]

# Sanity check
sum(is.na(exogen))
sum(is.na(cdata))

# ── Pareto analysis setup ─────────────────────────────────────────────────────

# VINP configurations to compare (add or remove as needed)
vinp_configs <- list(
  list(name = "short", lags = list(list(1:2, 1), list(1, 1:2))),
  list(name = "medium", lags = list(list(1:4, 1), list(1, 1:4))),
  list(name = "long", lags = list(list(1:6, 1), list(1, 1:6)))
)

# Models to compare (add or remove as needed)
model_types <- c("lm", "mlpe", "randomforest")

# Model-intrinsic complexity weights
model_complexity <- list(
  "lm"           = 1, # linear, minimal intrinsic complexity
  "mlpe"         = 2, # neural net, moderate
  "randomforest" = 3 # ensemble, highest
)

# ── Main loop ─────────────────────────────────────────────────────────────────
results <- data.frame(
  model = character(),
  vinp_name = character(),
  mae = numeric(),
  complexity = numeric()
)

for (model_type in model_types) {
  for (vc in vinp_configs) {
    VINP <- vc$lags

    tryCatch(
      {
        MNN <- mfit(mtr2, model_type, VINP, exogen = exotr)
        Pred3 <- lforecastm(MNN, h = LTS, exogen = exots)

        mae1 <- mmetric(Y[, 1], Pred3[[1]], metric = "MAE")
        mae2 <- mmetric(Y[, 2], Pred3[[2]], metric = "MAE")
        avg_mae <- mean(c(mae1, mae2))

        lag_complexity <- length(unlist(VINP[[1]])) + length(unlist(VINP[[2]]))
        total_complexity <- lag_complexity * model_complexity[[model_type]]

        results <- rbind(results, data.frame(
          model      = model_type,
          vinp_name  = vc$name,
          mae        = avg_mae,
          complexity = total_complexity
        ))

        # Also plot regression results for each combination
        fshow2(
          Y, Pred3[[1]], Pred3[[2]],
          paste(model_type, vc$name, sep = "-"), "nc", "sa"
        )
      },
      error = function(e) {
        message("Skipped: ", model_type, " + ", vc$name, " — ", e$message)
      }
    )
  }
}

# ── Pareto plot ───────────────────────────────────────────────────────────────
model_shapes <- c("lm" = 15, "mlpe" = 17, "randomforest" = 19)
model_colors <- c("lm" = "steelblue", "mlpe" = "darkorange", "randomforest" = "forestgreen")

pf <- pareto_front(results)
pf_sorted <- pf[order(pf$complexity), ]

plot(
  jitter(results$complexity), # jitter avoids overlapping points
  results$mae,
  pch  = model_shapes[results$model],
  col  = model_colors[results$model],
  xlab = "Complexity (lags × model weight)",
  ylab = "Average MAE",
  main = "Pareto Front: Accuracy vs Complexity"
)

text(jitter(results$complexity), results$mae,
  labels = results$vinp_name, pos = 3, cex = 0.7
)

pareto_color <- adjustcolor("red", alpha.f = 0.4)

lines(pf_sorted$complexity, pf_sorted$mae, col = pareto_color, lwd = 2)
points(pf_sorted$complexity, pf_sorted$mae, pch = 18, col = pareto_color, cex = 1.5)

legend("topright",
  legend = c(names(model_shapes), "Pareto front"),
  col    = c(model_colors, "red"),
  pch    = c(model_shapes, 18)
)
