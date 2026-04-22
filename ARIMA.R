library(forecast)
library(ggplot2)

ba <- read.table("baltimore.csv", header = TRUE, sep = ",")

N <- nrow(ba)
TRAINING <- ba[1:floor(N * 0.8), ]
TEST <- ba[(floor(N * 0.8) + 1):N, ]

TRAININGTS <- ts(TRAINING$Num_Customers, frequency = 7)
model <- auto.arima(TRAININGTS,
                    seasonal = TRUE,
                    stepwise = FALSE,
                    approximation = FALSE
)

print(coef(model))
checkresiduals(model)

# Rolling 1-step-ahead forecast
PREDICTVALUES <- numeric(nrow(TEST))
for (i in 1:nrow(TEST)) {
  train_i <- ts(c(TRAINING$Num_Customers, TEST$Num_Customers[seq_len(i-1)]), frequency = 7)
  fit_i <- Arima(train_i, model = model)
  PREDICTVALUES[i] <- forecast(fit_i, h = 1)$mean
}

REALVALUES <- TEST$Num_Customers

### Plots
df_plot <- data.frame(
  Data     = as.Date(TEST$Date),
  Real     = REALVALUES,
  Previsto = PREDICTVALUES
)

p <- ggplot(df_plot, aes(x = Data)) +
  geom_line(aes(y = Real, color = "Real"), size = 1) +
  geom_line(aes(y = Previsto, color = "Previsto"), size = 1, linetype = "dashed") +
  labs(
    title = "ARIMA",
    x = "Data", y = "Clientes",
    color = "Legenda"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "black", "Previsto" = "red"))

print(p)
