library(forecast)
library(ggplot2)

ba <- read.table("baltimore.csv", header = TRUE, sep = ",")
ba$TouristEvent <- ifelse(ba$TouristEvent == "Yes", 1, 0)
ba <- na.omit(ba)

N <- nrow(ba)
TRAINING <- ba[1:floor(N * 0.8), ]
TEST <- ba[(floor(N * 0.8) + 1):N, ]

TRAININGTS <- ts(TRAINING$Num_Customers, frequency = 7)

XREG_TRAIN <- as.matrix(TRAINING[, c("Num_Employees", "Pct_On_Sale", "TouristEvent")])
XREG_TEST <- as.matrix(TEST[, c("Num_Employees", "Pct_On_Sale", "TouristEvent")])

model <- auto.arima(TRAININGTS,
  seasonal      = TRUE,
  stepwise      = FALSE,
  approximation = FALSE,
  xreg          = XREG_TRAIN
)

# Rolling loop
PREDICTVALUES <- numeric(nrow(TEST))
for (i in 1:nrow(TEST)) {
  train_i <- ts(c(TRAINING$Num_Customers, TEST$Num_Customers[seq_len(i - 1)]), frequency = 7)
  xreg_i <- rbind(XREG_TRAIN, XREG_TEST[seq_len(i - 1), , drop = FALSE])
  fit_i <- Arima(train_i, model = model, xreg = xreg_i)
  PREDICTVALUES[i] <- forecast(fit_i, h = 1, xreg = XREG_TEST[i, , drop = FALSE])$mean
}

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
    title = "ARIMAX",
    x = "Data", y = "Clientes",
    color = "Legenda"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Real" = "black", "Previsto" = "red"))

print(p)
