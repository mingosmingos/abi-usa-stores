library(lmtest)
library(vars)
source("multi-utils.R")

# --- Data preparation (from attached file) ---
files <- list.files(pattern = "*.csv")

df_list <- lapply(files, function(f) {
  prefix <- tools::file_path_sans_ext(basename(f))
  df <- read.csv(f, stringsAsFactors = FALSE)
  df$Date <- as.Date(df$Date)
  names(df)[names(df) != "Date"] <- paste0(prefix, "_", names(df)[names(df) != "Date"])
  df
})

general <- Reduce(function(a, b) merge(a, b, by = "Date", all = FALSE), df_list)

stores <- tools::file_path_sans_ext(basename(files))
customer_cols <- paste0(stores, "_Num_Customers")
ENDO <- ts(general[, customer_cols], frequency = 7)

model <- autoVAR(ENDO, lag.max = 14, type = "const")
best_lag <- model$p

pvalue_matrix <- matrix(NA,
  nrow = length(stores), ncol = length(stores),
  dimnames = list(
    paste("Cause:", stores),
    paste("Effect:", stores)
  )
)

for (cause in stores) {
  for (effect in setdiff(stores, cause)) {
    res <- tryCatch(
      grangertest(general[[paste0(effect, "_Num_Customers")]],
        general[[paste0(cause, "_Num_Customers")]],
        order = best_lag
      ),
      error = function(e) NULL
    )
    if (!is.null(res)) {
      pvalue_matrix[paste("Cause:", cause), paste("Effect:", effect)] <- round(res$`Pr(>F)`[2], 4)
    }
  }
}

print(pvalue_matrix)
