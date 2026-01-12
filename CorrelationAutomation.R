# Install and load required packages (update them daily)
install.packages(c("quantmod", "urca", "tseries"))

# Install and load required packages (update them daily)

library(quantmod)
library(urca)
library(tseries)

# User inputs
stock_list <- c("NVDA", "AAPL", "GOOG", "MSFT", "AMZN", "AVGO", "META", "TSM", "TSLA",
                "TCEHY", "ORCL", "NFLX", "ASML", "PLTR", "IBM", "SAP", "AMD", "CSCO",
                "TXN", "KLAC",
                "LLY", "JNJ", "ABBV", "UNH", "AZN", "NVS", "MRK", "ISRG", "ABT",
                "TMO", "PFE", "BMY", "GILD", "ZBH", "MDT", "DHR", "BAX", "CI", "CVS", "HCA")
start_date <- Sys.Date() - 90
end_date <- Sys.Date()

# Generate all unique pairs from stock_list
all_pairs <- combn(stock_list, 2, simplify = FALSE)

results <- list()

# Compute spread, stationarity, and generate trading signal for a pair
pairs_trading_signal <- function(ticker1, ticker2, start_date, end_date) {
  # Download prices
  stock1 <- getSymbols(ticker1, from = start_date, to = end_date, auto.assign = FALSE)
  stock2 <- getSymbols(ticker2, from = start_date, to = end_date, auto.assign = FALSE)
  
  price1 <- Cl(stock1)
  price2 <- Cl(stock2)
  
  prices <- na.omit(merge(price1, price2))
  colnames(prices) <- c("stock1", "stock2")
  
  # Hedge ratio
  fit <- lm(stock1 ~ stock2, data = prices)
  beta <- coef(fit)[2]
  
  spread <- prices$stock1 - beta * prices$stock2
  
  # Engle-Granger cointegration test (Phillips-Ouliaris)
  eg_test <- ca.po(as.matrix(prices), type = "Pz")
  eg_summary <- summary(eg_test)
  
  # ADF test on spread
  adf_test <- adf.test(spread)
  
  # Calculate mean, sd, latest spread, and z-score
  spread_mean <- mean(spread)
  spread_sd <- sd(spread)
  latest_spread <- tail(spread, 1)
  z_score <- (as.numeric(latest_spread) - spread_mean) / spread_sd
  
  # Return useful info
  list(
    pair = paste(ticker1, ticker2, sep = "-"),
    beta = beta,
    eg_stat = eg_summary@teststat,
    eg_crit_values = eg_summary@cval,
    adf_p_value = adf_test$p.value,
    z_score = z_score,
    spread_mean = spread_mean,
    spread_sd = spread_sd,
    latest_spread = as.numeric(latest_spread)
  )
} 

cat("Starting pairs analysis...\n")
for (pair in all_pairs) {
  ticker1 <- pair[1]
  ticker2 <- pair[2]
  
  cat("Analyzing pair:", ticker1, "-", ticker2, "\n")
  result <- tryCatch({
    pairs_trading_signal(ticker1, ticker2, start_date, end_date)
  }, error = function(e) {
    cat("Error with pair", ticker1, "-", ticker2, ":", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(result)) {
    results[[result$pair]] <- result
  }
}

# Convert list of results to a data.frame for easy filtering/sorting
results_df <- do.call(rbind, lapply(results, as.data.frame))

# Filter pairs with ADF p-value < 0.05 (spread likely stationary)
filtered_df <- subset(results_df, adf_p_value < 0.05)

# Sort by absolute z-score descending (farthest from mean)
filtered_df <- filtered_df[order(-abs(filtered_df$z_score)), ]

# Print top 10 pairs farthest from mean spread
cat("\nTop 10 pairs by spread deviation:\n")
print(head(filtered_df, 10))

cat("\n--- Trading Recommendations ---\n")

# Stocks to LONG (buy first stock, sell second stock)
long_pairs <- subset(filtered_df, z_score < -1.5)
if (nrow(long_pairs) > 0) {
  cat("Pairs to LONG (buy first, sell second):\n")
  for (i in 1:nrow(long_pairs)) {
    parts <- strsplit(long_pairs$pair[i], "-")[[1]]
    cat(sprintf("  %s: BUY %s, SELL %s (z-score = %.2f)\n",
                long_pairs$pair[i], parts[1], parts[2], long_pairs$z_score[i]))
  }
} else {
  cat("No pairs to LONG at this time.\n")
}

# Stocks to SHORT (sell first stock, buy second stock)
short_pairs <- subset(filtered_df, z_score > 1.5)
if (nrow(short_pairs) > 0) {
  cat("\nPairs to SHORT (sell first, buy second):\n")
  for (i in 1:nrow(short_pairs)) {
    parts <- strsplit(short_pairs$pair[i], "-")[[1]]
    cat(sprintf("  %s: SELL %s, BUY %s (z-score = %.2f)\n",
                short_pairs$pair[i], parts[1], parts[2], short_pairs$z_score[i]))
  }
} else {
  cat("No pairs to SHORT at this time.\n")
}

# EXIT SIGNALS: For positions outside ±1.5 SD, show the 1.4 SD exit level
cat("\n--- EXIT SIGNALS ---\n")

# Exit signals for LONG positions (z-score < -1.5)
if (nrow(long_pairs) > 0) {
  cat("EXIT levels for LONG positions (close when spread reaches these levels):\n")
  for (i in 1:nrow(long_pairs)) {
    parts <- strsplit(long_pairs$pair[i], "-")[[1]]
    # Calculate the spread mean and SD from the results
    spread_mean <- long_pairs$spread_mean[i]
    spread_sd <- long_pairs$spread_sd[i]
    latest_spread <- long_pairs$latest_spread[i]
    # Exit target is mean -  0.75*SD (coming up from below)
    exit_target <- spread_mean - 0.75*spread_sd
    exit_z_score <- -0.75
    # Calculate distance to exit
    spread_distance <- exit_target - latest_spread
    cat(sprintf("  %s: EXIT when spread >= %.2f\n", long_pairs$pair[i], exit_target))
    cat(sprintf("    Current spread: %.2f, Exit spread: %.2f, Distance to exit: %.2f\n",
                latest_spread, exit_target, spread_distance))
    cat(sprintf("    Mean: %.2f, SD: %.2f, Current z-score: %.2f, Exit z-score: %.2f\n",
                spread_mean, spread_sd, long_pairs$z_score[i], exit_z_score))
  }
} else {
  cat("No LONG positions requiring exit monitoring.\n")
}

# Exit signals for SHORT positions (z-score > 1.5)
if (nrow(short_pairs) > 0) {
  cat("\nEXIT levels for SHORT positions (close when spread reaches these levels):\n")
  for (i in 1:nrow(short_pairs)) {
    parts <- strsplit(short_pairs$pair[i], "-")[[1]]
    # Calculate the spread mean and SD from the results
    spread_mean <- short_pairs$spread_mean[i]
    spread_sd <- short_pairs$spread_sd[i]
    latest_spread <- short_pairs$latest_spread[i]
    # Exit target is mean + 0.75 * SD (coming down from above)
    exit_target <- spread_mean + 0.75*spread_sd
    exit_z_score <- 0.75
    # Calculate distance to exit
    spread_distance <- latest_spread - exit_target
    cat(sprintf("  %s: EXIT when spread <= %.2f\n", short_pairs$pair[i], exit_target))
    cat(sprintf("    Current spread: %.2f, Exit spread: %.2f, Distance to exit: %.2f\n",
                latest_spread, exit_target, spread_distance))
    cat(sprintf("    Mean: %.2f, SD: %.2f, Current z-score: %.2f, Exit z-score: %.2f\n",
                spread_mean, spread_sd, short_pairs$z_score[i], exit_z_score))
  }
} else {
  cat("No SHORT positions requiring exit monitoring.\n")
}
