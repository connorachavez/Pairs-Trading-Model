# Pairs-Trading-Model
An R script for automated pairs trading analysis. Uses 'quantmod' and 'urca' to download stock data, test for cointegration (ADF test), and generate long/short signals based on spread z-scores.

# What This Project Does
This R script automates the analysis for a pairs trading strategy. It takes a list of stock tickers, systematically creates every possible unique pair, and then performs the following steps for each:

Downloads Data: Fetches the last 90 days of closing prices for both stocks in the pair.

Calculates Spread: Determines the optimal hedge ratio (beta) and calculates the historical "spread" between the two stocks.

Tests for Cointegration: Runs statistical tests (Augmented Dickey-Fuller and Engle-Granger) to determine if the spread is stationary. A stationary spread reliably reverts to its average, which is the key requirement for this strategy.

Generates Signals: For pairs with stationary spreads, it calculates the current z-score (how many standard deviations the spread is from its average) and generates specific trade recommendations:

# Why This Project Is Useful
This project provides a complete, data-driven tool for traders and analysts looking to implement a market-neutral strategy.

Automation: It automates the tedious process of downloading data, testing, and screening hundreds of potential stock pairs, saving significant time.

Statistical Rigor: It moves beyond simple correlation, using formal cointegration tests (ADF) to find statistically significant, mean-reverting relationships.

Actionable Signals: The script doesn't just provide analysis; it outputs clear, actionable "long" and "short" signals based on predefined z-score thresholds.

Market Neutrality: Pairs trading is a popular strategy because it aims to be "market-neutral." By being simultaneously long one stock and short another, the goal is to profit from the relative performance of the pair, regardless of whether the overall market goes up or down. This script is the engine for finding those opportunities.

Scalability: The framework is built to easily expand. You can add dozens of tickers to the stock_list, and the script will analyze every single possible combination.
