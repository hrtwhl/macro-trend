# =========================================================
# SCRIPT 5: BACKTEST & REPORTING (OPTION A - TOP 10)
# =========================================================
# Strategy: "Top 10" Greedy Allocation (Ranked)
# Logic: 
#   - Rank assets by Expected Return.
#   - Fill portfolio with top assets (10% each).
#   - Stop when portfolio is full (100%) or constraints hit.
# Constraints: 
#   1. Max Single Asset: 10% (Target) -> Satisfies <20% limit
#   2. Max Bond Allocation: 40%
#   3. No Shorts, No Leverage.

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(stringr)

# =========================================================
# 1. CONFIGURATION & DATA LOADING
# =========================================================
# Load objects (Ensure these exist in your environment)
# signals_long <- readRDS("signals_long.rds")
# assets_wide  <- readRDS("assets_wide.rds") 

BENCHMARK_TICKER <- "MIMUEMRN" # iShares MSCI EM

# --- PORTFOLIO CONSTRAINTS ---
# We set Single Asset to 0.10 to create a "Top 10" portfolio.
MAX_SINGLE_ASSET   <- 0.10 
MAX_BOND_ALLOC     <- 0.40
MAX_EQUITY_ALLOC   <- 1.00
MAX_TOTAL_EXPOSURE <- 1.00

# Helper: Map Tickers to Class
get_asset_class <- function(tickers) {
  bond_tickers <- c("JPEICORE", "GBIE1001", "BCEX6T", "BCEX4T", "IDCOT7", 
                    "BEIG1T", "LECPTREU", "IBOXXMJA")
  ifelse(tickers %in% bond_tickers, "Bond", "Equity")
}

# =========================================================
# 2. PREPARE DATA
# =========================================================
daily_prices <- assets_wide %>% 
  arrange(date) %>%
  select(date, everything()) 

# Create XTS
daily_prices_xts <- daily_prices %>%
  select(-date) %>%
  xts(order.by = daily_prices$date)

# Calculate Returns
daily_returns_xts <- Return.calculate(daily_prices_xts)

# Prepare Benchmark
bench_ret <- daily_returns_xts[, BENCHMARK_TICKER]
bench_ret <- na.omit(bench_ret) 
colnames(bench_ret) <- "Benchmark"

# =========================================================
# 3. WEIGHT DETERMINATION (GREEDY TOP 10)
# =========================================================
rebalance_dates <- unique(signals_long$date)
weights_history <- list()

cat("Calculating 'Top 10' Weights for", length(rebalance_dates), "periods...\n")

for (i in seq_along(rebalance_dates)) {
  curr_date <- rebalance_dates[i]
  
  # 1. Get & Rank Signals
  # We select ALL positive signals and sort them. 
  # The loop below will naturally stop after filling 10 slots (10 * 10% = 100%).
  sig_subset <- signals_long %>%
    filter(date == curr_date) %>%
    mutate(asset_class = get_asset_class(asset)) %>%
    filter(expected_return > 0) %>% # STRICT RULE: Never buy negative expected return
    arrange(desc(expected_return))  # Priority: Best returns first
  
  # 2. Initialize Counters
  # We use the full asset list to ensure the vector matches the daily data columns
  universe_assets <- colnames(daily_prices_xts)
  w_vec <- setNames(rep(0, length(universe_assets)), universe_assets)
  
  current_equity_exp <- 0
  current_bond_exp   <- 0
  total_exp          <- 0
  
  # 3. Allocation Loop (Greedy Waterfall)
  if (nrow(sig_subset) > 0) {
    for (j in 1:nrow(sig_subset)) {
      
      # Stop if portfolio is full (100%)
      # This effectively limits us to 10 assets (since 10 * 0.10 = 1.0)
      if (total_exp >= MAX_TOTAL_EXPOSURE - 0.001) break
      
      asset_name <- sig_subset$asset[j]
      aclass     <- sig_subset$asset_class[j]
      
      # Calculate Remaining Room
      # 1. Room in Total Portfolio
      room_total <- MAX_TOTAL_EXPOSURE - total_exp
      
      # 2. Room in Asset Class
      room_class <- if (aclass == "Bond") {
        MAX_BOND_ALLOC - current_bond_exp
      } else {
        MAX_EQUITY_ALLOC - current_equity_exp
      }
      
      # 3. Allocation Amount
      # We attempt to give 10% (MAX_SINGLE_ASSET).
      # If constraints (e.g. Bond Cap) prevent 10%, we give whatever room is left.
      # If room_class is 0 (Bond cap hit), allocation becomes 0.
      allocation <- min(MAX_SINGLE_ASSET, room_class, room_total)
      
      # Assign Weight if > 0
      if (allocation > 0.001) {
        w_vec[asset_name] <- allocation
        total_exp <- total_exp + allocation
        
        if (aclass == "Bond") {
          current_bond_exp <- current_bond_exp + allocation
        } else {
          current_equity_exp <- current_equity_exp + allocation
        }
      }
    }
  }
  
  weights_history[[as.character(curr_date)]] <- w_vec
}

# =========================================================
# 4. DAILY BACKTEST ENGINE
# =========================================================
all_dates <- index(daily_returns_xts)
strat_returns <- numeric(length(all_dates))
weight_matrix <- matrix(0, nrow = length(all_dates), ncol = ncol(daily_returns_xts))
colnames(weight_matrix) <- colnames(daily_returns_xts)

current_weights <- setNames(rep(0, ncol(daily_returns_xts)), colnames(daily_returns_xts))
last_rebal_idx <- 0

for (d in 1:length(all_dates)) {
  today <- all_dates[d]
  
  # Check for rebalance (Trade T+1)
  valid_rebal_dates <- rebalance_dates[rebalance_dates < today]
  
  if (length(valid_rebal_dates) > 0) {
    latest_rebal <- max(valid_rebal_dates)
    
    if (as.numeric(latest_rebal) != last_rebal_idx) {
      target_w <- weights_history[[as.character(latest_rebal)]]
      
      # Update Weights
      current_weights[] <- 0 
      current_weights[names(target_w)] <- target_w
      last_rebal_idx <- as.numeric(latest_rebal)
    }
  }
  
  weight_matrix[d, ] <- current_weights
  
  # Calc Daily Return
  day_rets <- daily_returns_xts[d, ]
  day_rets[is.na(day_rets)] <- 0
  
  strat_returns[d] <- sum(day_rets * current_weights)
}

strat_xts <- xts(strat_returns, order.by = all_dates)
colnames(strat_xts) <- "Regime_Top10"

# =========================================================
# 5. REPORTING & VISUALIZATION
# =========================================================

# 1. Sync Start Date (First Trade)
first_trade_idx <- min(which(rowSums(weight_matrix) > 0))
start_date <- all_dates[first_trade_idx]

cat(paste("Backtest Start Date:", start_date, "\n"))

# 2. Merge & Trim
compare_xts_raw <- merge(strat_xts, bench_ret, join = "inner")
compare_xts <- compare_xts_raw[paste0(start_date, "/")]

# 3. Charts
charts.PerformanceSummary(compare_xts, 
                          main = "Regime Strategy (Top 10) vs Benchmark",
                          colorset = c("blue", "gray"),
                          lwd = 2)

# 4. Stats
stats <- rbind(
  table.AnnualizedReturns(compare_xts),
  maxDrawdown(compare_xts)
)
cat("\n--- PERFORMANCE SUMMARY (Top 10) ---\n")
print(round(stats, 4))

# =========================================================
# 6. ALLOCATION CHART (FIXED)
# =========================================================
weight_matrix_xts <- xts(weight_matrix, order.by = all_dates)
weight_sync <- weight_matrix_xts[paste0(start_date, "/")]

ac_map <- get_asset_class(colnames(weight_sync))

# Robust Aggregation (Fixed rowSums error)
equity_alloc <- xts(rowSums(weight_sync[, ac_map == "Equity"], na.rm=TRUE), order.by = index(weight_sync))
bond_alloc   <- xts(rowSums(weight_sync[, ac_map == "Bond"],   na.rm=TRUE), order.by = index(weight_sync))
cash_alloc   <- 1 - (equity_alloc + bond_alloc)

# Clean small errors
cash_alloc[cash_alloc < 0] <- 0
cash_alloc[cash_alloc > 1] <- 1

alloc_df <- merge(equity_alloc, bond_alloc, cash_alloc)
colnames(alloc_df) <- c("Equity", "Bond", "Cash")

chart.StackedBar(alloc_df[endpoints(alloc_df, "months")], 
                 main = "Portfolio Allocation (Top 10)",
                 colorset = c("darkgreen", "orange", "lightgray"),
                 ylab = "Allocation", 
                 ylim = c(0, 1.0),
                 border=NA)

cat("\n--- LATEST ALLOCATION ---\n")
latest_w <- coredata(weight_sync)[nrow(weight_sync), ]
print(round(latest_w[latest_w > 0], 3))

