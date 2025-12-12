# =========================================================
# SCRIPT 5: BACKTEST & REPORTING (FINAL FIX)
# =========================================================
# Fixes:
# 1. Starts charts EXACTLY when strategy makes its first trade (skips warm-up).
# 2. Variable name consistency (fixes "object not found" errors).
# 3. Robust Benchmark calculation (prevents disappearing crashes).

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(ggplot2)
library(scales)
library(stringr)

# =========================================================
# 1. CONFIGURATION & DATA LOADING
# =========================================================
# Ensure these objects are loaded in your environment
# signals_long <- readRDS("signals_long.rds")
# assets_long  <- readRDS("assets_long.rds") 
# assets_wide  <- readRDS("assets_wide.rds") 

BENCHMARK_TICKER <- "MIMUEMRN" # iShares MSCI EM or similar

# Portfolio Limits (Strict 100% Cap)
MAX_SINGLE_ASSET   <- 0.20
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
# 2. PREPARE DAILY DATA
# =========================================================
# Sort and ensure unique dates
daily_prices <- assets_wide %>% 
  arrange(date) %>%
  distinct(date, .keep_all = TRUE) 

# Create XTS
daily_prices_xts <- daily_prices %>%
  select(-date) %>%
  xts(order.by = daily_prices$date)

# Calculate Returns (Keep NAs for now, handled in loop)
daily_returns_xts <- Return.calculate(daily_prices_xts)

# --- ROBUST BENCHMARK EXTRACTION ---
# We treat the benchmark separately to ensure we bridge gaps/holidays correctly.
bench_prices_clean <- na.omit(daily_prices_xts[, BENCHMARK_TICKER])
bench_ret <- Return.calculate(bench_prices_clean)
colnames(bench_ret) <- "Benchmark"

# =========================================================
# 3. WEIGHT DETERMINATION
# =========================================================
rebalance_dates <- unique(signals_long$date)
weights_history <- list()

cat("Calculating Weights for", length(rebalance_dates), "periods...\n")

for (i in seq_along(rebalance_dates)) {
  curr_date <- rebalance_dates[i]
  
  # 1. Get & Rank Signals
  sig_subset <- signals_long %>%
    filter(date == curr_date) %>%
    mutate(asset_class = get_asset_class(asset)) %>%
    arrange(desc(expected_return)) 
  
  # 2. Initialize Counters
  w_vec <- rep(0, nrow(sig_subset))
  names(w_vec) <- sig_subset$asset
  
  current_equity_exp <- 0
  current_bond_exp   <- 0
  total_exp          <- 0
  
  # 3. Allocation Loop
  for (j in 1:nrow(sig_subset)) {
    if (total_exp >= MAX_TOTAL_EXPOSURE) break
    
    asset_name <- sig_subset$asset[j]
    aclass     <- sig_subset$asset_class[j]
    exp_ret    <- sig_subset$expected_return[j]
    
    # SAFETY CHECK: Treat NA/NaN as 0 (No Signal)
    if (is.na(exp_ret) || is.nan(exp_ret)) exp_ret <- 0
    
    if (exp_ret <= 0) next 
    
    room_total <- MAX_TOTAL_EXPOSURE - total_exp
    room_class <- if (aclass == "Bond") MAX_BOND_ALLOC - current_bond_exp else MAX_EQUITY_ALLOC - current_equity_exp
    
    allocation <- min(MAX_SINGLE_ASSET, room_class, room_total)
    
    if (allocation > 0.001) {
      w_vec[asset_name] <- allocation
      total_exp <- total_exp + allocation
      if (aclass == "Bond") current_bond_exp <- current_bond_exp + allocation else current_equity_exp <- current_equity_exp + allocation
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

# Helper variables
asset_names <- colnames(daily_returns_xts)
current_weights <- rep(0, length(asset_names))
names(current_weights) <- asset_names
last_rebal_idx <- 0

cat("Running Daily Simulation...\n")

for (d in 1:length(all_dates)) {
  today <- all_dates[d]
  
  valid_rebal_dates <- rebalance_dates[rebalance_dates < today]
  
  if (length(valid_rebal_dates) > 0) {
    latest_rebal <- max(valid_rebal_dates)
    
    if (as.numeric(latest_rebal) != last_rebal_idx) {
      target_w <- weights_history[[as.character(latest_rebal)]]
      current_weights[] <- 0 
      valid_assets <- intersect(names(target_w), names(current_weights))
      current_weights[valid_assets] <- target_w[valid_assets]
      last_rebal_idx <- as.numeric(latest_rebal)
    }
  }
  
  weight_matrix[d, ] <- current_weights
  day_rets <- daily_returns_xts[d, ]
  day_rets[is.na(day_rets)] <- 0 # Treat missing assets as 0 return
  
  strat_returns[d] <- sum(day_rets * current_weights)
}

strat_xts <- xts(strat_returns, order.by = all_dates)
colnames(strat_xts) <- "Regime.Strategy"

# =========================================================
# 5. REPORTING: DYNAMIC START DATE (THE FIX)
# =========================================================

# 1. Merge Strategy & Benchmark (Outer Join to capture all data)
compare_xts_full <- merge(strat_xts, bench_ret, join = "outer")
compare_xts_full[is.na(compare_xts_full)] <- 0

# 2. FIND ACTUAL START DATE (Skip the Flat/Warm-up Period)
# We find the first day the strategy return is NOT zero (either profit or loss).
non_zero_idx <- which(compare_xts_full[, "Regime.Strategy"] != 0)

if (length(non_zero_idx) > 0) {
  # Start exactly at the first trade
  start_index_num   <- non_zero_idx[1]
  actual_start_date <- index(compare_xts_full)[start_index_num]
} else {
  # Fallback if strategy never trades
  actual_start_date <- first(index(compare_xts_full))
  warning("Strategy is flat (0%) for the entire history!")
}

cat(paste("Strategy Warm-up Complete. Actual Trading Starts:", actual_start_date, "\n"))

# 3. TRIM DATA to this Start Date
compare_xts <- compare_xts_full[paste0(actual_start_date, "/")]

# 4. Standard Chart
charts.PerformanceSummary(compare_xts, 
                          main = "Regime Strategy vs Benchmark (Synchronized)",
                          colorset = c("darkblue", "gray"),
                          lwd = 2)

# 5. Log-Scale Chart (ggplot)
plot_data <- data.frame(date = index(compare_xts), coredata(compare_xts)) %>%
  pivot_longer(cols = -date, names_to = "Series", values_to = "Daily_Return") %>%
  arrange(Series, date) %>%
  group_by(Series) %>%
  mutate(Wealth_Index = cumprod(1 + Daily_Return)) %>%
  ungroup()

p_log <- ggplot(plot_data, aes(x = date, y = Wealth_Index, color = Series)) +
  geom_line(linewidth = 0.8) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_color_manual(values = c("Regime.Strategy" = "#003366", "Benchmark" = "#999999")) +
  labs(title = "Regime Strategy vs Benchmark (Log Scale)",
       subtitle = paste("Actual Trading Start:", actual_start_date),
       y = "Wealth Index (Log 10)", x = NULL, color = NULL) +
  theme_minimal() +
  theme(legend.position = "top", plot.title = element_text(face = "bold", size = 12))

print(p_log)

# 6. Calendar Table (Manual Aggregation Fix)
monthly_stats <- apply.monthly(compare_xts, Return.cumulative)

cat("\n--- CALENDAR RETURNS ---\n")
print(table.CalendarReturns(monthly_stats))

cat("\n--- PERFORMANCE SUMMARY ---\n")
print(round(rbind(table.AnnualizedReturns(compare_xts),
                  maxDrawdown(compare_xts)), 4))

# =========================================================
# 6. ALLOCATION CHART (SYNCHRONIZED)
# =========================================================

# 1. Trim Weights to match the SAME Start Date as the Chart
weight_matrix_xts <- xts(weight_matrix, order.by = all_dates)
weight_matrix_sync <- weight_matrix_xts[paste0(actual_start_date, "/")]

# 2. Aggregate
ac_map <- get_asset_class(colnames(weight_matrix_sync))
equity_alloc <- xts(rowSums(weight_matrix_sync[, ac_map == "Equity"], na.rm=TRUE), order.by = index(weight_matrix_sync))
bond_alloc   <- xts(rowSums(weight_matrix_sync[, ac_map == "Bond"],   na.rm=TRUE), order.by = index(weight_matrix_sync))
cash_alloc   <- 1 - (equity_alloc + bond_alloc)

# Clean
cash_alloc[cash_alloc < 0] <- 0 
cash_alloc[cash_alloc > 1] <- 1

# 3. Plot
alloc_df <- merge(equity_alloc, bond_alloc, cash_alloc)
colnames(alloc_df) <- c("Equity", "Bond", "Cash")

chart.StackedBar(alloc_df[endpoints(alloc_df, "months")], 
                 main = "Portfolio Allocation (Synchronized)",
                 colorset = c("darkgreen", "orange", "lightgray"),
                 ylab = "Allocation", 
                 ylim = c(0, 1.0),
                 border=NA)
