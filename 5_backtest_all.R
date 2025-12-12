# =========================================================
# SCRIPT 5: BACKTEST & REPORTING (OPTION C - BREADTH)
# =========================================================
# Strategy: Equal Weight all Positive Signals
# Logic: 
#   - Invest in ALL assets with Expected Return > 0.
#   - Weight = 1 / N (Subject to constraints).
#   - Dynamic Start Date & Robust Reporting applied.
# Constraints: 
#   1. Max Single Asset: 20%
#   2. Max Bond Allocation: 40%
#   3. No Shorts, No Leverage.

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
# Load objects (Ensure these exist in your environment)
# signals_long <- readRDS("signals_long.rds")
# assets_wide  <- readRDS("assets_wide.rds") 

BENCHMARK_TICKER <- "MIMUEMRN" # iShares MSCI EM

# Constraints
MAX_SINGLE_ASSET   <- 0.20
MAX_BOND_TOTAL     <- 0.40
MAX_TOTAL_EXPOSURE <- 1.00 

# Asset Class Definitions
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
  distinct(date, .keep_all = TRUE) 

# Create XTS
daily_prices_xts <- daily_prices %>%
  select(-date) %>%
  xts(order.by = daily_prices$date)

# Calculate Returns
daily_returns_xts <- Return.calculate(daily_prices_xts)

# --- ROBUST BENCHMARK EXTRACTION ---
# Treat benchmark separately to bridge gaps/holidays correctly.
bench_prices_clean <- na.omit(daily_prices_xts[, BENCHMARK_TICKER])
bench_ret <- Return.calculate(bench_prices_clean)
colnames(bench_ret) <- "Benchmark"

# =========================================================
# 3. WEIGHT DETERMINATION (OPTION C LOGIC)
# =========================================================
rebalance_dates <- unique(signals_long$date)
weights_history <- list()

cat("Calculating Option C Weights (Breadth) for", length(rebalance_dates), "periods...\n")

for (i in seq_along(rebalance_dates)) {
  curr_date <- rebalance_dates[i]
  
  # 1. Filter Candidates
  # We select ALL positive signals.
  candidates <- signals_long %>%
    filter(date == curr_date) %>%
    mutate(asset_class = get_asset_class(asset)) %>%
    # SAFETY: Treat NA/NaN as 0 (No Signal)
    mutate(expected_return = ifelse(is.na(expected_return) | is.nan(expected_return), 0, expected_return)) %>%
    filter(expected_return > 0)
  
  # Initialize zero weights
  universe_assets <- colnames(daily_prices_xts)
  w_vec <- setNames(rep(0, length(universe_assets)), universe_assets)
  
  if (nrow(candidates) > 0) {
    
    # 2. Base Weight Calculation (Equal Weight)
    n_candidates <- nrow(candidates)
    base_weight  <- 1 / n_candidates
    
    # 3. Apply Single Asset Cap (Max 20%)
    # If we have 2 assets, 1/N = 50%, capped at 20%.
    final_single_weight <- min(base_weight, MAX_SINGLE_ASSET)
    
    candidate_weights <- rep(final_single_weight, n_candidates)
    names(candidate_weights) <- candidates$asset
    
    # 4. Apply Bond Cap (Max 40% Total)
    bond_subset <- candidates %>% filter(asset_class == "Bond")
    
    if (nrow(bond_subset) > 0) {
      current_bond_total <- sum(candidate_weights[bond_subset$asset])
      
      if (current_bond_total > MAX_BOND_TOTAL) {
        # Scale down bonds proportionally
        scale_factor <- MAX_BOND_TOTAL / current_bond_total
        candidate_weights[bond_subset$asset] <- candidate_weights[bond_subset$asset] * scale_factor
      }
    }
    
    # 5. Assign to Main Vector
    w_vec[names(candidate_weights)] <- candidate_weights
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

cat("Running Daily Simulation...\n")

for (d in 1:length(all_dates)) {
  today <- all_dates[d]
  
  valid_rebal_dates <- rebalance_dates[rebalance_dates < today]
  
  if (length(valid_rebal_dates) > 0) {
    latest_rebal <- max(valid_rebal_dates)
    
    if (as.numeric(latest_rebal) != last_rebal_idx) {
      target_w <- weights_history[[as.character(latest_rebal)]]
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
colnames(strat_xts) <- "Regime_All"

# =========================================================
# 5. REPORTING: DYNAMIC START DATE (THE FIX)
# =========================================================

# 1. Merge Strategy & Benchmark (Outer Join)
compare_xts_full <- merge(strat_xts, bench_ret, join = "outer")
compare_xts_full[is.na(compare_xts_full)] <- 0

# 2. FIND ACTUAL START DATE (Skip Warm-up)
non_zero_idx <- which(compare_xts_full[, "Regime_All"] != 0)

if (length(non_zero_idx) > 0) {
  start_index_num   <- non_zero_idx[1]
  actual_start_date <- index(compare_xts_full)[start_index_num]
} else {
  actual_start_date <- first(index(compare_xts_full))
  warning("Strategy is flat (0%) for the entire history!")
}

cat(paste("Strategy Warm-up Complete. Actual Trading Starts:", actual_start_date, "\n"))

# 3. TRIM DATA to this Start Date
compare_xts <- compare_xts_full[paste0(actual_start_date, "/")]

# 4. Standard Chart (Linear)
charts.PerformanceSummary(compare_xts, 
                          main = "Regime All-Assets vs Benchmark",
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
  scale_color_manual(values = c("Regime_All" = "#003366", "Benchmark" = "#999999")) +
  labs(title = "Regime All-Assets vs Benchmark (Log Scale)",
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
                 main = "Portfolio Allocation (Option C - Breadth)",
                 colorset = c("darkgreen", "orange", "lightgray"),
                 ylab = "Allocation", 
                 ylim = c(0, 1.0),
                 border=NA)

cat("\n--- LATEST ALLOCATION ---\n")
latest_w <- coredata(weight_matrix_sync)[nrow(weight_matrix_sync), ]
print(round(latest_w[latest_w > 0.001], 3))
