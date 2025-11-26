# =========================================================
# 5_backtestStrategy.R (CONFIDENCE WEIGHTED)
# =========================================================
# PURPOSE:
# 1. Implement Relative Confidence Sizing
#    Weight_i = Hit_Ratio_i / Sum(Hit_Ratios_Alive)
# 2. Daily Trading Execution with 1-Day Lag
# 3. Full Performance Reporting
# =========================================================

# ---- Packages ----
library(tidyverse)
library(tidyquant)
library(timetk)
library(PerformanceAnalytics)
library(xts)

# ---- Load Data ----
# Ensure these are loaded
# final_signals <- readRDS("final_signals.rds")
# assets_long   <- readRDS("assets_long.rds")

# ---- Parameters ----
TX_COST <- 0.0010  # 10 bps

# =========================================================
# 1. SIGNAL PROCESSING & WEIGHTING
# =========================================================

# Step A: Daily Skeleton & Signal Join
daily_skeleton <- assets_long %>%
  filter(asset %in% unique(final_signals$asset)) %>%
  arrange(asset, date) %>%
  left_join(final_signals %>% select(date, asset, signal, hit_ratio), 
            by = c("date", "asset")) %>%
  group_by(asset) %>%
  fill(signal, hit_ratio, .direction = "down") %>%
  ungroup()

# Step B: Cross-Sectional Sizing (The New Logic)
# We calculate weights day-by-day based on the active signals.
daily_weights <- daily_skeleton %>%
  group_by(date) %>%
  mutate(
    # 1. Filter: Only 'Alive' assets contribute to the weight calculation
    raw_score = if_else(signal == 1, hit_ratio, 0),
    
    # 2. Aggregation: What is the total conviction today?
    total_score = sum(raw_score, na.rm = TRUE),
    
    # 3. Allocation: Pro-rata allocation
    #    If total_score is 0 (All Dead), weight is 0 (Cash)
    target_weight = if_else(total_score > 0, raw_score / total_score, 0)
  ) %>%
  ungroup() %>%
  
  # Step C: Trading Execution (Lag & Cost)
  group_by(asset) %>%
  mutate(
    # Trade Tomorrow based on Today's close signal
    trade_weight = lag(target_weight, 1),
    trade_weight = replace_na(trade_weight, 0),
    
    # Returns & Costs
    asset_ret    = price / lag(price) - 1,
    weight_delta = abs(trade_weight - lag(trade_weight, default = 0)),
    cost_drag    = weight_delta * TX_COST,
    strat_ret    = (trade_weight * asset_ret) - cost_drag
  ) %>%
  drop_na(asset_ret) %>%
  ungroup()

# =========================================================
# 2. PORTFOLIO AGGREGATION
# =========================================================

# Since we already weighted the assets effectively (sum of weights = 1.0),
# the Portfolio Return is just the sum of weighted returns for each day.
portfolio_daily <- daily_weights %>%
  group_by(date) %>%
  summarise(
    portfolio_ret = sum(strat_ret, na.rm = TRUE)
  )

# =========================================================
# 3. DATE ALIGNMENT & BENCHMARK
# =========================================================

# Find start date (Post-Warmup)
first_trade_date <- daily_weights %>%
  filter(trade_weight > 0) %>%
  summarise(min_date = min(date)) %>%
  pull(min_date)

print(paste("Strategy Start Date:", first_trade_date))

# Filter Portfolio
portfolio_daily <- portfolio_daily %>%
  filter(date >= first_trade_date)

# Prepare Benchmark (EUNM or CSEMAS)
benchmark_asset <- "EUNM" # Change if needed
benchmark_daily <- assets_long %>%
  filter(asset == benchmark_asset) %>%
  arrange(date) %>%
  mutate(bench_ret = price / lag(price) - 1) %>%
  select(date, bench_ret) %>%
  drop_na() %>%
  filter(date >= first_trade_date)

# =========================================================
# 4. REPORTING
# =========================================================

comparison_data <- portfolio_daily %>%
  left_join(benchmark_daily, by = "date") %>%
  drop_na()

xts_data <- xts(
  x = comparison_data %>% select(portfolio_ret, bench_ret),
  order.by = comparison_data$date
)
colnames(xts_data) <- c("Schrödinger_Confidence", "Benchmark")

# A. Stats
print("--- Performance Stats ---")
print(table.AnnualizedReturns(xts_data, scale = 252))

# B. Chart
charts.PerformanceSummary(
  xts_data, 
  main = "Schrödinger (Confidence Weighted) vs Benchmark",
  colorset = c("darkblue", "gray"),
  lwd = 2
)

# C. Check Allocations (Sanity Check)
# Let's see an example day to ensure weights sum to ~1
print("--- Allocation Example (Recent Date) ---")
example_date <- max(daily_weights$date) - 5
print(daily_weights %>% 
        filter(date == example_date, trade_weight > 0) %>% 
        select(date, asset, hit_ratio, trade_weight))

print(paste("Total Weight on", example_date, ":", 
            daily_weights %>% filter(date == example_date) %>% summarise(sum(trade_weight)) %>% pull()))



# =========================================================
# Sanity Check: Crisis Inspection
# =========================================================

# 1. COVID CRASH (March 2020)
# Did we reduce exposure?
print("--- Allocation during COVID (March 2020) ---")
print(daily_weights %>%
  filter(date >= "2020-03-01", date <= "2020-03-31") %>%
  group_by(date) %>%
  summarise(Total_Invested = sum(trade_weight)) %>%
  slice(c(1, 10, 20)) # Show beginning, middle, end of month
)

# 2. INFLATION BEAR MARKET (Oct 2022)
# Did the Macro Z-Scores (High Inflation/Rates) force us out?
print("--- Allocation during Peak Inflation (Oct 2022) ---")
print(daily_weights %>%
  filter(date >= "2022-10-01", date <= "2022-10-31") %>%
  group_by(date) %>%
  summarise(Total_Invested = sum(trade_weight)) %>%
  slice(c(1, 10, 20))
)




# =========================================================
# Sanity Check: Portfolio Composition During Crises
# =========================================================

# Helper to show top holdings on specific dates
show_holdings <- function(target_date) {
  holdings <- daily_weights %>%
    filter(date == as.Date(target_date)) %>%
    filter(trade_weight > 0) %>%
    select(date, asset, hit_ratio, trade_weight) %>%
    arrange(desc(trade_weight))
  
  print(paste("--- Holdings on", target_date, "---"))
  print(holdings)
}

# 1. COVID CRASH (March 2020)
# Did we pivot to Bonds?
show_holdings("2020-03-02") # Pre-Crash
show_holdings("2020-03-23") # Bottom of Crash

# 2. INFLATION SHOCK (Oct 2022)
# Did we pivot to Bonds (which also fell) or something else?
show_holdings("2022-10-03")
show_holdings("2022-10-31")
