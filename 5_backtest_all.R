# =========================================================
# SCRIPT 5: BACKTEST & REPORTING (OPTION C - BREADTH)
# =========================================================
# Strategy: Equal Weight all Positive Signals (Regime Breadth)
# Constraints: 
# 1. Max Single Asset: 20% (at investment time)
# 2. Max Bond Allocation: 40%
# 3. No Shorts, No Leverage (Max 100% Total)

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
# Load objects from previous steps
# signals_long <- readRDS("signals_long.rds")
# assets_wide  <- readRDS("assets_wide.rds") 

# Benchmark (MSCI EM)
BENCHMARK_TICKER <- "MIMUEMRN" 

# Constraints
MAX_SINGLE_ASSET   <- 0.20
MAX_BOND_TOTAL     <- 0.40
MAX_TOTAL_EXPOSURE <- 1.00 

# Asset Class Definitions (Specific to your Universe)
get_asset_class <- function(tickers) {
  # List of Bond ETFs identified in your data
  bond_tickers <- c("JPEICORE", "GBIE1001", "BCEX6T", "BCEX4T", "IDCOT7", 
                    "BEIG1T", "LECPTREU", "IBOXXMJA")
  
  ifelse(tickers %in% bond_tickers, "Bond", "Equity")
}

# =========================================================
# 2. PREPARE DATA
# =========================================================
# Prepare Daily Prices & Returns
daily_prices <- assets_wide %>% 
  arrange(date) %>%
  select(date, everything()) 

# Create XTS object for backtesting
daily_prices_xts <- daily_prices %>%
  select(-date) %>%
  xts(order.by = daily_prices$date)

# Calculate Daily Returns
daily_returns_xts <- Return.calculate(daily_prices_xts)

# Prepare Benchmark (Clean NAs for chart alignment)
bench_ret <- daily_returns_xts[, BENCHMARK_TICKER]
bench_ret <- na.omit(bench_ret)
colnames(bench_ret) <- "MSCI Emerging Markets (EUR)"

# =========================================================
# 3. WEIGHT DETERMINATION (OPTION C LOGIC)
# =========================================================
rebalance_dates <- unique(signals_long$date)
weights_history <- list()

cat("Calculating Option C Weights (Breadth) for", length(rebalance_dates), "periods...\n")

for (i in seq_along(rebalance_dates)) {
  curr_date <- rebalance_dates[i]
  
  # 1. Filter: Select ALL assets with Expected Return > 0
  # We do not limit to 'Top 5'. We trust the direction, not the magnitude.
  candidates <- signals_long %>%
    filter(date == curr_date) %>%
    filter(expected_return > 0) %>%
    mutate(asset_class = get_asset_class(asset))
  
  # Initialize zero weights for all assets in universe
  universe_assets <- colnames(daily_prices_xts)
  w_vec <- setNames(rep(0, length(universe_assets)), universe_assets)
  
  # If no assets have positive signal, we stay 100% Cash (w_vec remains 0)
  if (nrow(candidates) > 0) {
    
    # 2. Base Weight Calculation: Equal Weight (1 / N)
    n_candidates <- nrow(candidates)
    base_weight  <- 1 / n_candidates
    
    # Assign Base Weights to Candidates
    # 3. Apply Constraint 1: Single Asset Cap (Max 20%)
    # If 1/N is 33%, we cap at 20%. 
    # If 1/N is 5%, we keep 5%.
    final_single_weight <- min(base_weight, MAX_SINGLE_ASSET)
    
    # Temporary Weight Vector for Candidates
    candidate_weights <- rep(final_single_weight, n_candidates)
    names(candidate_weights) <- candidates$asset
    
    # 4. Apply Constraint 2: Bond Cap (Max 40% Total)
    bond_subset <- candidates %>% filter(asset_class == "Bond")
    
    if (nrow(bond_subset) > 0) {
      current_bond_total <- sum(candidate_weights[bond_subset$asset])
      
      if (current_bond_total > MAX_BOND_TOTAL) {
        # Calculate Scaling Factor (e.g., 0.40 / 0.60 = 0.66)
        scale_factor <- MAX_BOND_TOTAL / current_bond_total
        
        # Scale down ONLY the bonds
        candidate_weights[bond_subset$asset] <- candidate_weights[bond_subset$asset] * scale_factor
      }
    }
    
    # 5. Assign to Main Vector
    w_vec[names(candidate_weights)] <- candidate_weights
  }
  
  # Store weights
  weights_history[[as.character(curr_date)]] <- w_vec
}

# =========================================================
# 4. DAILY BACKTEST ENGINE
# =========================================================
all_dates <- index(daily_returns_xts)
strat_returns <- numeric(length(all_dates))

# Matrix to store daily weights for analysis
weight_matrix <- matrix(0, nrow = length(all_dates), ncol = ncol(daily_returns_xts))
colnames(weight_matrix) <- colnames(daily_returns_xts)

# Helper variables
asset_names <- colnames(daily_returns_xts)
current_weights <- setNames(rep(0, length(asset_names)), asset_names)
last_rebal_idx <- 0

cat("Running Daily Simulation...\n")

for (d in 1:length(all_dates)) {
  today <- all_dates[d]
  
  # Check for Rebalance (Trade on Day T+1 relative to Signal Date)
  valid_rebal_dates <- rebalance_dates[rebalance_dates < today]
  
  if (length(valid_rebal_dates) > 0) {
    latest_rebal <- max(valid_rebal_dates)
    
    # If we crossed a new rebalance date, update target weights
    if (as.numeric(latest_rebal) != last_rebal_idx) {
      target_w <- weights_history[[as.character(latest_rebal)]]
      
      # Update Portfolio (Instant Rebalance Assumption)
      current_weights[] <- 0 
      current_weights[names(target_w)] <- target_w
      last_rebal_idx <- as.numeric(latest_rebal)
    }
  }
  
  # Store Today's Allocation
  weight_matrix[d, ] <- current_weights
  
  # Calculate Today's Return
  # Logic: Sum(Asset_Return * Weight). 
  # Note: If Sum(Weights) < 1.0, the remainder is implicitly Cash (0% return).
  day_rets <- daily_returns_xts[d, ]
  day_rets[is.na(day_rets)] <- 0 # Handle missing data (assets not yet trading)
  
  strat_returns[d] <- sum(day_rets * current_weights)
}

# Create Strategy XTS
strat_xts <- xts(strat_returns, order.by = all_dates)
colnames(strat_xts) <- "Strategy"

# =========================================================
# 5. REPORTING & VISUALIZATION
# =========================================================

# 1. Sync Start Dates
# Find the first date we actually traded
first_trade_idx <- min(which(rowSums(weight_matrix) > 0))
start_date <- all_dates[first_trade_idx]

cat(paste("Backtest Start Date:", start_date, "\n"))

# 2. Merge & Trim
compare_xts_raw <- merge(strat_xts, bench_ret, join = "inner")
compare_xts <- compare_xts_raw[paste0(start_date, "/")]

# 3. Performance Stats
stats <- rbind(
  table.AnnualizedReturns(compare_xts),
  maxDrawdown(compare_xts)
)

cat("\n--- PERFORMANCE SUMMARY (All) ---\n")
print(round(stats, 4))

# 4. Performance Chart
charts.PerformanceSummary(compare_xts, 
                          main = "Regime Strategy - All Assets EW",
                          colorset = c("darkblue", "gray"),
                          lwd = 2)


# =========================================================
# 6. ALLOCATION ANALYSIS (FIXED)
# =========================================================
# Prepare Allocation Data for Plotting
weight_matrix_xts <- xts(weight_matrix, order.by = all_dates)
weight_sync <- weight_matrix_xts[paste0(start_date, "/")]

# Aggregate by Class
ac_map <- get_asset_class(colnames(weight_sync))

# FIX: Wrap rowSums in xts() to preserve the time index
# We use 'na.rm=TRUE' just to be safe, though 0s should be fine.
equity_alloc <- xts(rowSums(weight_sync[, ac_map == "Equity"], na.rm=TRUE), order.by = index(weight_sync))
bond_alloc   <- xts(rowSums(weight_sync[, ac_map == "Bond"],   na.rm=TRUE), order.by = index(weight_sync))

# Calculate Cash (Residual)
# Since equity_alloc and bond_alloc are now xts, this math works correctly
cash_alloc   <- 1 - (equity_alloc + bond_alloc)

# Fix slight floating point errors (e.g. -0.0000001 becoming 0)
cash_alloc[cash_alloc < 0] <- 0
cash_alloc[cash_alloc > 1] <- 1

# Merge the three XTS objects
alloc_df <- merge(equity_alloc, bond_alloc, cash_alloc)
colnames(alloc_df) <- c("Equity", "Bond", "Cash")

# Stacked Bar Chart (Monthly Snapshot)
# We use 'endpoints' to pick the last day of every month to make the chart readable
chart.StackedBar(alloc_df[endpoints(alloc_df, "months")], 
                 main = "Portfolio Allocation Over Time",
                 colorset = c("darkgreen", "orange", "lightgray"),
                 ylab = "Exposure", 
                 ylim = c(0, 1.0),
                 border=NA)

# Current Portfolio Snapshot
cat("\n--- LATEST PORTFOLIO ALLOCATION ---\n")
latest_w <- coredata(weight_sync)[nrow(weight_sync), ]
active_pos <- latest_w[latest_w > 0.001] # Filter small dust
print(round(active_pos, 3))

cat("\nTotal Exposure:", sum(active_pos), "\n")
# We calculate class sums manually for the print output
cat("Bond Exposure: ", sum(latest_w[ac_map == "Bond"]), "\n")
cat("Cash Exposure: ", 1 - sum(active_pos), "\n")