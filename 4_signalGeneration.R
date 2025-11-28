# =========================================================
# SCRIPT 4: SIGNAL GENERATION (THE FORECAST)
# =========================================================
# Purpose: 
# 1. Calculate future returns for all assets (the "Target").
# 2. Map "Nearest Neighbors" (from Script 3) to these returns.
# 3. Generate a signal: "Average Return of Asset X in Similar Regimes".

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

# =========================================================
# 1. CONFIGURATION
# =========================================================
# Paper parameter: "We use the full history and pick the 15% most similar months" [cite: 360]
# or "quintile 1" (20%)[cite: 480]. 
SIMILARITY_PERCENTILE <- 0.20 

# =========================================================
# 2. PREPARE ASSET RETURNS (TARGETS)
# =========================================================
# We need the return the strategy WOULD capture if it invested.
# Since we make the decision at month-end T, we capture the return T+1.

# Ensure we are working with the 'assets_long' from Script 1
# We aggregate to monthly if the data is daily
asset_monthly_returns <- assets_long %>%
  mutate(month_date = floor_date(date, "month")) %>%
  group_by(asset, month_date) %>%
  summarise(price = last(price), .groups = "drop") %>% # Get month-end price
  arrange(asset, month_date) %>%
  group_by(asset) %>%
  mutate(
    # 1-Month Forward Return (The "Result" of the regime)
    # If we invest at end of Jan, we get the return of Feb (Price_Feb / Price_Jan - 1)
    fwd_ret_1m = lead(price, 1) / price - 1
  ) %>%
  ungroup() %>%
  drop_na(fwd_ret_1m)

# =========================================================
# 3. SIGNAL CONSTRUCTION ENGINE
# =========================================================
# We iterate through the regime_distances object.
# For each Analysis_Date, we:
# 1. Identify the Top N neighbors.
# 2. Lookup the 'fwd_ret_1m' for those neighbor dates.
# 3. Average them to get the Expected Return.

generate_signals <- function(regime_dist_df, asset_ret_df, percentile) {
  
  # Get unique analysis dates from the regime output
  # Filter only dates where we actually have asset data (post-2009)
  valid_dates <- unique(regime_dist_df$Analysis_Date)
  min_asset_date <- min(asset_ret_df$month_date)
  
  # Only generate signals if we are at least past the start of asset data
  valid_dates <- valid_dates[valid_dates >= min_asset_date]
  
  results_list <- list()
  
  cat("Generating Signals for", length(valid_dates), "months...\n")
  
  for (i in seq_along(valid_dates)) {
    curr_date <- valid_dates[i]
    
    # 1. Get Neighbors for this date
    # We first determine how many neighbors constitute the top X% 
    # based on the history available AT THAT MOMENT.
    
    current_regimes <- regime_dist_df %>%
      filter(Analysis_Date == curr_date)
    
    # Number of historical months available so far
    n_history <- nrow(current_regimes)
    
    # Select Top N (Top 20% closest)
    # Paper: "Select the 15% most similar months" [cite: 360]
    n_select <- ceiling(n_history * percentile)
    
    top_neighbors <- current_regimes %>%
      arrange(Distance) %>%
      head(n_select) %>%
      pull(Neighbor_Date)
    
    # 2. Retrieve Asset Returns for these Neighbors
    # This automatically handles the "Data starts in 2009" issue.
    # If a neighbor is 2005, it won't be found in 'asset_ret_df', 
    # so it won't contribute to the mean.
    
    neighbor_returns <- asset_ret_df %>%
      filter(month_date %in% top_neighbors) %>%
      group_by(asset) %>%
      summarise(
        # The Core Signal: Average return in similar regimes
        expected_return = mean(fwd_ret_1m, na.rm = TRUE),
        
        # Diagnostics: How many neighbors actually had data?
        neighbors_found = n(), 
        
        # Risk Metric: Consistency (Win Rate of the neighbors)
        win_rate = sum(fwd_ret_1m > 0) / n(),
        .groups = "drop"
      ) %>%
      mutate(date = curr_date)
    
    results_list[[i]] <- neighbor_returns
  }
  
  bind_rows(results_list)
}

signals_long <- generate_signals(regime_distances, asset_monthly_returns, SIMILARITY_PERCENTILE)

# =========================================================
# 4. DIAGNOSTICS & OUTPUT
# =========================================================

# Check 1: Coverage
# Do we have signals for your key ETFs?
cat("\nSignal Coverage (Sample Assets):\n")
print(head(unique(signals_long$asset), 10))

# Check 2: The "2009 Problem"
# Let's check a date early in the sample (e.g., 2011) to see 
# if 'neighbors_found' is lower than recent dates.
early_date_check <- min(signals_long$date) + years(1)
recent_date_check <- max(signals_long$date)

cat("\n--- Data Availability Check ---\n")
cat("Early Date (", as.character(early_date_check), ") Neighbors found:", 
    mean(signals_long$neighbors_found[signals_long$date == early_date_check]), "\n")
cat("Recent Date (", as.character(recent_date_check), ") Neighbors found:", 
    mean(signals_long$neighbors_found[signals_long$date == recent_date_check]), "\n")

# Check 3: Current Signal (Actionable Insight)
# What is the model predicting for next month?
cat("\n--- LATEST TRADING SIGNAL (", as.character(max(signals_long$date)), ") ---\n")

signals_long %>%
  filter(date == max(date)) %>%
  select(asset, expected_return, win_rate, neighbors_found) %>%
  arrange(desc(expected_return)) %>%
  print(n = 20)

# Save for Script 5
# saveRDS(signals_long, "signals_long.rds")
# saveRDS(asset_monthly_returns, "asset_monthly_returns.rds")