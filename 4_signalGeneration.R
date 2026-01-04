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
  #mutate(month_date = floor_date(date, "month")) %>%
  mutate(month_date = ceiling_date(date, "month") - days(1)) %>%
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

# =========================================================
# MODIFIED SIGNAL GENERATION (With Minimum N Check)
# =========================================================

# Add this constant at the top of Script 4
MIN_NEIGHBORS_REQUIRED <- 6  # <--- Adjust this (3 to 5 is best)

generate_signals <- function(regime_dist_df, asset_ret_df, percentile) {
  
  valid_dates <- unique(regime_dist_df$Analysis_Date)
  min_asset_date <- min(asset_ret_df$month_date)
  valid_dates <- valid_dates[valid_dates >= min_asset_date]
  
  results_list <- list()
  
  cat("Generating Signals with Min Neighbor Constraint...\n")
  
  for (i in seq_along(valid_dates)) {
    curr_date <- valid_dates[i]
    
    # 1. Get Top Neighbors
    current_regimes <- regime_dist_df %>% filter(Analysis_Date == curr_date)
    n_history <- nrow(current_regimes)
    n_select <- ceiling(n_history * percentile)
    
    top_neighbors <- current_regimes %>%
      arrange(Distance) %>%
      head(n_select) %>%
      pull(Neighbor_Date)
    
    # 2. Calculate Signals with "Safety Filter"
    neighbor_returns <- asset_ret_df %>%
      filter(month_date %in% top_neighbors) %>%
      group_by(asset) %>%
      summarise(
        neighbors_found = n(), # Count matches BEFORE calculating mean
        
        # --- THE FIX ---
        # If we have fewer than X neighbors, force the Signal to 0.
        expected_return = if (n() < MIN_NEIGHBORS_REQUIRED) 0 else mean(fwd_ret_1m, na.rm = TRUE),
        
        win_rate = if (n() < MIN_NEIGHBORS_REQUIRED) 0 else sum(fwd_ret_1m > 0) / n(),
        # ---------------
        
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

