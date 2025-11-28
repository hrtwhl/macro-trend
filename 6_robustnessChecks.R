# =========================================================
# SCRIPT 6: ROBUSTNESS CHECKS (THE STRESS TEST)
# =========================================================
# Purpose: 
# 1. Stress test parameters (Thresholds, Concentration).
# 2. Variable Importance (Leave-One-Out).
# 3. Methodological Validation (Anti-Regimes).
# 4. Generate Summary Tables & Visuals.

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(ggplot2) # For Heatmaps/Plots

# =========================================================
# 1. DEFINE WRAPPER FUNCTIONS
# =========================================================
# We need to wrap the logic from previous scripts into functions 
# so we can loop them with different inputs.

# A) Distance Engine Wrapper (From Script 3)
# Calculates distance matrix based on a specific set of Z-score columns
calc_distance_wrapper <- function(z_df, columns_to_use) {
  # Subset the Z-score dataframe
  mat_data <- as.matrix(z_df %>% select(all_of(columns_to_use)))
  dates <- z_df$month_date
  n_rows <- nrow(mat_data)
  
  results <- list()
  start_sim <- 12 
  
  for (i in start_sim:n_rows) {
    current_vec <- mat_data[i, , drop = FALSE]
    history_mat <- mat_data[1:(i-1), , drop = FALSE]
    
    diff_sq <- sweep(history_mat, 2, current_vec, "-")^2
    dists <- sqrt(rowSums(diff_sq))
    
    results[[i]] <- tibble(
      Analysis_Date = dates[i],
      Neighbor_Date = dates[1:(i-1)],
      Distance = dists
    )
  }
  bind_rows(results)
}

# B) Signal Engine Wrapper (From Script 4)
# Generates signals based on Percentile and Direction (Similar vs Dissimilar)
gen_signal_wrapper <- function(regime_df, asset_ret_df, percentile, direction = "top") {
  
  valid_dates <- unique(regime_df$Analysis_Date)
  min_asset_date <- min(asset_ret_df$month_date)
  valid_dates <- valid_dates[valid_dates >= min_asset_date]
  
  results_list <- list()
  
  for (i in seq_along(valid_dates)) {
    curr_date <- valid_dates[i]
    current_regimes <- regime_df %>% filter(Analysis_Date == curr_date)
    
    # Determine selection count
    n_select <- ceiling(nrow(current_regimes) * percentile)
    
    # Sort: Ascending for Similarity (Top), Descending for Anti-Regime (Bottom)
    if (direction == "top") {
      sorted_regimes <- current_regimes %>% arrange(Distance)
    } else {
      sorted_regimes <- current_regimes %>% arrange(desc(Distance))
    }
    
    top_neighbors <- head(sorted_regimes, n_select)$Neighbor_Date
    
    # Calculate Mean Returns
    neighbor_returns <- asset_ret_df %>%
      filter(month_date %in% top_neighbors) %>%
      group_by(asset) %>%
      summarise(expected_return = mean(fwd_ret_1m, na.rm = TRUE), .groups = "drop") %>%
      mutate(date = curr_date)
    
    results_list[[i]] <- neighbor_returns
  }
  bind_rows(results_list)
}

# C) Backtest Engine Wrapper (From Script 5 - Top N Logic)
# Runs the backtest given specific signals and concentration constraints
run_backtest_wrapper <- function(signals_in, max_assets, max_single_weight) {
  
  # Configuration
  MAX_BOND_ALLOC     <- 0.40
  MAX_TOTAL_EXPOSURE <- 1.00
  
  rebalance_dates <- unique(signals_in$date)
  weights_history <- list()
  
  for (i in seq_along(rebalance_dates)) {
    curr_date <- rebalance_dates[i]
    
    # Select Candidates
    sig_subset <- signals_in %>%
      filter(date == curr_date, expected_return > 0) %>% # Positive exp return only
      arrange(desc(expected_return)) # Greedy Rank
    
    # Determine Asset Classes
    # (Assuming helper function 'get_asset_class' exists from Script 5)
    sig_subset$asset_class <- get_asset_class(sig_subset$asset)
    
    # Greedy Allocation Loop
    universe_assets <- unique(signals_in$asset) 
    # Note: In robust code we usually match universe to price data, 
    # but here we simplify using signal assets for the loop
    w_vec <- setNames(rep(0, length(universe_assets)), universe_assets)
    
    current_bond_exp <- 0
    total_exp <- 0
    
    if (nrow(sig_subset) > 0) {
      for (j in 1:nrow(sig_subset)) {
        if (total_exp >= MAX_TOTAL_EXPOSURE - 0.001) break
        
        asset_name <- sig_subset$asset[j]
        aclass     <- sig_subset$asset_class[j]
        
        room_total <- MAX_TOTAL_EXPOSURE - total_exp
        room_class <- if (aclass == "Bond") MAX_BOND_ALLOC - current_bond_exp else 1.00
        
        # Use the passed 'max_single_weight' parameter
        allocation <- min(max_single_weight, room_class, room_total)
        
        if (allocation > 0.001) {
          w_vec[asset_name] <- allocation
          total_exp <- total_exp + allocation
          if (aclass == "Bond") current_bond_exp <- current_bond_exp + allocation
        }
      }
    }
    weights_history[[as.character(curr_date)]] <- w_vec
  }
  
  # Quick Daily Simulation (Vectorized-ish)
  # We need to map these weights to the daily returns used in Script 5
  # We assume 'daily_returns_xts' exists globally from Script 5
  
  strat_daily <- numeric(nrow(daily_returns_xts))
  all_dates <- index(daily_returns_xts)
  
  # Align weights
  curr_w <- setNames(rep(0, ncol(daily_returns_xts)), colnames(daily_returns_xts))
  last_rebal <- 0
  
  for (d in 1:length(all_dates)) {
    today <- all_dates[d]
    valid_dates <- rebalance_dates[rebalance_dates < today]
    
    if (length(valid_dates) > 0) {
      latest <- max(valid_dates)
      if (as.numeric(latest) != last_rebal) {
        # Update weights
        target <- weights_history[[as.character(latest)]]
        curr_w[] <- 0
        # Map target names to current_w names carefully
        common <- intersect(names(target), names(curr_w))
        curr_w[common] <- target[common]
        last_rebal <- as.numeric(latest)
      }
    }
    
    # Calc Return
    day_r <- daily_returns_xts[d, ]
    day_r[is.na(day_r)] <- 0
    strat_daily[d] <- sum(day_r * curr_w)
  }
  
  # Calculate Stats
  strat_xts <- xts(strat_daily, order.by = all_dates)
  
  # Trim to start date (same logic as Script 5)
  first_idx <- min(which(strat_daily != 0))
  if(is.infinite(first_idx)) first_idx <- 1
  strat_trimmed <- strat_xts[first_idx:nrow(strat_xts)]
  
  ann_ret <- Return.annualized(strat_trimmed)
  sharpe  <- SharpeRatio.annualized(strat_trimmed, Rf=0)
  max_dd  <- maxDrawdown(strat_trimmed)
  
  return(c(Return = ann_ret, Sharpe = sharpe, Drawdown = max_dd))
}

# =========================================================
# 2. RUN ROBUSTNESS CHECKS
# =========================================================
# Container for all results
robustness_log <- list()

cat("Starting Robustness Checks... This will take a few minutes.\n")

# ---------------------------------------------------------
# CHECK 1: THRESHOLD SENSITIVITY (10% to 30%)
# ---------------------------------------------------------
cat("\n[1/4] Running Threshold Sensitivity...\n")
thresholds <- c(0.10, 0.15, 0.20, 0.25, 0.30)
res_thresh <- list()

for (th in thresholds) {
  cat("   Testing Threshold:", th * 100, "% ...\n")
  # 1. Gen Signals
  sigs <- gen_signal_wrapper(regime_distances, asset_monthly_returns, percentile = th, direction = "top")
  # 2. Backtest (Standard Top 10 Config)
  stats <- run_backtest_wrapper(sigs, max_assets = 10, max_single_weight = 0.10)
  res_thresh[[as.character(th)]] <- c(Parameter = th, stats)
}
robustness_log[["Threshold"]] <- do.call(rbind, res_thresh)

# ---------------------------------------------------------
# CHECK 2: CONCENTRATION SENSITIVITY (Top 5 vs 10 vs 15)
# ---------------------------------------------------------
cat("\n[2/4] Running Concentration Sensitivity...\n")
# We use the standard 20% threshold signals for this test
base_signals <- signals_long # From Script 4 (20% threshold)

configs <- list(
  "Top 5"  = list(max_a = 5,  max_w = 0.20),
  "Top 10" = list(max_a = 10, max_w = 0.10),
  "Top 15" = list(max_a = 15, max_w = 0.066),
  "Top 20" = list(max_a = 20, max_w = 0.05)
)
res_conc <- list()

for (nm in names(configs)) {
  cat("   Testing Config:", nm, "...\n")
  stats <- run_backtest_wrapper(base_signals, max_assets = configs[[nm]]$max_a, max_single_weight = configs[[nm]]$max_w)
  res_conc[[nm]] <- c(Config = nm, stats)
}
robustness_log[["Concentration"]] <- do.call(rbind, res_conc)

# ---------------------------------------------------------
# CHECK 3: VARIABLE IMPORTANCE (Leave-One-Out)
# ---------------------------------------------------------
cat("\n[3/4] Running Variable Importance (LOO)...\n")
# Ensure we have macro_z from Script 3
# If macro_z_scores.rds exists, load it, otherwise use environment object
if (!exists("macro_z")) macro_z <- readRDS("macro_z_scores.rds")

# Identify columns (excluding date)
all_vars <- colnames(macro_z)[colnames(macro_z) != "month_date"]
res_loo <- list()

# Baseline (All Vars)
cat("   Running Baseline (All Variables)...\n")
# We already have regime_distances for all vars, but let's calc fresh to be consistent
dist_base <- calc_distance_wrapper(macro_z, all_vars)
sig_base  <- gen_signal_wrapper(dist_base, asset_monthly_returns, 0.20)
stat_base <- run_backtest_wrapper(sig_base, 10, 0.10)
res_loo[["Baseline"]] <- c(Variable_Removed = "None (Baseline)", stat_base)

# Loop Remove 1
for (v in all_vars) {
  cat("   Removing:", v, "...\n")
  subset_vars <- setdiff(all_vars, v)
  
  # Recalc Chain
  dist_loo <- calc_distance_wrapper(macro_z, subset_vars)
  sig_loo  <- gen_signal_wrapper(dist_loo, asset_monthly_returns, 0.20)
  stat_loo <- run_backtest_wrapper(sig_loo, 10, 0.10)
  
  res_loo[[v]] <- c(Variable_Removed = v, stat_loo)
}
robustness_log[["LOO"]] <- do.call(rbind, res_loo)

# ---------------------------------------------------------
# CHECK 4: ANTI-REGIME (Method Validation)
# ---------------------------------------------------------
cat("\n[4/4] Running Anti-Regime Test...\n")
# Use Bottom 20% (Most Dissimilar)
sig_anti <- gen_signal_wrapper(regime_distances, asset_monthly_returns, percentile = 0.20, direction = "bottom")
stat_anti <- run_backtest_wrapper(sig_anti, 10, 0.10)

res_anti <- rbind(
  c(Type = "Similarity (Top 20%)", stat_base), # From LOO baseline
  c(Type = "Dissimilarity (Bottom 20%)", stat_anti)
)
robustness_log[["AntiRegime"]] <- res_anti

# =========================================================
# 3. GENERATE VISUALS (CORRECTED)
# =========================================================

# 1. Threshold Plot
df_thresh <- as.data.frame(robustness_log[["Threshold"]])

# FIX: Use the actual column names from the wrapper output ("Sharpe", not "AnnualizedSharpeRatio")
df_thresh$Sharpe <- as.numeric(df_thresh$Sharpe) 
df_thresh$Param  <- as.numeric(df_thresh$Parameter)

barplot(height = df_thresh$Sharpe, names.arg = df_thresh$Param,
        main = "Robustness: Sharpe Ratio vs Similarity Threshold",
        xlab = "Similarity Percentile (Top %)", ylab = "Sharpe Ratio",
        col = "lightblue", ylim = c(0, max(df_thresh$Sharpe)*1.2))
abline(h = mean(df_thresh$Sharpe), col="red", lty=2)

# 2. LOO Plot (Variable Importance)
df_loo <- as.data.frame(robustness_log[["LOO"]])
df_loo$Sharpe <- as.numeric(df_loo$Sharpe)

# Calculate Delta from Baseline
# Ensure we match the "None (Baseline)" string exactly as defined in the loop
baseline_sr <- df_loo$Sharpe[df_loo$Variable_Removed == "None (Baseline)"]
df_loo$Delta <- df_loo$Sharpe - baseline_sr

# Remove baseline row for plotting deltas
df_plot_loo <- df_loo[df_loo$Variable_Removed != "None (Baseline)", ]

# Sort for better visualization
df_plot_loo <- df_plot_loo[order(df_plot_loo$Delta), ]

par(mar=c(5,10,4,2)) # Increase left margin for variable names
barplot(height = df_plot_loo$Delta, names.arg = df_plot_loo$Variable_Removed,
        horiz = TRUE, las = 1,
        main = "Impact of Removing Variable (Delta Sharpe)",
        xlab = "Change in Sharpe Ratio (Positive = Variable was Noise)",
        col = ifelse(df_plot_loo$Delta > 0, "darkgreen", "red")) 

# 3. Anti-Regime Plot (Simple Bar)
df_anti <- as.data.frame(robustness_log[["AntiRegime"]])
df_anti$Sharpe <- as.numeric(df_anti$Sharpe)

par(mar=c(5,5,4,2))
barplot(height = df_anti$Sharpe, names.arg = df_anti$Type,
        main = "Method Validation: Similarity vs Dissimilarity",
        ylab = "Sharpe Ratio",
        col = c("blue", "gray"))
