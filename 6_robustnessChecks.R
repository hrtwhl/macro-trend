# =========================================================
# SCRIPT 6: ROBUSTNESS CHECKS (THE STRESS TEST) - FINAL
# =========================================================
# Purpose: 
# 1. Stress test parameters (Thresholds, Concentration).
# 2. Variable Importance (Leave-One-Out) using Aggressive Top 5.
# 3. Methodological Validation (Anti-Regimes).
# 4. Execution Lag Sensitivity (Trading Delay).
# 5. Generate Summary Tables & Visuals.

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(ggplot2) # For Heatmaps/Plots

# =========================================================
# 1. SETUP & HELPER FUNCTIONS
# =========================================================

# Helper: Map Tickers to Class (Global definition to prevent crashes)
get_asset_class <- function(tickers) {
  # Add your specific bond tickers here
  bond_tickers <- c("JPEICORE", "GBIE1001", "BCEX6T", "BCEX4T", "IDCOT7", 
                    "BEIG1T", "LECPTREU", "IBOXXMJA")
  ifelse(tickers %in% bond_tickers, "Bond", "Equity")
}

# ---------------------------------------------------------
# WRAPPER FUNCTIONS
# ---------------------------------------------------------

# A) Distance Engine Wrapper
calc_distance_wrapper <- function(z_df, columns_to_use) {
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

# B) Signal Engine Wrapper
gen_signal_wrapper <- function(regime_df, asset_ret_df, percentile, direction = "top") {
  valid_dates <- unique(regime_df$Analysis_Date)
  min_asset_date <- min(asset_ret_df$month_date)
  valid_dates <- valid_dates[valid_dates >= min_asset_date]
  results_list <- list()
  
  for (i in seq_along(valid_dates)) {
    curr_date <- valid_dates[i]
    current_regimes <- regime_df %>% filter(Analysis_Date == curr_date)
    
    n_select <- ceiling(nrow(current_regimes) * percentile)
    
    if (direction == "top") {
      sorted_regimes <- current_regimes %>% arrange(Distance)
    } else {
      sorted_regimes <- current_regimes %>% arrange(desc(Distance))
    }
    
    top_neighbors <- head(sorted_regimes, n_select)$Neighbor_Date
    
    neighbor_returns <- asset_ret_df %>%
      filter(month_date %in% top_neighbors) %>%
      group_by(asset) %>%
      summarise(expected_return = mean(fwd_ret_1m, na.rm = TRUE), .groups = "drop") %>%
      mutate(date = curr_date)
    
    results_list[[i]] <- neighbor_returns
  }
  bind_rows(results_list)
}

# C) Backtest Engine Wrapper
run_backtest_wrapper <- function(signals_in, max_assets, max_single_weight) {
  
  MAX_BOND_ALLOC     <- 0.40
  MAX_TOTAL_EXPOSURE <- 1.00
  
  rebalance_dates <- unique(signals_in$date)
  weights_history <- list()
  
  for (i in seq_along(rebalance_dates)) {
    curr_date <- rebalance_dates[i]
    
    sig_subset <- signals_in %>%
      filter(date == curr_date, expected_return > 0) %>% 
      arrange(desc(expected_return)) 
    
    sig_subset$asset_class <- get_asset_class(sig_subset$asset)
    
    universe_assets <- unique(signals_in$asset) 
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
  
  strat_daily <- numeric(nrow(daily_returns_xts))
  all_dates <- index(daily_returns_xts)
  curr_w <- setNames(rep(0, ncol(daily_returns_xts)), colnames(daily_returns_xts))
  last_rebal <- 0
  
  for (d in 1:length(all_dates)) {
    today <- all_dates[d]
    valid_dates <- rebalance_dates[rebalance_dates < today]
    
    if (length(valid_dates) > 0) {
      latest <- max(valid_dates)
      if (as.numeric(latest) != last_rebal) {
        target <- weights_history[[as.character(latest)]]
        curr_w[] <- 0
        common <- intersect(names(target), names(curr_w))
        curr_w[common] <- target[common]
        last_rebal <- as.numeric(latest)
      }
    }
    
    day_r <- daily_returns_xts[d, ]
    day_r[is.na(day_r)] <- 0
    strat_daily[d] <- sum(day_r * curr_w)
  }
  
  strat_xts <- xts(strat_daily, order.by = all_dates)
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
robustness_log <- list()
cat("Starting Robustness Checks... This will take a few minutes.\n")

# Ensure dependencies are loaded
if (!exists("macro_z")) macro_z <- readRDS("macro_z_scores.rds")
if (!exists("daily_returns_xts")) stop("Warning: daily_returns_xts missing! Run Script 5 first.")

# ---------------------------------------------------------
# CHECK 1: THRESHOLD SENSITIVITY (Aggressive Top 5 Config)
# ---------------------------------------------------------
cat("\n[1/5] Running Threshold Sensitivity (Top 5 Config)...\n")
thresholds <- c(0.10, 0.15, 0.20, 0.25, 0.30)
res_thresh <- list()

for (th in thresholds) {
  cat("   Testing Threshold:", th * 100, "% ...\n")
  sigs <- gen_signal_wrapper(regime_distances, asset_monthly_returns, percentile = th, direction = "top")
  # UPDATED: Use Top 5 (0.20 weight) to match Script 5
  stats <- run_backtest_wrapper(sigs, max_assets = 5, max_single_weight = 0.20)
  res_thresh[[as.character(th)]] <- c(Parameter = th, stats)
}
robustness_log[["Threshold"]] <- do.call(rbind, res_thresh)

# ---------------------------------------------------------
# CHECK 2: CONCENTRATION SENSITIVITY
# ---------------------------------------------------------
cat("\n[2/5] Running Concentration Sensitivity...\n")
base_signals <- signals_long 

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
# CHECK 3: VARIABLE IMPORTANCE (LOO) (Aggressive Top 5 Config)
# ---------------------------------------------------------
cat("\n[3/5] Running Variable Importance (Top 5 Config)...\n")
all_vars <- colnames(macro_z)[colnames(macro_z) != "month_date"]
res_loo <- list()

# Baseline
cat("   Running Baseline (All Variables)...\n")
dist_base <- calc_distance_wrapper(macro_z, all_vars)
sig_base  <- gen_signal_wrapper(dist_base, asset_monthly_returns, 0.20)
# UPDATED: Use Top 5
stat_base <- run_backtest_wrapper(sig_base, 5, 0.20)
res_loo[["Baseline"]] <- c(Variable_Removed = "None (Baseline)", stat_base)

for (v in all_vars) {
  cat("   Removing:", v, "...\n")
  subset_vars <- setdiff(all_vars, v)
  
  dist_loo <- calc_distance_wrapper(macro_z, subset_vars)
  sig_loo  <- gen_signal_wrapper(dist_loo, asset_monthly_returns, 0.20)
  # UPDATED: Use Top 5
  stat_loo <- run_backtest_wrapper(sig_loo, 5, 0.20)
  
  res_loo[[v]] <- c(Variable_Removed = v, stat_loo)
}
robustness_log[["LOO"]] <- do.call(rbind, res_loo)

# ---------------------------------------------------------
# CHECK 4: ANTI-REGIME (Aggressive Top 5 Config)
# ---------------------------------------------------------
cat("\n[4/5] Running Anti-Regime Test (Top 5 Config)...\n")
sig_anti <- gen_signal_wrapper(regime_distances, asset_monthly_returns, percentile = 0.20, direction = "bottom")
# UPDATED: Use Top 5
stat_anti <- run_backtest_wrapper(sig_anti, 5, 0.20)

res_anti <- rbind(
  c(Type = "Similarity (Top 5)", stat_base), # From LOO baseline
  c(Type = "Dissimilarity (Bottom 5)", stat_anti)
)
robustness_log[["AntiRegime"]] <- res_anti

# ---------------------------------------------------------
# CHECK 5: EXECUTION LAG SENSITIVITY (Exact Logic)
# ---------------------------------------------------------
cat("\n[5/5] Running Execution Lag Test (Exact Greedy Logic)...\n")

lags_to_test <- c(0, 1, 2, 3, 5, 10)

run_lagged_backtest_exact <- function(lag_days, signals, daily_rets) {
  
  # Constants from Script 5
  MAX_SINGLE_ASSET   <- 0.20
  MAX_BOND_ALLOC     <- 0.40
  MAX_EQUITY_ALLOC   <- 1.00
  MAX_TOTAL_EXPOSURE <- 1.00 
  
  all_dates <- zoo::index(daily_rets)
  n_days    <- length(all_dates)
  strat_returns <- numeric(n_days)
  
  rebalance_dates <- unique(signals$date)
  trading_schedule <- list()
  
  # Inner function to calculate greedy weights
  calc_weights_greedy <- function(d) {
    sig_subset <- signals %>% 
      filter(date == d) %>% 
      mutate(asset_class = get_asset_class(asset)) %>%
      arrange(desc(expected_return))
    
    w_vec <- setNames(rep(0, ncol(daily_rets)), colnames(daily_rets))
    current_equity_exp <- 0
    current_bond_exp   <- 0
    total_exp          <- 0
    
    for (j in 1:nrow(sig_subset)) {
      if (total_exp >= MAX_TOTAL_EXPOSURE) break
      
      asset_name <- sig_subset$asset[j]
      aclass     <- sig_subset$asset_class[j]
      exp_ret    <- sig_subset$expected_return[j]
      
      if (is.na(exp_ret) || exp_ret <= 0) next 
      
      room_total <- MAX_TOTAL_EXPOSURE - total_exp
      room_class <- if (aclass == "Bond") MAX_BOND_ALLOC - current_bond_exp else MAX_EQUITY_ALLOC - current_equity_exp
      
      allocation <- min(MAX_SINGLE_ASSET, room_class, room_total)
      
      if (allocation > 0.001) {
        if (asset_name %in% names(w_vec)) {
          w_vec[asset_name] <- allocation
          total_exp <- total_exp + allocation
          if (aclass == "Bond") current_bond_exp <- current_bond_exp + allocation else current_equity_exp <- current_equity_exp + allocation
        }
      }
    }
    return(w_vec)
  }
  
  # Schedule trading with Lag
  for (d_idx in seq_along(rebalance_dates)) {
    sig_date <- rebalance_dates[d_idx]
    
    idx_in_xts <- which(all_dates <= sig_date)
    if (length(idx_in_xts) == 0) next
    last_idx <- max(idx_in_xts)
    
    # Apply Lag: Trade at signal_date + 1 (normal) + lag
    target_idx <- last_idx + 1 + lag_days
    
    if (target_idx <= n_days) {
      trade_date <- all_dates[target_idx]
      weights <- calc_weights_greedy(sig_date)
      trading_schedule[[as.character(trade_date)]] <- weights
    }
  }
  
  # Daily Simulation
  curr_weights <- setNames(rep(0, ncol(daily_rets)), colnames(daily_rets))
  
  for (i in 1:n_days) {
    today_char <- as.character(all_dates[i])
    if (!is.null(trading_schedule[[today_char]])) {
      curr_weights <- trading_schedule[[today_char]]
    }
    day_ret_vals <- daily_rets[i, ]
    day_ret_vals[is.na(day_ret_vals)] <- 0
    strat_returns[i] <- sum(day_ret_vals * curr_weights)
  }
  
  # Stats
  strat_xts <- xts(strat_returns, order.by = all_dates)
  first_trade <- min(which(strat_returns != 0))
  if(is.infinite(first_trade)) first_trade <- 1
  strat_xts <- strat_xts[first_trade:n_days]
  
  ann_ret <- Return.annualized(strat_xts)
  sharpe  <- SharpeRatio.annualized(strat_xts, Rf=0)
  
  return(c(Lag = lag_days, Return = ann_ret, Sharpe = sharpe))
}

res_lag <- list()
for (L in lags_to_test) {
  cat("   Testing Trading Delay:", L, "Days...\n")
  res_lag[[as.character(L)]] <- run_lagged_backtest_exact(L, signals_long, daily_returns_xts)
}
df_lag <- do.call(rbind, res_lag) %>% as.data.frame()
robustness_log[["ExecutionLag"]] <- df_lag


# =========================================================
# 3. GENERATE VISUALS (FINAL)
# =========================================================

# 1. Threshold Plot
df_thresh <- as.data.frame(robustness_log[["Threshold"]])
df_thresh$Sharpe <- as.numeric(df_thresh$Sharpe) 
df_thresh$Param  <- as.numeric(df_thresh$Parameter)

ggplot(df_thresh, aes(x = factor(Param), y = Sharpe)) +
  # 1. Draw bars (fill mimics 'col' in base R, color is the border)
  geom_col(fill = "#003f5c", width = 0.7) +
  
  # 3. Add titles and labels
  labs(title = "Sharpe Ratio vs Similarity Threshold",
       x = "Similarity Percentile (Top %)",
       y = "Sharpe Ratio") +
  
  # 4. Handle Y-axis limits (expand adds the 20% buffer at the top)
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  
  # 5. Clean theme
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center title
    axis.text.x = element_text(angle = 0) # Adjust angle if labels overlap
  )


# 2. LOO Plot (Variable Importance)
df_loo <- as.data.frame(robustness_log[["LOO"]])
df_loo$Sharpe <- as.numeric(df_loo$Sharpe)
baseline_sr <- df_loo$Sharpe[df_loo$Variable_Removed == "None (Baseline)"]
df_loo$Delta <- df_loo$Sharpe - baseline_sr
df_plot_loo <- df_loo[df_loo$Variable_Removed != "None (Baseline)", ]
df_plot_loo$Color <- ifelse(df_plot_loo$Delta > 0, "#003f5c", "#003f5c") 

print(
  ggplot(df_plot_loo, aes(x = reorder(Variable_Removed, Delta), y = Delta, fill = Color)) +
    geom_col() +
    coord_flip() +
    scale_fill_identity() +
    labs(title = "Variable Importance",
         y = "Change in Sharpe Ratio", x = "Removed Variable") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) # Center title)
)

# 3. Anti-Regime Plot
df_anti <- as.data.frame(robustness_log[["AntiRegime"]])
df_anti$Sharpe <- as.numeric(df_anti$Sharpe)
par(mar=c(5,5,4,2))
ggplot(df_anti, aes(x = Type, y = Sharpe, fill = Type)) +
  # 1. Create bars with black borders
  geom_col(width = 0.3) +
  
  # 2. Replicate the specific color scheme ("blue", "gray")
  scale_fill_manual(values = c("#003f5c", "#003f5c")) +
  
  # 3. Titles and Labels
  labs(title = "Top 5 Similarity vs Dissimilarity",
       y = "Sharpe Ratio",
       x = NULL) + # x-label is usually removed if the categories explain themselves
  
  # 4. Clean theme & formatting
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"), # Center title
    legend.position = "none", # Remove legend since x-axis labels already show the types
    axis.text.x = element_text(size = 11) # Make x-axis labels slightly readable
  )

# 4. Execution Lag Plot
library(ggplot2)

# Data prep
df_lag <- as.data.frame(robustness_log[["ExecutionLag"]])

# Create a formatted factor for X-axis labels so they display as "+1 d", "+2 d" etc.
# We set levels explicitly to ensure they don't get sorted alphabetically (e.g. 1, 10, 2...)
df_lag$LagLabel <- factor(paste0("+", df_lag$Lag, " d"), 
                          levels = paste0("+", df_lag$Lag, " d"))

# Plot
ggplot(df_lag, aes(x = LagLabel, y = Sharpe)) +
  # 1. Draw bars
  geom_col(fill = "#003f5c", width = 0.6) +
  
  # 2. Titles and Labels
  labs(title = "Execution Lag",
       y = "Sharpe Ratio",
       x = NULL) + # Removing x-label as the tick labels ("+1 d") are self-explanatory
  
  # 3. Y-axis limits (mimics ylim * 1.2)
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  
  # 4. Styling
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# =========================================================
# 4. DIAGNOSTICS: NEIGHBOR DISTANCES
# =========================================================
# Check if we are forcing matches on dissimilar history
analysis_dates <- unique(signals_long$date)
dist_history <- numeric(length(analysis_dates))

for(i in seq_along(analysis_dates)) {
  d <- analysis_dates[i]
  subset_dist <- regime_distances %>% 
    filter(Analysis_Date == d) %>%
    arrange(Distance)
  n_neighbors <- ceiling(nrow(subset_dist) * 0.20)
  dist_history[i] <- mean(head(subset_dist$Distance, n_neighbors))
}

dist_df <- data.frame(Date = analysis_dates, AvgDistance = dist_history)

print(
  ggplot(dist_df, aes(x = Date, y = AvgDistance)) +
    geom_line(color = "#003f5c", size = 0.6) +
    labs(title = "Euclidian Distance to Nearest Neighbors",
         y = NULL) +
    theme_minimal()
)
