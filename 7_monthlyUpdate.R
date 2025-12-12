# =========================================================
# SCRIPT 7: MONTHLY PRODUCTION DASHBOARD
# =========================================================
# Purpose: 
# 1. Trade Sheet (Sorted by Expected Return).
# 2. Portfolio Change Report (Last Month vs This Month).
# 3. Regime Visualization (Accurate to your Backtest).

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(gridExtra)

# =========================================================
# 1. VISUALIZATION ENGINE (Global Score)
# =========================================================
# Updates: Removed "Masked Period" to match your actual Backtest logic.

plot_global_score <- function(target_date, regime_df, top_pct = 0.20) {
  
  target_date <- as.Date(target_date)
  
  # 1. Filter Data for this specific date
  current_view <- regime_df %>%
    filter(Analysis_Date == target_date) %>%
    mutate(Neighbor_Date = as.Date(Neighbor_Date)) %>%
    arrange(Neighbor_Date)
  
  if(nrow(current_view) == 0) {
    stop("No regime data found for this date.")
  }
  
  # 2. Identify "Similar Months" (Selection)
  # We select the Top N% closest neighbors from the FULL history (matching Script 4)
  n_select <- ceiling(nrow(current_view) * top_pct)
  
  selected_neighbors <- current_view %>%
    arrange(Distance) %>%
    head(n_select) %>%
    pull(Neighbor_Date)
  
  # 3. Plotting
  # Create blue bands for neighbors
  rects_df <- data.frame(
    xmin = selected_neighbors,
    xmax = selected_neighbors + months(1),
    ymin = -Inf,
    ymax = Inf
  )
  
  p <- ggplot(current_view, aes(x = Neighbor_Date, y = Distance)) +
    # A. The Blue Bands (Selected Regimes)
    geom_rect(data = rects_df, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill = "lightblue", alpha = 0.8, inherit.aes = FALSE) +
    
    # B. The Global Score Line
    geom_line(color = "#003f5c", size = 1) +
    
    # C. Formatting
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(title = paste("Regime Similarity for:", format(target_date, "%b %Y")),
         subtitle = paste("Dashed Line = Euclidean Distance. Blue Bands = Top", 
                          top_pct*100, "% Nearest Neighbors."),
         y = "Euclidean Distance", x = NULL) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  return(p)
}

# =========================================================
# 2. PORTFOLIO CHANGE REPORT (New Feature)
# =========================================================
# Compares Last Month's Weights vs This Month's Target

get_portfolio_change_report <- function(analysis_date, signals_df, weights_hist) {
  
  analysis_date <- as.Date(analysis_date)
  
  # --- Step A: Get TARGET Weights (Next Month) ---
  # Re-calculate allocation (Top 5 Logic)
  todays_signals <- signals_df %>%
    filter(date == analysis_date) %>%
    mutate(
      Exp_Return_Pct = expected_return, # Keep numeric for sorting
      Win_Rate_Pct   = win_rate 
    )
  
  # Allocation Logic (Copy from Script 5 Top 5)
  MAX_SINGLE <- 0.20
  MAX_BOND   <- 0.40
  bond_tickers <- c("JPEICORE", "GBIE1001", "BCEX6T", "BCEX4T", "IDCOT7", "BEIG1T", "LECPTREU", "IBOXXMJA")
  
  target_w <- setNames(rep(0, nrow(todays_signals)), todays_signals$asset)
  candidates <- todays_signals %>% filter(expected_return > 0) %>% arrange(desc(expected_return))
  
  total_exp <- 0; bond_exp <- 0
  
  if(nrow(candidates) > 0) {
    for(j in 1:nrow(candidates)) {
      if(total_exp >= 1.0) break
      nm <- candidates$asset[j]
      is_bond <- nm %in% bond_tickers
      room_total <- 1.0 - total_exp
      room_class <- if(is_bond) MAX_BOND - bond_exp else 1.0
      alloc <- min(MAX_SINGLE, room_class, room_total)
      if(alloc > 0.001) {
        target_w[nm] <- alloc
        total_exp <- total_exp + alloc
        if(is_bond) bond_exp <- bond_exp + alloc
      }
    }
  }
  
  # --- Step B: Get PREVIOUS Weights (Last Month) ---
  hist_dates <- sort(as.Date(names(weights_hist)))
  prev_date_idx <- which(hist_dates == analysis_date) - 1
  
  if (length(prev_date_idx) > 0 && prev_date_idx > 0) {
    prev_date <- hist_dates[prev_date_idx]
    current_w_vec <- weights_hist[[as.character(prev_date)]]
  } else {
    current_w_vec <- setNames(rep(0, length(target_w)), names(target_w))
    prev_date <- "Start"
  }
  
  # --- Step C: Merge & Format ---
  # Create Dataframes
  df_target  <- tibble(Asset = names(target_w), Target_Weight = target_w)
  df_current <- tibble(Asset = names(current_w_vec), Previous_Weight = current_w_vec)
  
  # Full Join
  report <- full_join(df_target, df_current, by = "Asset") %>%
    left_join(todays_signals %>% select(asset, Exp_Return_Pct, Win_Rate_Pct), by = c("Asset" = "asset")) %>%
    mutate(
      Target_Weight   = coalesce(Target_Weight, 0),
      Previous_Weight = coalesce(Previous_Weight, 0),
      Change          = Target_Weight - Previous_Weight
    ) %>%
    # Filter: Show only assets that are involved (Prev > 0 OR Target > 0)
    filter(Target_Weight > 0 | Previous_Weight > 0) %>%
    arrange(desc(Target_Weight), desc(Previous_Weight)) %>%
    transmute(
      Asset = Asset,
      `Last Month` = sprintf("%.1f%%", Previous_Weight * 100),
      `This Month` = sprintf("%.1f%%", Target_Weight * 100),
      Change       = sprintf("%+.1f%%", Change * 100),
      `Exp. Return` = sprintf("%.2f%%", Exp_Return_Pct * 100),
      `Confidence`  = sprintf("%.0f%%", Win_Rate_Pct * 100)
    )
  
  return(report)
}

# =========================================================
# 3. TRADE SHEET GENERATOR (Sorted by Return)
# =========================================================

get_trade_sheet <- function(analysis_date, signals_df, weights_hist) {
  
  # Reuse the logic from the report function to get the raw data
  # (Simulating calling the logic again for the Trade Sheet specifically)
  
  report_raw <- get_portfolio_change_report(analysis_date, signals_df, weights_hist)
  
  # We re-parse the strings or just calculate fresh for the Trade Sheet
  # To keep it simple/robust, let's re-run the merged calculation quickly
  
  # ... [Repeat Alloc Logic - omitted for brevity, using the result from above] ...
  # Actually, let's just use the 'report' we just built, but change the sorting!
  
  # Re-generating raw data for sorting
  analysis_date <- as.Date(analysis_date)
  
  # Get Signals
  todays_signals <- signals_df %>%
    filter(date == analysis_date) %>%
    select(asset, expected_return, win_rate)
  
  # Get Weights (Using the Change Report Logic wrapper)
  # NOTE: In production, optimize this to not run twice. Here clarity > speed.
  
  # --- Quick Calculation for Trade Sheet ---
  # 1. Target
  # (Top 5 Logic again)
  MAX_SINGLE <- 0.20; MAX_BOND <- 0.40
  bond_tickers <- c("JPEICORE", "GBIE1001", "BCEX6T", "BCEX4T", "IDCOT7", "BEIG1T", "LECPTREU", "IBOXXMJA")
  
  target_w <- setNames(rep(0, nrow(todays_signals)), todays_signals$asset)
  candidates <- todays_signals %>% filter(expected_return > 0) %>% arrange(desc(expected_return))
  total_exp <- 0; bond_exp <- 0
  
  if(nrow(candidates) > 0) {
    for(j in 1:nrow(candidates)) {
      if(total_exp >= 1.0) break
      nm <- candidates$asset[j]
      is_bond <- nm %in% bond_tickers
      room_total <- 1.0 - total_exp
      room_class <- if(is_bond) MAX_BOND - bond_exp else 1.0
      alloc <- min(MAX_SINGLE, room_class, room_total)
      if(alloc > 0.001) {
        target_w[nm] <- alloc
        total_exp <- total_exp + alloc
        if(is_bond) bond_exp <- bond_exp + alloc
      }
    }
  }
  
  # 2. Current
  hist_dates <- sort(as.Date(names(weights_hist)))
  prev_date_idx <- which(hist_dates == analysis_date) - 1
  if (length(prev_date_idx) > 0 && prev_date_idx > 0) {
    current_w <- weights_hist[[as.character(hist_dates[prev_date_idx])]]
  } else {
    current_w <- setNames(rep(0, length(target_w)), names(target_w))
  }
  
  # 3. Combine
  trade_sheet <- tibble(Asset = names(target_w), Target = target_w) %>%
    left_join(tibble(Asset = names(current_w), Current = current_w), by="Asset") %>%
    left_join(todays_signals, by=c("Asset"="asset")) %>%
    mutate(
      Current = coalesce(Current, 0),
      Target = coalesce(Target, 0),
      Net_Change = Target - Current,
      Action = case_when(
        Net_Change > 0.001 ~ "BUY",
        Net_Change < -0.001 ~ "SELL",
        TRUE ~ "HOLD"
      )
    ) %>%
    # Sorting Requirement: Sort by Expected Return (Desc)
    arrange(desc(expected_return)) %>% 
    transmute(
      Asset,
      Action,
      `Trade Size` = sprintf("%+.1f%%", Net_Change * 100),
      `Target Alloc` = sprintf("%.1f%%", Target * 100),
      `Current Alloc` = sprintf("%.1f%%", Current * 100),
      `Exp. Return` = sprintf("%.2f%%", expected_return * 100),
      `Confidence` = sprintf("%.0f%%", win_rate * 100)
    )
  
  return(trade_sheet)
}

# =========================================================
# 4. EXECUTION
# =========================================================

# Set Date
latest_analysis_date <- max(signals_long$date)
cat(paste("\n=== MONTHLY UPDATE FOR:", format(latest_analysis_date, "%B %Y"), "===\n"))

# 1. Chart
p_latest <- plot_global_score(latest_analysis_date, regime_distances)
print(p_latest)

# 2. Portfolio Comparison Table

cat("\n--- PORTFOLIO CHANGE REPORT (Last Month vs This Month) ---\n")

change_report <- get_portfolio_change_report(latest_analysis_date, signals_long, weights_history)
print(as.data.frame(change_report), row.names = FALSE)

# 3. Trade Sheet
cat("\n--- TRADE SHEET (Sorted by Exp. Return) ---\n")
trades <- get_trade_sheet(latest_analysis_date, signals_long, weights_history)
print(as.data.frame(trades), row.names = FALSE)

# 4. Specific Date Inspector (Optional)
inspect_historical_date <- function(check_date_str) {
  check_date <- as.Date(check_date_str)
  cat(paste("\n\n=== DEEP DIVE REPORT:", check_date_str, "===\n"))
  
  # Plot
  print(plot_global_score(check_date, regime_distances))
  
  # Nearest Neighbors Table
  regime_view <- regime_distances %>% filter(Analysis_Date == check_date) %>% arrange(Distance)
  n_select <- ceiling(nrow(regime_view) * 0.20)
  
  cat("\n--- Top Neighbors (Used for Signal) ---\n")
  print(head(regime_view %>% select(Neighbor_Date, Distance), 10))
}

inspect_historical_date("2016-03-01")
