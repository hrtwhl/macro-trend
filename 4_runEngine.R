# =========================================================
# 4_runEngine.R (CORRECTED WITH HEIDEN FILTER)
# =========================================================
# PURPOSE:
# 1. Calculate Forward Returns
# 2. Run Nearest Neighbor Search
# 3. Generate Signals with STRICTER Logic (Hit Ratio > 0.5)
# =========================================================

# ---- Packages ----
library(tidyverse)
library(furrr)

# ---- Load Data ----
# master_data <- readRDS("master_data_features.rds")

# =========================================================
# 1. PREP: CALCULATE FORWARD RETURNS
# =========================================================

engine_data <- master_data %>%
  group_by(asset) %>%
  arrange(date) %>%
  mutate(
    ret_1m_fwd = lead(price, 1) / price - 1
  ) %>%
  ungroup() %>%
  filter(!is.na(date))

# =========================================================
# 2. THE ENGINE FUNCTIONS
# =========================================================

get_distances <- function(current_vec, history_mat) {
  diffs <- sweep(history_mat, 2, current_vec, "-")
  sqrt(rowSums(diffs^2))
}

run_asset_engine <- function(chunk_data, n_neighbors = 15, min_history = 48) {
  
  # A. Feature Selection
  feature_cols <- c(
    "mom_12m", "vol_12m", "dd_2y",       # Price
    "z_spx", "z_china", "z_cpi",         # Macro
    "z_usdem", "z_yield10", "z_spread", "z_vix"
  )
  
  chunk_data <- chunk_data %>% arrange(date)
  feat_matrix <- chunk_data %>% select(all_of(feature_cols)) %>% as.matrix()
  dates       <- chunk_data$date
  fwd_rets    <- chunk_data$ret_1m_fwd
  n_rows      <- nrow(chunk_data)
  
  results_list <- list()
  
  if (n_rows <= min_history) return(NULL)
  
  # B. Walk Forward Loop
  for (i in (min_history + 1):n_rows) {
    
    current_date <- dates[i]
    current_feat <- feat_matrix[i, ]
    
    # History: Strictly BEFORE current month
    history_idx  <- 1:(i-1)
    hist_feats   <- feat_matrix[history_idx, , drop = FALSE]
    hist_fwds    <- fwd_rets[history_idx]
    
    # 1. Distance & Neighbors
    dists <- get_distances(current_feat, hist_feats)
    k_adj <- min(n_neighbors, length(dists))
    nearest_indices <- order(dists)[1:k_adj]
    
    # 2. Analogue Outcomes
    analogue_returns <- hist_fwds[nearest_indices]
    
    # 3. Stats
    exp_return <- mean(analogue_returns, na.rm = TRUE)
    hit_ratio  <- sum(analogue_returns > 0, na.rm = TRUE) / k_adj
    
    # 4. SIGNAL GENERATION (THE FIX)
    # Old Logic: if exp_return > 0 -> Alive
    # New Logic (Heiden Filter): 
    # Must be Positive Expectancy (>0) AND Consistent (>50% Win Rate)
    
    signal_state <- if_else(exp_return > 0 & hit_ratio > 0.50, 1, 0)
    
    results_list[[i]] <- tibble(
      date = current_date,
      signal = signal_state,
      exp_return = exp_return,
      hit_ratio = hit_ratio,
      n_analogues = k_adj
    )
  }
  
  bind_rows(results_list) %>%
    mutate(asset = chunk_data$asset[1])
}

# =========================================================
# 3. EXECUTION
# =========================================================

print("Starting Engine with Heiden Filter (Hit Ratio > 0.50)...")

final_signals <- engine_data %>%
  group_split(asset) %>%
  map_dfr(~ run_asset_engine(.x, n_neighbors = 15, min_history = 48))

print("Engine Complete. New Signal Distribution:")
print(table(final_signals$signal))

# saveRDS(final_signals, "final_signals.rds")

