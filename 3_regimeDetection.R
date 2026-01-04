# =========================================================
# SCRIPT 3: REGIME DETECTION (THE ENGINE)
# =========================================================
# Purpose: 
# 1. Transform macro variables (12m Change -> Rolling Z-Score)
# 2. Calculate "Similarity" (Euclidean Distance) for every historical month
# 3. Output the "Nearest Neighbors" for each point in time

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(zoo) # for rolling functions

# =========================================================
# 1. CONFIGURATION
# =========================================================
# Variable Selection (The "Optimal 7" we decided on)
# Ensure these exact names exist in your 'macro_wide'
SELECTED_VARS <- c("SPX", "china_cli", "dollar_index", 
                   "cpi_headline", "term_spread_10y3m", 
                   "yield_10y", "vix_index")

SELECTED_VARS <- c("SPX", "dollar_index", 
                   "term_spread_10y3m", 
                   "yield_10y", "vix_index", "oil", "copper")



names(macro_wide)
# Parameters from Paper / Adaptation
LOOKBACK_WINDOW_MONTHS <- 120 # 10 Years (Paper standard)
MIN_HISTORY_MONTHS     <- 36  # Warmup period (To start trading in 2003, not 2010)
WINSORIZE_LIMIT        <- 3   # Cap Z-scores at +/- 3 (Paper standard)

# =========================================================
# 2. DATA PREPARATION
# =========================================================
# We assume 'macro_wide' is loaded from Script 2. 
# We need monthly granularity (Last observation of the month).

macro_monthly_raw <- macro_wide %>%
  arrange(date) %>%
  tidyr::fill(all_of(SELECTED_VARS), .direction = "down") %>%
  #mutate(month_date = floor_date(date, "month")) %>%
  mutate(month_date = ceiling_date(date, "month") - days(1)) %>% 
  group_by(month_date) %>%
  summarise(across(all_of(SELECTED_VARS), dplyr::last), .groups = "drop") %>%
  drop_na() %>%
  arrange(month_date)

tail(macro_monthly_raw$month_date, 1)
#View(macro_monthly_raw)
# =========================================================
# 3. TRANSFORMATION (12-Month Change)
# =========================================================
# Paper: "For each variable, we take a 12-month change" 

macro_chg <- macro_monthly_raw %>%
  mutate(
    # Log Returns for Levels (Prices/Indices)
    SPX_chg      = log(SPX / lag(SPX, 12)),
    oil_chg   = log(oil / lag(oil, 12)),
    copper_chg   = log(copper / lag(copper, 12)),
    dollar_chg   = log(dollar_index / lag(dollar_index, 12)),
    
    # Absolute Change for Rates/Spreads
    spread_chg   = term_spread_10y3m - lag(term_spread_10y3m, 12),
    yield_chg    = yield_10y - lag(yield_10y, 12),
    vix_chg      = vix_index - lag(vix_index, 12)
  ) %>%
  select(month_date, ends_with("_chg")) %>%
  drop_na() # Drop the first 12 months of NAs



# =========================================================
# 4. NORMALIZATION (Rolling/Expanding Z-Score)
# =========================================================
# Paper: "normalize it by computing the z-score over a rolling 10 years" 
# Adaptation: We use an expanding window from month 36 to 120, then rolling 120.

calc_hybrid_zscore <- function(x, min_per = 36, max_per = 120) {
  # This function calculates Z-score using ONLY past data (No look-ahead bias)
  z_scores <- numeric(length(x))
  z_scores[1:(min_per-1)] <- NA # Not enough history
  
  for (i in min_per:length(x)) {
    # Determine window start
    start_idx <- max(1, i - max_per + 1)
    window_data <- x[start_idx:i]
    
    # Calculate Stats
    mu <- mean(window_data, na.rm = TRUE)
    sigma <- sd(window_data, na.rm = TRUE)
    
    # Z-Score for the CURRENT observation (i)
    # Paper: Winsorize at +/- 3
    val <- (x[i] - mu) / sigma
    if (!is.na(val)) {
      if (val > 3) val <- 3
      if (val < -3) val <- -3
    }
    z_scores[i] <- val
  }
  return(z_scores)
}

# Apply to all columns

macro_z <- macro_chg %>%
  mutate(across(ends_with("_chg"), ~ calc_hybrid_zscore(., MIN_HISTORY_MONTHS, LOOKBACK_WINDOW_MONTHS))) %>%
  drop_na() # Drop the warmup period

# =========================================================
# 5. SIMILARITY ENGINE (Euclidean Distance)
# =========================================================
# Paper: "calculate a sum of squares... aggregated similarity score" [cite: 351-354]

# We need to iterate through time. For each month T, compare to t=1...T-1
# We will store the results in a list of "Nearest Neighbors"

# Helper function to calculate distance between row T and all rows 1:(T-1)
calculate_distances <- function(df_z) {
  
  # Prepare output container
  # We will store: Date, Neighbors_Dates, Neighbors_Distances
  results <- list()
  
  # Matrix for speed
  mat_data <- as.matrix(df_z %>% select(-month_date))
  dates <- df_z$month_date
  
  n_rows <- nrow(mat_data)
  
  # Start loop. We need at least a few months of history to find a "neighbor"
  # Let's start after 12 months of valid Z-scores
  start_sim <- 12 
  
  for (i in start_sim:n_rows) {
    
    current_date <- dates[i]
    current_vec <- mat_data[i, , drop = FALSE]
    
    # History: All rows strictly BEFORE i
    history_mat <- mat_data[1:(i-1), , drop = FALSE]
    history_dates <- dates[1:(i-1)]
    
    # Euclidean Distance: sqrt(sum((x - y)^2))
    # We leverage R's rowSums for speed
    diff_sq <- sweep(history_mat, 2, current_vec, "-")^2
    dists <- sqrt(rowSums(diff_sq))
    
    # Store Raw Distances (We will filter/sort in the Strategy Step)
    # We store a tibble for this specific date
    results[[i]] <- tibble(
      Analysis_Date = current_date,
      Neighbor_Date = history_dates,
      Distance = dists
    )
  }
  
  # Combine all history into one massive long DF
  bind_rows(results)
}

cat("Calculating Regime Distances... this may take a moment.\n")
regime_distances <- calculate_distances(macro_z)

# =========================================================
# 6. OUTPUT & DIAGNOSTICS
# =========================================================

# Check 1: Do we have data?
cat("Total Regime Comparisons Calculated:", nrow(regime_distances), "\n")

# Check 2: Look at a specific date (e.g., COVID Crash or Recent)
# Let's see what the model thought was "similar" to March 2020 (if in sample)
check_date <- as.Date("2020-03-01")
if (check_date %in% regime_distances$Analysis_Date) {
  cat("\nTop 5 Regimes most similar to:", as.character(check_date), "\n")
  
  regime_distances %>%
    filter(Analysis_Date == check_date) %>%
    arrange(Distance) %>%
    head(5) %>%
    print()
}

# Check 3: Look at the most recent available date
last_date <- max(regime_distances$Analysis_Date)
cat("\nTop 5 Regimes most similar to NOW (", as.character(last_date), "):\n")

regime_distances %>%
  filter(Analysis_Date == last_date) %>%
  arrange(Distance) %>%
  head(5) %>%
  print()

# Save the object for Script 4
# saveRDS(regime_distances, "regime_distances.rds") 
# saveRDS(macro_z, "macro_z_scores.rds")


