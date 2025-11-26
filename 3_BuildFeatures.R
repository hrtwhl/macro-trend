# =========================================================
# 3_buildFeatures.R
# =========================================================
# PURPOSE:
# 1. Harmonize Assets and Macro to Monthly Frequency
# 2. Calculate Price Features (Mom, Vol, DD)
# 3. Calculate Macro Features (Rolling Z-Scores)
# 4. Create the Master "Fingerprint" Table
# =========================================================

# ---- Packages ----
library(tidyverse)
library(tidyquant)
library(zoo)        # For rolling functions
library(timetk)     # For time series tools
library(lubridate)  # For date manipulation

# ---- Load Data ----
# Ensure you have 'assets_long' and 'macro_wide' in your environment
# If not, run scripts 1 and 2, or load them:
# assets_long <- readRDS("assets_long.rds") 
# macro_wide  <- readRDS("macro_wide.rds")

# =========================================================
# 1. PREPARE ASSETS (Target Variables)
# =========================================================

# A. Calculate Monthly Realized Volatility from Daily data
asset_vol_monthly <- assets_long %>%
  group_by(asset) %>%
  tq_transmute(
    select = price,
    mutate_fun = apply.monthly,
    FUN = sd,
    na.rm = TRUE,
    col_rename = "mth_std_dev"
  ) %>%
  # FIX: Use lubridate to force the date to the last calendar day of the month
  mutate(date = ceiling_date(as.Date(date), "month") - days(1)) %>% 
  ungroup()

# B. Aggregate Price to Monthly (Last Price)
asset_mth_price <- assets_long %>%
  group_by(asset) %>%
  tq_transmute(
    select = price,
    mutate_fun = to.monthly,
    indexAt = "lastof",
    col_rename = "price"
  ) %>%
  ungroup() %>%
  # FIX: Ensure this also aligns to the last calendar day so the JOIN works perfectly
  mutate(date = ceiling_date(as.Date(date), "month") - days(1))

# C. Merge and Calculate Price Features
assets_features <- asset_mth_price %>%
  left_join(asset_vol_monthly, by = c("date", "asset")) %>%
  group_by(asset) %>%
  arrange(date) %>%
  mutate(
    # 1. Momentum: 12-month ROC
    mom_12m = price / lag(price, 12) - 1,

    # 2. Volatility: Annualized (Monthly SD * sqrt(12))
    vol_12m = rollapply(mth_std_dev, width = 12, FUN = mean, align = "right", fill = NA) * sqrt(252),

    # 3. Drawdown: % off the rolling 2-Year (24 month) High
    rolling_high_24m = rollapply(price, width = 24, FUN = max, align = "right", fill = NA),
    dd_2y = (price / rolling_high_24m) - 1
  ) %>%
  ungroup() %>%
  select(date, asset, price, mom_12m, vol_12m, dd_2y) %>%
  drop_na()

# =========================================================
# 2. PREPARE MACRO (The "Regime" Map)
# =========================================================

# Helper: Rolling Z-Score Function (Winsorized to +/- 3)
calc_rolling_z <- function(x, window = 60) {
  # Rolling Mean and SD
  r_mean <- rollapply(x, width = window, FUN = mean, align = "right", fill = NA)
  r_sd   <- rollapply(x, width = window, FUN = sd,   align = "right", fill = NA)
  
  # Z-Score
  z <- (x - r_mean) / r_sd
  
  # Winsorize (Cap at +/- 3)
  z <- pmin(pmax(z, -3), 3)
  return(z)
}

macro_features <- macro_wide %>%
  # 1. Resample to Monthly (Last Observation)
  mutate(date = as.Date(date)) %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise(across(where(is.numeric), ~ last(.x, na_rm = TRUE))) %>%
  mutate(date = ceiling_date(month, "month") - days(1)) %>% # Force end-of-month date
  select(-month) %>%
  arrange(date) %>%
  
  # 2. Lag "Slow" Reporting Variables (Avoid Look-ahead bias)
  mutate(
    # Assuming 'cpi_headline' and 'china_cli' are in your macro_wide
    # Adjust names if they differ in your environment
    cpi_headline = lag(cpi_headline, 1),
    china_cli    = lag(china_cli, 1)
  ) %>%
  
  # 3. Calculate Raw Variations (YoY Changes, etc.)
  mutate(
    spx_yoy       = SPX / lag(SPX, 12) - 1,
    cpi_yoy       = cpi_headline / lag(cpi_headline, 12) - 1,
    china_cli_yoy = china_cli / lag(china_cli, 12) - 1,
    usd_em_yoy    = usd_em_index / lag(usd_em_index, 12) - 1,
    yield_10y_chg = yield_10y - lag(yield_10y, 12) 
    # VIX and Spread: We use Levels
  ) %>%
  
  # 4. Calculate Rolling Z-Scores (The "Fingerprint")
  #    Using a 5-Year (60 month) rolling window
  mutate(
    z_spx       = calc_rolling_z(spx_yoy),
    z_china     = calc_rolling_z(china_cli_yoy),
    z_cpi       = calc_rolling_z(cpi_yoy),
    z_usdem     = calc_rolling_z(usd_em_yoy),
    z_yield10   = calc_rolling_z(yield_10y_chg),
    z_spread    = calc_rolling_z(term_spread_10y2y),
    z_vix       = calc_rolling_z(vix_index)
  ) %>%
  select(date, starts_with("z_")) %>%
  drop_na()

# =========================================================
# 3. MASTER JOIN
# =========================================================

master_data <- assets_features %>%
  inner_join(macro_features, by = "date") %>%
  arrange(asset, date)

# Print success message
print(paste("Master Data created with", nrow(master_data), "rows."))
print(head(master_data))




# Check 1: Do we have any NAs left in the critical columns?
na_check <- master_data %>% 
  summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>% 
  filter(na_count > 0)

print("--- Columns with NAs ---")
print(na_check)

# Check 2: Count months per asset (Do we have roughly the same history?)
count_check <- master_data %>% 
  count(asset, name = "months_of_history") %>% 
  arrange(months_of_history)

print("--- History Length per Asset ---")
print(count_check)
