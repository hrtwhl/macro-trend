# =========================================================
# SCRIPT 2b: MACRO DIAGNOSTICS (REPLICATING EXHIBITS 5 & 6)
# =========================================================
# Purpose: 
# 1. Transform raw macro data (12m Change -> Standard Rolling Z-Score).
# 2. Replicate Exhibit 5 (Autocorrelations & Descriptive Stats).
# 3. Replicate Exhibit 6 (Correlation Heatmap).

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(zoo) # For rolling functions
library(purrr)

# =========================================================
# 1. PREPARE DATA (Monthly + Transformations)
# =========================================================

# A. Aggregate to Monthly (Last observation of the month)
macro_monthly <- macro_wide %>%
  mutate(month_date = floor_date(date, "month")) %>%
  group_by(month_date) %>%
  summarise(across(where(is.numeric), ~ last(na.omit(.))), .groups = "drop") %>%
  arrange(month_date)

# B. DEFINE ROLLING Z-SCORE FUNCTION (Standard Z-Score)
# Methodology:
# 1. Calculate 12-month change.
# 2. Calculate Rolling Mean AND Rolling SD (Min 3 years, Max 10 years).
# 3. Compute Z = (Value - Rolling Mean) / Rolling SD.
# 4. Winsorize at +/- 3.

calculate_rolling_z_score <- function(raw_series, is_log_return = FALSE) {
  
  # 1. Calculate 12-month Change
  d12 <- rep(NA, length(raw_series))
  
  if (length(raw_series) > 12) {
    if (is_log_return) {
      d12[13:length(raw_series)] <- diff(log(raw_series), lag = 12)
    } else {
      d12[13:length(raw_series)] <- diff(raw_series, lag = 12)
    }
  }
  
  # 2. Calculate Rolling Statistics (Min 3y / Max 10y)
  n <- length(d12)
  rolling_mean <- rep(NA, n)
  rolling_sd   <- rep(NA, n)
  
  # Min 3 years (36 obs) of valid 12m diffs required
  min_obs <- 36 
  max_obs <- 120
  
  # Start calc at index where we have enough history (12 lags + 36 obs)
  start_calc_index <- 12 + min_obs + 1
  
  if (n >= start_calc_index) {
    for (i in start_calc_index:n) {
      # Lookback window logic (Expanding then Rolling)
      window_start <- max(13, i - max_obs + 1)
      window_data  <- d12[window_start:i]
      
      # Calculate BOTH Mean and SD for standard Z-score
      rolling_mean[i] <- mean(window_data, na.rm = TRUE)
      rolling_sd[i]   <- sd(window_data, na.rm = TRUE)
    }
  }
  
  # 3. Compute Standard Z-Score: (X - Mean) / SD
  score <- (d12 - rolling_mean) / rolling_sd
  
  # 4. Winsorize at +/- 3
  final_score <- pmax(pmin(score, 3), -3)
  
  return(final_score)
}

# C. Apply Transformation to All Variables
macro_z_diag <- macro_monthly %>%
  transmute(
    month_date = month_date,
    
    # 1. Log Return Variables (Prices/Indices)
    SPX_chg    = calculate_rolling_z_score(SPX, is_log_return = TRUE),
    oil_chg    = calculate_rolling_z_score(oil, is_log_return = TRUE),
    copper_chg = calculate_rolling_z_score(copper, is_log_return = TRUE),
    dollar_chg = calculate_rolling_z_score(dollar_index, is_log_return = TRUE),
    
    # 2. Absolute Change Variables (Rates/Spreads)
    spread_chg = calculate_rolling_z_score(term_spread_10y3m, is_log_return = FALSE),
    yield_chg  = calculate_rolling_z_score(yield_10y, is_log_return = FALSE), 
    vix_chg    = calculate_rolling_z_score(vix_index, is_log_return = FALSE)
  ) %>%
  drop_na() # Remove rows where the rolling window wasn't full enough yet

# =========================================================
# 2. REPLICATE EXHIBIT 5 (Descriptive Stats & Autocorr)
# =========================================================

calc_exhibit5_stats <- function(x) {
  # Autocorrelations
  acf_vals <- acf(x, plot = FALSE, lag.max = 120)$acf
  
  get_lag <- function(l) if(l < length(acf_vals)) round(acf_vals[l+1], 2) else NA
  
  tibble(
    `1 month`  = get_lag(1),
    `3 month`  = get_lag(3),
    `12 month` = get_lag(12),
    `3 year`   = get_lag(36),
    `10 year`  = get_lag(120),
    `mean`     = round(mean(x, na.rm=TRUE), 2),
    `std`      = round(sd(x, na.rm=TRUE), 2),
    `freq`     = "monthly"
  )
}

# Calculate for all columns
exhibit_5 <- purrr::map_dfr(names(macro_z_diag)[-1], function(col_name) {
  stats <- calc_exhibit5_stats(macro_z_diag[[col_name]])
  stats %>% mutate(Variable = col_name, .before = 1)
})

cat("\n--- EXHIBIT 5: PERSISTENCE OF STATE VARIABLES ---\n")
print(as.data.frame(exhibit_5))

# =========================================================
# 3. REPLICATE EXHIBIT 6 (Correlation Matrix)
# =========================================================

# Calculate Correlation Matrix
cor_mat <- cor(macro_z_diag %>% select(-month_date), use = "pairwise.complete.obs")

# Format for Heatmap (Long format)
cor_melt <- as.data.frame(as.table(cor_mat))
names(cor_melt) <- c("Var1", "Var2", "Correlation")

# Plot
exhibit_6_plot <- ggplot(cor_melt, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#2c7bb6", high = "#d7191c", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 3) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_fixed() +
  labs(title = "Correlation of Economic State Variables",
       subtitle = "Standard Rolling Z-Scores")

print(exhibit_6_plot)
