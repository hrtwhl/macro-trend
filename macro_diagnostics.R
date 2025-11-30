# =========================================================
# SCRIPT 2b: MACRO DIAGNOSTICS (REPLICATING EXHIBITS 5 & 6)
# =========================================================
# Purpose: 
# 1. Transform raw macro data to match the Paper's input (12m Change -> Z-Score).
# 2. Replicate Exhibit 5 (Autocorrelations & Descriptive Stats).
# 3. Replicate Exhibit 6 (Correlation Heatmap).

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(zoo) # For rolling functions

# =========================================================
# 1. PREPARE DATA (Monthly + Transformations)
# =========================================================

# A. Aggregate to Monthly (Last observation of the month)
macro_monthly <- macro_wide %>%
  mutate(month_date = floor_date(date, "month")) %>%
  group_by(month_date) %>%
  summarise(across(where(is.numeric), ~ last(na.omit(.))), .groups = "drop") %>%
  arrange(month_date)

# B. Calculate 12-Month Changes (The "Regime" Transformation)
# We must apply the exact logic from Script 3 to analyze the actual model inputs.
macro_trans <- macro_monthly %>%
  mutate(
    # Log Returns for Levels (Prices/Indices)
    SPX_chg         = log(SPX / lag(SPX, 12)),
    cpi_chg         = log(cpi_headline / lag(cpi_headline, 12)),
    usd_eur_chg     = log(usd_eur_rate / lag(usd_eur_rate, 12)),
    
    # Absolute Change for Rates/Spreads/Indices
    china_cli_chg   = china_cli - lag(china_cli, 12),
    spread_chg      = term_spread_10y3m - lag(term_spread_10y3m, 12),
    yield_chg       = yield_10y - lag(yield_10y, 12),
    vix_chg         = vix_index - lag(vix_index, 12)
  ) %>%
  select(month_date, ends_with("_chg")) %>%
  drop_na() # Remove the first 12 months


# C. Normalize (Paper Style: Scale by Volatility ONLY)
# The paper says: "divide the one-year difference by the standard deviation" [Source: 195]
# They do NOT subtract the mean, which preserves the positive drift of equities.

paper_z_score <- function(x) { 
  # We assume the standard deviation of the full sample for this diagnostic table
  # (In the live model, it's rolling, but for this table, we just want to match the logic)
  x / sd(x, na.rm=TRUE) 
}

macro_z_diag <- macro_trans %>%
  mutate(across(ends_with("_chg"), paper_z_score))



# =========================================================
# 2. REPLICATE EXHIBIT 5 (Descriptive Stats & Autocorr)
# =========================================================

calc_exhibit5_stats <- function(x) {
  # Autocorrelations (handle short history if 10y is not avail)
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
    axis.ticks = element_blank(),
    #legend.justification = c(1, 0),
    #legend.position = c(0.6, 0.7),
    #legend.direction = "horizontal"
  ) +
  coord_fixed() +
  labs(title = "Exhibit 6: Correlation of Economic State Variables",
       subtitle = "Cross-correlations of 12-month changes (Z-scored)")

print(exhibit_6_plot)
