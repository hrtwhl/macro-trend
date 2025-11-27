# =========================================================
# FORENSIC DIAGNOSTICS: WHY IS SR > 1.8?
# =========================================================

# 1. CURRENCY CHECK (The Inflation Trap)
# We assume 'assets_long' is loaded.
# Let's compare the Cumulative Return of your assets vs the Benchmark.
# If Turkey (MXTR) or similar is up 2000%, it's likely Local Currency.

asset_cum_ret <- assets_long %>%
  arrange(asset, date) %>%
  group_by(asset) %>%
  summarise(
    total_return_pct = (last(price) / first(price) - 1) * 100,
    start_date = min(date),
    currency_guess = "Unknown" 
  ) %>%
  arrange(desc(total_return_pct))

print("--- TOP PERFORMING ASSETS (Check for Hyperinflation Returns) ---")
print(asset_cum_ret)

# 2. LOOK-AHEAD BIAS CHECK (The "Peeking" Trap)
# We check the correlation between Today's Signal and Today's Return.
# In a real system, Signal(T-1) should predict Return(T).
# If Signal(T) is correlated with Return(T), you are peeking.

# Get the daily signal (weights) and daily returns
# Align them
check_df <- merge(strat_xts, bench_ret, join = "inner")
check_df$Signal_Change <- diff(check_df[,1]) # Did exposure change?

# Is high return correlated with buying?
# We look at the Portfolio Weights vs Next Day Return
# (This requires reconstructing the weight XT object from Script 5)
weight_xts_check <- xts(weight_matrix, order.by = all_dates)
weight_xts_check <- weight_xts_check[index(check_df)]

# Calculate Portfolio Beta to Benchmark over time
beta_roll <- rollapply(check_df, width = 60,
                       function(x) cov(x[,1], x[,2])/var(x[,2]),
                       by.column = FALSE)

plot(beta_roll, main = "Rolling Beta of Strategy (Is it avoiding risks magically?)")

# 3. THE 2008 'MAGIC' CHECK
# Let's look at the exact weights during the Lehman Brothers crash (Sept 2008).
# Did it go to cash BEFORE the crash?
crash_date <- as.Date("2008-09-15")
crash_idx  <- which.min(abs(index(weight_xts_check) - crash_date))

print(paste("Allocation around Lehman Crash (", crash_date, "):"))
print(weight_xts_check[(crash_idx-2):(crash_idx+2), ])
