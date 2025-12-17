# =========================================================
# SCRIPT 6: REALISTIC BACKTEST (DRIFT & FILL)
# =========================================================
# Purpose: 
# 1. Sell ONLY assets that leave the Top 5.
# 2. Keep existing assets untouched (let them drift).
# 3. Buy NEW assets using the cash from sales (split equally).
# 4. Pay 50 EUR only for actual Buy/Sell events.

# Dependencies
library(dplyr)
library(xts)
library(PerformanceAnalytics)
library(ggplot2)
library(tidyr)
library(scales)

# =========================================================
# 1. CONFIGURATION & SETUP
# =========================================================

# --- USER INPUTS ---
INITIAL_CAPITAL <- 100000   # Dein Kapital (z.B. 75k)
COST_PER_TRADE  <- 50      # Fixkosten pro Transaktion
BENCHMARK_TICKER <- "MIMUEMRN" # Benchmark Ticker

# Check Dependencies from Script 5
if (!exists("daily_returns_xts") || !exists("weights_history")) {
  stop("FEHLER: Bitte zuerst Script 5 ausführen! 'daily_returns_xts' fehlt.")
}

# --- RE-CREATE BENCHMARK (Safety Check) ---
if (exists("assets_wide")) {
  bench_prices <- na.omit(xts(assets_wide[[BENCHMARK_TICKER]], order.by = assets_wide$date))
  bench_ret <- Return.calculate(bench_prices)
  colnames(bench_ret) <- "Benchmark"
} else if (exists("bench_ret")) {
  # Alles gut
} else {
  stop("Konnte Benchmark nicht finden. Bitte Script 5 (oder 1) laufen lassen.")
}

# =========================================================
# 2. SIMULATION ENGINE (DRIFT & FILL)
# =========================================================

# Prepare Timeline
all_dates <- index(daily_returns_xts)
asset_names <- colnames(daily_returns_xts)

# Storage for Results
nav_curve  <- numeric(length(all_dates))
cost_curve <- numeric(length(all_dates)) 

# Initialize Portfolio State
current_cash     <- INITIAL_CAPITAL
current_holdings <- rep(0, length(asset_names))
names(current_holdings) <- asset_names

cat("Starting Drift & Fill Simulation (Capital:", INITIAL_CAPITAL, "EUR)...\n")

for (i in seq_along(all_dates)) {
  today <- all_dates[i]
  date_char <- as.character(today)
  
  # A. MARKET MOVEMENT (Drift)
  # -------------------------------------------------------
  todays_ret <- daily_returns_xts[today, ]
  todays_ret[is.na(todays_ret)] <- 0
  
  # Update Value
  current_holdings <- current_holdings * (1 + as.numeric(todays_ret))
  
  fees_today <- 0
  
  # B. REBALANCING LOGIC (DRIFT & FILL)
  # -------------------------------------------------------
  if (date_char %in% names(weights_history)) {
    
    # 1. Identify Who is Who
    # ----------------------
    # Target Assets (The "New List" from Script 5)
    target_weights_map <- weights_history[[date_char]]
    target_assets <- names(target_weights_map)[target_weights_map > 0]
    
    # Current Assets (Who we actually hold > 1 EUR)
    held_assets <- names(current_holdings)[current_holdings > 1]
    
    # Define Sets
    leavers   <- setdiff(held_assets, target_assets)   # Sell these
    newcomers <- setdiff(target_assets, held_assets)   # Buy these
    stayers   <- intersect(held_assets, target_assets) # Keep these (Drift)
    
    # Special Case: Initial Start (All are newcomers)
    if (sum(current_holdings) == 0 && length(target_assets) > 0) {
      newcomers <- target_assets
    }
    
    # 2. Execute Sales (LEAVERS)
    # --------------------------
    cash_from_sales <- 0
    
    if (length(leavers) > 0) {
      for (asset in leavers) {
        # Sell entire position
        cash_from_sales <- cash_from_sales + current_holdings[asset]
        current_holdings[asset] <- 0
        
        # Fee for selling
        fees_today <- fees_today + COST_PER_TRADE
      }
    }
    
    # Add sales proceeds to cash pile
    current_cash <- current_cash + cash_from_sales
    
    # Deduct Selling Fees immediately from cash (if possible)
    # Note: In reality, fees reduce the cash balance.
    current_cash <- current_cash - (length(leavers) * COST_PER_TRADE)
    
    # 3. Execute Buys (NEWCOMERS)
    # ---------------------------
    # We use the AVAILABLE CASH to buy newcomers equal weight.
    # We do NOT touch the "stayers".
    
    if (length(newcomers) > 0) {
      
      # Calculate buying fees
      buy_fees <- length(newcomers) * COST_PER_TRADE
      fees_today <- fees_today + buy_fees
      
      # Net cash available to invest
      investable_cash <- current_cash - buy_fees
      
      if (investable_cash > 0) {
        amount_per_newcomer <- investable_cash / length(newcomers)
        
        for (asset in newcomers) {
          current_holdings[asset] <- amount_per_newcomer
        }
        
        # Cash is now fully invested (except for potential rounding dust)
        current_cash <- 0 
      } else {
        # Warning: Fees ate all the cash (unlikely with reasonable capital)
        current_cash <- investable_cash # (negative balance would indicate debt)
      }
    }
    
    # 4. Stayers
    # ----------
    # Nothing happens. They stay at `current_holdings` value.
    # No fees.
    
  }
  
  # C. STORE RESULTS
  # -------------------------------------------------------
  # Sanity check for negative cash due to fees
  if (current_cash < 0) {
     # Assume we borrow or inject cash to pay fees (simplified)
     # In a backtest, this just lowers the NAV
  }
  
  nav_curve[i]  <- current_cash + sum(current_holdings)
  cost_curve[i] <- fees_today
}

# =========================================================
# 3. PROCESS RESULTS & MERGE
# =========================================================

# 1. Net Strategy
net_strategy_xts <- xts(nav_curve, order.by = all_dates)
net_ret <- Return.calculate(net_strategy_xts)
net_ret[is.na(net_ret)] <- 0
colnames(net_ret) <- "Regime_Net"

# 2. Gross Strategy (Script 5)
gross_ret <- strat_xts 
colnames(gross_ret) <- "Regime_Gross"

# 3. Merge
comparison <- merge(gross_ret, net_ret, bench_ret, join = "inner")
comparison[is.na(comparison)] <- 0

# 4. Trim Start
start_idx <- which(comparison$Regime_Net != 0)[1]
if (!is.na(start_idx)) {
  actual_start <- index(comparison)[start_idx]
  comparison   <- comparison[paste0(actual_start, "/")]
}

# =========================================================
# 4. REPORTING
# =========================================================

total_fees <- sum(cost_curve)
final_equity <- last(nav_curve)
days_with_trades <- sum(cost_curve > 0)

cat("\n===============================================\n")
cat(" FINAL RESULT (Drift & Fill)\n")
cat("===============================================\n")
cat("Start Capital:   ", format(INITIAL_CAPITAL, big.mark="."), "EUR\n")
cat("Final Equity:    ", format(round(final_equity, 2), big.mark="."), "EUR\n")
cat("Total Fees:      ", format(round(total_fees, 2), big.mark="."), "EUR\n")
cat("Trading Months:  ", days_with_trades, "\n")
cat("Cost Drag:       ", round((total_fees/final_equity)*100, 2), "%\n")

# Plot
plot_data <- data.frame(date = index(comparison), coredata(comparison)) %>%
  mutate(
    Gross_Curve = cumprod(1 + Regime_Gross) * INITIAL_CAPITAL,
    Net_Curve   = cumprod(1 + Regime_Net) * INITIAL_CAPITAL,
    Bench_Curve = cumprod(1 + Benchmark) * INITIAL_CAPITAL
  ) %>%
  pivot_longer(cols = ends_with("Curve"), names_to = "Type", values_to = "Equity")

cols <- c("Gross_Curve" = "darkblue", "Net_Curve" = "lightblue", "Bench_Curve" = "gray")

p_drift <- ggplot(plot_data, aes(x = date, y = Equity, color = Type)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = cols, labels = c("Benchmark", "Strategy Gross", "Strategy Net")) +
  scale_y_log10(labels = dollar_format(prefix = "€", big.mark = ".")) +
  labs(
    title = paste("Regime Top5 vs. Benchmark (Log)"),
    y = "Portfolio Value (Log)", x = NULL, color = NULL
  ) +
  theme_minimal() + theme(legend.position = "top")

print(p_drift)

charts.PerformanceSummary(comparison, 
                          main = "Regime Top 5 vs Benchmark",
                          colorset = c("darkblue", "lightblue", "gray"),
                          lwd = 2)

chart.Drawdown(comparison, 
               main = "Drawdowns: Strategy vs Benchmark",
               colorset = c("darkblue", "lightblue", "gray"),
               legend.loc = "bottomright")

# Stats
cat("\n--- PERFORMANCE SUMMARY ---\n")
print(round(rbind(table.AnnualizedReturns(comparison),
                  maxDrawdown(comparison)), 4))
