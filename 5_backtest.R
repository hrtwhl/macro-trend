# =========================================================
# SCRIPT 5: BACKTEST & REPORTING (CORRECTED)
# =========================================================
# Fixes: Enforces Global Portfolio Cap of 100% (No Leverage).
# Strict Waterfall: Best Assets -> Constraints -> Cash.

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(stringr)

# =========================================================
# 1. CONFIGURATION & DATA LOADING
# =========================================================
# Use the objects from previous steps
# signals_long <- readRDS("signals_long.rds")
# assets_long  <- readRDS("assets_long.rds") 
# assets_wide  <- readRDS("assets_wide.rds") # Ensure this exists from Script 1

BENCHMARK_TICKER <- "MIMUEMRN" # iShares MSCI EM

# Portfolio Limits (Strict)
MAX_SINGLE_ASSET   <- 0.20
MAX_BOND_ALLOC     <- 0.40
MAX_EQUITY_ALLOC   <- 1.00
MAX_TOTAL_EXPOSURE <- 1.00 # The hard ceiling

# Helper: Map Tickers to Class
get_asset_class <- function(tickers) {
  bond_tickers <- c("JPEICORE", "GBIE1001", "BCEX6T", "BCEX4T", "IDCOT7", 
                    "BEIG1T", "LECPTREU", "IBOXXMJA")
  ifelse(tickers %in% bond_tickers, "Bond", "Equity")
}

# =========================================================
# 2. PREPARE DAILY DATA (FIXED)
# =========================================================
daily_prices <- assets_wide %>% 
  arrange(date) %>%
  select(date, everything()) 

# Create XTS
daily_prices_xts <- daily_prices %>%
  select(-date) %>%
  xts(order.by = daily_prices$date)

# Calculate Returns
daily_returns_xts <- Return.calculate(daily_prices_xts)

# CRITICAL FIX: Do NOT use na.omit() on the whole object.
# Instead, we leave NAs in place. The backtest loop handles them.
# However, for the Benchmark, we need a clean series.

bench_ret <- daily_returns_xts[, BENCHMARK_TICKER]
bench_ret <- na.omit(bench_ret) # It's okay to clean the benchmark vector
colnames(bench_ret) <- "Benchmark"

# =========================================================
# 3. WEIGHT DETERMINATION (STRICT 100% CAP)
# =========================================================
rebalance_dates <- unique(signals_long$date)
weights_history <- list()

cat("Calculating Non-Leveraged Weights for", length(rebalance_dates), "periods...\n")

for (i in seq_along(rebalance_dates)) {
  curr_date <- rebalance_dates[i]
  
  # 1. Get & Rank Signals
  sig_subset <- signals_long %>%
    filter(date == curr_date) %>%
    mutate(asset_class = get_asset_class(asset)) %>%
    arrange(desc(expected_return)) # Best assets first
  
  # 2. Initialize Counters
  w_vec <- rep(0, nrow(sig_subset))
  names(w_vec) <- sig_subset$asset
  
  current_equity_exp <- 0
  current_bond_exp   <- 0
  total_exp          <- 0
  
  # 3. Allocation Loop
  for (j in 1:nrow(sig_subset)) {
    # Stop immediately if portfolio is full
    if (total_exp >= MAX_TOTAL_EXPOSURE) break
    
    asset_name <- sig_subset$asset[j]
    aclass     <- sig_subset$asset_class[j]
    exp_ret    <- sig_subset$expected_return[j]
    
    # Logic: Only invest if signal is Positive
    if (exp_ret <= 0) next 
    
    # Calculate Remaining Room
    # 1. Room in Total Portfolio
    room_total <- MAX_TOTAL_EXPOSURE - total_exp
    
    # 2. Room in Asset Class
    room_class <- if (aclass == "Bond") {
      MAX_BOND_ALLOC - current_bond_exp
    } else {
      MAX_EQUITY_ALLOC - current_equity_exp
    }
    
    # 3. Actual Allocation is the MINIMUM of:
    #    - The standard Single Asset Cap (20%)
    #    - Space left in the Class
    #    - Space left in the Portfolio
    allocation <- min(MAX_SINGLE_ASSET, room_class, room_total)
    
    # Assign Weight if > 0
    if (allocation > 0.001) {
      w_vec[asset_name] <- allocation
      total_exp <- total_exp + allocation
      
      if (aclass == "Bond") {
        current_bond_exp <- current_bond_exp + allocation
      } else {
        current_equity_exp <- current_equity_exp + allocation
      }
    }
  }
  
  weights_history[[as.character(curr_date)]] <- w_vec
}

# =========================================================
# 4. DAILY BACKTEST ENGINE
# =========================================================
all_dates <- index(daily_returns_xts)
strat_returns <- numeric(length(all_dates))
weight_matrix <- matrix(0, nrow = length(all_dates), ncol = ncol(daily_returns_xts))
colnames(weight_matrix) <- colnames(daily_returns_xts)

# Align columns helper
asset_names <- colnames(daily_returns_xts)
current_weights <- rep(0, length(asset_names))
names(current_weights) <- asset_names
last_rebal_idx <- 0

for (d in 1:length(all_dates)) {
  today <- all_dates[d]
  
  # Check for rebalance
  valid_rebal_dates <- rebalance_dates[rebalance_dates < today]
  
  if (length(valid_rebal_dates) > 0) {
    latest_rebal <- max(valid_rebal_dates)
    
    if (as.numeric(latest_rebal) != last_rebal_idx) {
      target_w <- weights_history[[as.character(latest_rebal)]]
      
      # Reset current weights
      current_weights[] <- 0 
      current_weights[names(target_w)] <- target_w
      last_rebal_idx <- as.numeric(latest_rebal)
    }
  }
  
  # Store daily weight
  weight_matrix[d, ] <- current_weights
  
  # Calc Return
  day_rets <- daily_returns_xts[d, ]
  day_rets[is.na(day_rets)] <- 0
  
  # Weights sum to <= 1.0. 
  # If Sum < 1.0, the remainder is implicitly Cash (0% return)
  strat_returns[d] <- sum(day_rets * current_weights)
}

strat_xts <- xts(strat_returns, order.by = all_dates)
colnames(strat_xts) <- "Regime Strategy"

# =========================================================
# 5. REPORTING & VISUALIZATION (ROBUST SYNCHRONIZATION)
# =========================================================

# 1. Bestimme den Startzeitpunkt der Strategie
# Wir schauen, wann das allererste Signal generiert wurde.
# Da wir am Tag NACH dem Signal handeln, ist das unser Startdatum.
first_signal_date <- min(as.Date(rebalance_dates))

cat(paste("Strategie-Signale beginnen am:", first_signal_date, "\n"))

# 2. Merge Strategy und Benchmark
compare_xts_raw <- merge(strat_xts, bench_ret, join = "inner")

# 3. HARD CUT: Alles vor dem ersten Signal abschneiden
# Wir filtern den XTS-Datensatz, sodass er erst ab dem Startdatum beginnt.
compare_xts <- compare_xts_raw[paste0(first_signal_date, "/")]

# Check: Hat das geklappt?
cat("Startdatum des Charts:", as.character(start(compare_xts)), "\n")

# 4. Charts erstellen (Jetzt sollten beide gleichzeitig starten)
charts.PerformanceSummary(compare_xts, 
                          main = "Regime Strategy vs Benchmark (Synchronisiert)",
                          colorset = c("blue", "gray"),
                          lwd = 2)

# 5. Statistiken berechnen (Nur für den aktiven Zeitraum)
stats <- rbind(
  table.AnnualizedReturns(compare_xts),
  maxDrawdown(compare_xts),
  SharpeRatio.annualized(compare_xts, Rf = 0)
)

cat("\n--- PERFORMANCE SUMMARY (Synchronisiert) ---\n")
print(round(stats, 4))

# =========================================================
# 6. ALLOCATION CHART (CORRECTED)
# =========================================================

# Wir schneiden die Gewichtungs-Matrix ab, damit sie zum Chart passt
weight_matrix_xts <- xts(weight_matrix, order.by = all_dates)
weight_matrix_sync <- weight_matrix_xts[paste0(first_signal_date, "/")]

# Aggregation für den Stacked Bar Chart
ac_map <- get_asset_class(colnames(weight_matrix_sync))

# ACHTUNG: rowSums erzeugt oft normale Vektoren ohne Datum.
# Wir müssen sicherstellen, dass das Ergebnis ein xts bleibt.
equity_vec <- rowSums(weight_matrix_sync[, ac_map == "Equity"])
bond_vec   <- rowSums(weight_matrix_sync[, ac_map == "Bond"])
cash_vec   <- 1 - (equity_vec + bond_vec)

# Zusammenfügen und explizit als xts definieren mit dem korrekten Zeitindex
alloc_df <- xts(cbind(Equity = equity_vec, Bond = bond_vec, Cash = cash_vec),
                order.by = index(weight_matrix_sync))

# Kleine Bereinigungen (Floating point errors vermeiden)
alloc_df[alloc_df < 0] <- 0 
alloc_df[alloc_df > 1] <- 1

# Jetzt plotten (sollte nun funktionieren, da alloc_df ein xts ist)
chart.StackedBar(alloc_df[endpoints(alloc_df, "months")], 
                 main = "Portfolio Allocation (Synchronisiert)",
                 colorset = c("green", "orange", "lightgray"),
                 ylab = "Allocation", 
                 ylim = c(0, 1.0))

# Current Allocation Check
cat("\n--- CURRENT ALLOCATION (Strict 100% Cap) ---\n")
latest_w <- coredata(weight_matrix_sync)[nrow(weight_matrix_sync), ]
latest_w <- latest_w[latest_w > 0]
print(round(latest_w, 3))
cat("Total Allocation:", sum(latest_w), "\n")
