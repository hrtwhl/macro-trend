# =========================================================
# SCRIPT 5.2: REALISTIC BACKTEST (DRIFT & FILL) - STANDALONE
# =========================================================
# Fixes:
# 1. NA handling for cumprod (Fixes Log-Chart ending in 2000).
# 2. Fixed formatting warnings & scientific notation.
# 3. Integrated Log-Scale Wealth Chart & Allocation Chart.

# Dependencies
library(dplyr)
library(tidyr)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
library(ggplot2)
library(scales)
library(stringr)

# =========================================================
# 1. CONFIGURATION & DATA PREPARATION
# =========================================================

# --- USER INPUTS ---
INITIAL_CAPITAL    <- 100000   
COST_PER_TRADE     <- 50       
BENCHMARK_TICKER   <- "MIMUEMRN" 

# Portfolio Limits
MAX_SINGLE_ASSET   <- 0.20
MAX_BOND_ALLOC     <- 0.40
MAX_EQUITY_ALLOC   <- 1.00
MAX_TOTAL_EXPOSURE <- 1.00 

if (!exists("assets_wide") || !exists("signals_long")) {
  stop("FEHLER: Bitte zuerst Skripte 1 bis 4 ausführen!")
}

# Helper: Asset Classes
get_asset_class <- function(tickers) {
  bond_tickers <- c("JPEICORE", "GBIE1001", "BCEX6T", "BCEX4T", "IDCOT7", 
                    "BEIG1T", "LECPTREU", "IBOXXMJA")
  ifelse(tickers %in% bond_tickers, "Bond", "Equity")
}

# --- SETUP DAILY DATA ---
daily_prices_xts <- assets_wide %>% 
  arrange(date) %>%
  distinct(date, .keep_all = TRUE) %>%
  select(-date) %>%
  xts(order.by = sort(unique(assets_wide$date)))

daily_returns_xts <- Return.calculate(daily_prices_xts)
daily_returns_xts[is.na(daily_returns_xts)] <- 0 # Initial NAs entfernen

bench_prices_clean <- na.omit(daily_prices_xts[, BENCHMARK_TICKER])
bench_ret <- Return.calculate(bench_prices_clean)
colnames(bench_ret) <- "Benchmark"

# =========================================================
# 2. WEIGHT DETERMINATION (TARGETS)
# =========================================================
rebalance_dates <- unique(signals_long$date)
weights_history <- list()

cat("Berechne Ziel-Gewichtungen für", length(rebalance_dates), "Perioden...\n")

for (i in seq_along(rebalance_dates)) {
  curr_date <- rebalance_dates[i]
  sig_subset <- signals_long %>%
    filter(date == curr_date) %>%
    mutate(asset_class = get_asset_class(asset)) %>%
    arrange(desc(expected_return)) 
  
  w_vec <- rep(0, nrow(sig_subset)); names(w_vec) <- sig_subset$asset
  curr_eq <- 0; curr_bond <- 0; total_exp <- 0
  
  for (j in 1:nrow(sig_subset)) {
    if (total_exp >= MAX_TOTAL_EXPOSURE) break
    asset_name <- sig_subset$asset[j]; aclass <- sig_subset$asset_class[j]; exp_ret <- sig_subset$expected_return[j]
    if (is.na(exp_ret) || is.nan(exp_ret)) exp_ret <- 0
    if (exp_ret <= 0) next 
    
    room_total <- MAX_TOTAL_EXPOSURE - total_exp
    room_class <- if (aclass == "Bond") MAX_BOND_ALLOC - curr_bond else MAX_EQUITY_ALLOC - curr_eq
    allocation <- min(MAX_SINGLE_ASSET, room_class, room_total)
    
    if (allocation > 0.001) {
      w_vec[asset_name] <- allocation
      total_exp <- total_exp + allocation
      if (aclass == "Bond") curr_bond <- curr_bond + allocation else curr_eq <- curr_eq + allocation
    }
  }
  weights_history[[as.character(curr_date)]] <- w_vec
}

# =========================================================
# 3. SIMULATION ENGINE (DRIFT & FILL)
# =========================================================
all_dates <- index(daily_returns_xts)
asset_names <- colnames(daily_returns_xts)
ac_map <- get_asset_class(asset_names)

nav_curve     <- numeric(length(all_dates))
cost_curve    <- numeric(length(all_dates)) 
gross_returns <- numeric(length(all_dates)) 
alloc_matrix  <- matrix(0, nrow = length(all_dates), ncol = 3, 
                        dimnames = list(NULL, c("Equity", "Bond", "Cash")))

current_cash     <- INITIAL_CAPITAL
current_holdings <- rep(0, length(asset_names)); names(current_holdings) <- asset_names
gross_weights    <- rep(0, length(asset_names)); names(gross_weights) <- asset_names

cat("Starte Simulation (Kapital:", format(INITIAL_CAPITAL, big.mark=".", decimal.mark=",", scientific=F), "EUR)...\n")

for (i in seq_along(all_dates)) {
  today <- all_dates[i]; date_char <- as.character(today)
  todays_ret <- daily_returns_xts[today, ]; todays_ret[is.na(todays_ret)] <- 0
  
  # A. Update Values (Drift)
  current_holdings <- current_holdings * (1 + as.numeric(todays_ret))
  fees_today <- 0
  
  # B. Rebalancing
  if (date_char %in% names(weights_history)) {
    target_w <- weights_history[[date_char]]
    gross_weights[] <- 0
    valid_assets <- intersect(names(target_w), names(gross_weights))
    gross_weights[valid_assets] <- target_w[valid_assets]
    
    target_assets <- names(target_w)[target_w > 0]
    held_assets   <- names(current_holdings)[current_holdings > 1]
    
    leavers   <- setdiff(held_assets, target_assets)
    newcomers <- setdiff(target_assets, held_assets)
    if (sum(current_holdings) == 0) newcomers <- target_assets
    
    if (length(leavers) > 0) {
      for (a in leavers) { current_cash <- current_cash + current_holdings[a]; current_holdings[a] <- 0 }
      fees_today <- fees_today + (length(leavers) * COST_PER_TRADE)
      current_cash <- current_cash - (length(leavers) * COST_PER_TRADE)
    }
    
    if (length(newcomers) > 0) {
      buy_fees <- length(newcomers) * COST_PER_TRADE
      fees_today <- fees_today + buy_fees
      investable <- current_cash - buy_fees
      if (investable > 0) {
        for (a in newcomers) current_holdings[a] <- investable / length(newcomers)
        current_cash <- 0
      }
    }
  }
  
  # C. Records
  total_nav <- current_cash + sum(current_holdings)
  nav_curve[i]  <- total_nav
  cost_curve[i] <- fees_today
  gross_returns[i] <- sum(todays_ret * gross_weights)
  
  alloc_matrix[i, "Equity"] <- sum(current_holdings[ac_map == "Equity"]) / total_nav
  alloc_matrix[i, "Bond"]   <- sum(current_holdings[ac_map == "Bond"]) / total_nav
  alloc_matrix[i, "Cash"]   <- current_cash / total_nav
}

# =========================================================
# 4. REPORTING & CHARTS
# =========================================================
net_ret <- Return.calculate(xts(nav_curve, order.by = all_dates))
gross_ret_xts <- xts(gross_returns, order.by = all_dates)

# WICHTIG: NAs entfernen, damit cumprod() im Log-Chart nicht abbricht
comparison <- merge(net_ret, gross_ret_xts, bench_ret)
comparison[is.na(comparison)] <- 0
colnames(comparison) <- c("Net", "Gross", "Benchmark")

# Trim auf den ersten Trade
start_idx <- which(nav_curve > INITIAL_CAPITAL * 1.0001 | nav_curve < INITIAL_CAPITAL * 0.9999)[1]
actual_start <- index(comparison)[start_idx]
comp_plot <- comparison[paste0(actual_start, "/")]

# --- LOG CHART ---
plot_df <- data.frame(date = index(comp_plot), coredata(comp_plot)) %>%
  pivot_longer(-date, names_to = "Type", values_to = "Ret") %>%
  group_by(Type) %>% 
  mutate(Wealth = INITIAL_CAPITAL * cumprod(1 + Ret))

p_log <- ggplot(plot_df, aes(x=date, y=Wealth, color=Type)) +
  geom_line(linewidth=0.8) + 
  scale_y_log10(labels = dollar_format(prefix="€", big.mark=".", decimal.mark=",")) +
  scale_color_manual(values=c("Benchmark"="gray", "Gross"="darkblue", "Net"="lightblue")) +
  labs(title="Regime Strategy: Wealth Index (Log Scale)", 
       subtitle=paste("Start Simulation:", actual_start), y="Portfolio Value", x=NULL) +
  theme_minimal() + theme(legend.position="top")
print(p_log)

# --- ALLOCATION CHART ---
alloc_xts <- xts(alloc_matrix, order.by = all_dates)[paste0(actual_start, "/")]
chart.StackedBar(alloc_xts[endpoints(alloc_xts, "months")], 
                 main="Portfolio Allocation (Drift & Fill)", 
                 colorset=c("#003f5c", "#ffa600", "#bc5090"), ylab="Exposure", border=NA)

# --- PERFORMANCE SUMMARY ---
charts.PerformanceSummary(comp_plot[,c("Net", "Benchmark")], 
                          main="Net Strategy vs Benchmark", 
                          colorset=c("darkblue", "gray"), lwd=2)

# --- TEXT OUTPUT ---
cat("\n===============================================\n")
cat(" FINAL RESULT (Drift & Fill Standalone)\n")
cat("===============================================\n")
cat("Start Capital:   ", format(INITIAL_CAPITAL, big.mark=".", decimal.mark=",", scientific=F), "EUR\n")
cat("Final Equity:    ", format(round(last(nav_curve), 2), big.mark=".", decimal.mark=",", scientific=F), "EUR\n")
cat("Total Fees:      ", format(round(sum(cost_curve), 2), big.mark=".", decimal.mark=",", scientific=F), "EUR\n")
cat("Cost Drag:       ", round((sum(cost_curve)/INITIAL_CAPITAL)*100, 2), "%\n")

print(round(rbind(table.AnnualizedReturns(comp_plot), maxDrawdown(comp_plot)), 4))
