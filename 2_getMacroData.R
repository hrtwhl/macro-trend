# =========================================================
# PACKAGES
# =========================================================
required <- c("fredr", "dplyr", "tidyr", "purrr", "tidyquant", "stringr", "tibble")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
invisible(lapply(required, library, character.only = TRUE))

# =========================================================
# FRED API KEY
# =========================================================
# Your file should call: fredr_set_key("<YOUR_KEY>")
source("APIKey.R")

# =========================================================
# FRED SERIES (THE "CORE 6" MACRO LIST)
# =========================================================
# This list is based on our analysis of Heiden/AQR/Man
# and designed for low correlation and high EM-relevance.
fred_series <- c(

  #usd_em_index      = "DTWEXEMEGS",
  #usd_eur_rate      = "DEXUSEU", 
  dollar_index      = "NBUSBIS",
  term_spread_10y3m = "T10Y3M",
  yield_10y         = "DGS3MO",
  #yield_10y         = "DGS10",
  #yield_3m          = "DGS3MO",

  #cpi_headline      = "CPIAUCSL",
  vix_index         = "VIXCLS"
  #china_cli         = "CHNLOLITOAASTSAM"
  #copper            = "PCOPPUSDM",
  #oil               = "POILWTIUSDM"
)


# Helper: download FRED as long table
get_fred_data <- function(named_series, start_date = "2000-01-01", freq = NULL) {
  purrr::imap_dfr(named_series, function(ticker, nice_name) {
    fredr(
      series_id = ticker,
      observation_start = as.Date(start_date),
      frequency = freq
    ) |>
      transmute(date, series = nice_name, value)
  }) |>
    mutate(source = "FRED")
}

# =========================================================
# YAHOO FUTURES (THE "CORE 6" MACRO LIST)
# =========================================================
# 5. Global Growth (Man Group Theme)
# 6. Inflation Shock (Man Group Theme)
yf_futures <- tibble::tibble(
  yf = c("^SPX", "CL=F", "HG=F"),
  series = c("SPX", "oil", "copper")  # Clean names for the series
)

get_yahoo_close <- function(tickers_tbl) {
  tq_get(
    tickers_tbl$yf,
    get  = "stock.prices",
    from = as.Date("1900-01-01"),
    to   = Sys.Date()
  ) |>
    transmute(date, symbol, value = close) |>
    left_join(tickers_tbl, by = c("symbol" = "yf")) |>
    transmute(date, series, value, source = "YF") |>
    arrange(series, date)
}

# =========================================================
# DOWNLOAD & BUILD FINAL TABLES (WITH LAG CORRECTION)
# =========================================================
# 1) FRED (4 series)
fred_long <- get_fred_data(fred_series, start_date = "2000-01-01")

# 2) Yahoo futures (2 series)
yf_long   <- get_yahoo_close(yf_futures)

# 3) Combined LONG
macro_long_raw <- bind_rows(fred_long, yf_long) %>%
  arrange(series, date)

# --- APPLY LAGS HERE (CORRECTED) ---
# Logic: 
# CPI is released mid-next month -> We only know Jan CPI in mid-Feb. 
# Safe to trade on it end of Feb (Lag = 1 month).
# China CLI often lags 1-2 months. Safe to trade end of Month+2 (Lag = 2 months).
lags_vec <- c("cpi_headline" = 1, "china_cli" = 2)

macro_long <- macro_long_raw %>%
  group_by(series) %>%
  mutate(
    # 1. Lookup the lag. 
    # as.character ensures we match the name correctly.
    # coalesce replaces any NA (not found) with 0.
    lag_amount = coalesce(lags_vec[as.character(series)], 0),
    
    # 2. Apply lag. 
    # If lag_amount is 0, lag(value, 0) simply returns the value (no change).
    # This avoids the "n must be a whole number" error.
    value = dplyr::lag(value, n = first(lag_amount))
  ) %>%
  select(-lag_amount) %>% # Clean up
  ungroup() %>%
  drop_na() # Drop the first few months that became NA due to lagging

# 4) Combined WIDE (Standard pivot)
macro_wide <- macro_long %>%
  dplyr::select(date, series, value) %>%
  dplyr::distinct() %>%
  dplyr::group_by(date, series) %>%
  dplyr::summarise(value = dplyr::last(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = series, values_from = value) %>%
  dplyr::arrange(date)

# Check
cat("Lag applied. NAs removed. Start Date:", as.character(min(macro_wide$date)), "\n")

View(macro_wide)
