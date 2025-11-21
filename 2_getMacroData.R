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
# FRED SERIES
# =========================================================
fred_series <- c(
  # 1) USD & FX
  usd_broad_index   = "DTWEXBGS",
  usd_em_index      = "DTWEXEMEGS",
  usd_afe_index     = "DTWEXAFEGS",

  # 2) U.S. Rates & Curve
  fed_funds_rate    = "FEDFUNDS",
  yield_2y          = "DGS2",
  yield_10y         = "DGS10",
  term_spread_10y2y = "T10Y2Y",
  term_spread_10y3m = "T10Y3M",
  breakeven_infl_10y= "T10YIE",
  infl_exp_5y5y     = "T5YIFR",
  real_yield_10y    = "DFII10",

  # 3) Risk Appetite / Credit
  vix_index         = "VIXCLS",
  hy_spread         = "BAMLH0A0HYM2",
  bbb_spread        = "BAMLC0A4CBBB",
  stl_fsi           = "STLFSI4",
  chicago_fci       = "NFCI",

  # 5) Global / China Growth
  china_cli         = "CHNLOLITOAASTSAM",
  g7_cli            = "G7LOLITOAASTSAM",
  oecd_cli          = "OECDLOLITOTRGYSAM",

  # 6) U.S. Real Activity & Inflation
  industrial_prod   = "INDPRO",
  nonfarm_payrolls  = "PAYEMS",
  unemployment_rate = "UNRATE",
  initial_claims    = "ICSA",
  continuing_claims = "CCSA",
  real_retail_sales = "RRSFS",
  cpi_headline      = "CPIAUCSL",
  cpi_core          = "CPILFESL",
  ppi_all_commodities = "PPIACO",
  pce_index         = "PCEPI",
  pce_core          = "PCEPILFE"
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
# YAHOO FUTURES (full history, daily CLOSE)
# =========================================================
# Tickers per your list; Gold assumed GC=F
yf_futures <- tibble::tibble(
  yf = c("GD=F", "CL=F", "HG=F", "GC=F"),
  series = stringr::str_replace(yf, "=F$", "")   # drop '=F' -> CL, HG, GD, GC
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
# DOWNLOAD & BUILD FINAL TABLES
# =========================================================
# 1) FRED
fred_long <- get_fred_data(fred_series, start_date = "2000-01-01")

# 2) Yahoo futures
yf_long   <- get_yahoo_close(yf_futures)

# 3) Combined LONG (this is your final macro_long)
macro_long <- bind_rows(fred_long, yf_long)

# 4) Combined WIDE (this is your final macro_wide)
# 4) Combined WIDE (one row per date; allow NAs)
macro_wide <- macro_long %>%
  dplyr::select(date, series, value) %>%     # <- drop `source`
  dplyr::distinct() %>%                      # guard against accidental dups
  dplyr::group_by(date, series) %>%          # in case any ticker returns 2 rows/day
  dplyr::summarise(value = dplyr::last(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = series, values_from = value) %>%
  dplyr::arrange(date)


# Done:
#   - macro_long: date, series, value, source (FRED or YF)
#   - macro_wide: date + one column per series (FRED + futures)




#--------- Reducing to core 6 -----
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

  usd_em_index      = "DTWEXEMEGS",
  term_spread_10y2y = "T10Y2Y",
  yield_2y          = "DGS2",
  yield_10y         = "DGS10",
  cpi_headline      = "CPIAUCSL",
  vix_index         = "VIXCLS",
  industrial_prod   = "INDPRO",
  china_cli         = "CHNLOLITOAASTSAM"
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
  yf = "^SPX",
  series = "SPX"   # Clean names for the series
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
# DOWNLOAD & BUILD FINAL TABLES
# =========================================================
# 1) FRED (4 series)
fred_long <- get_fred_data(fred_series, start_date = "2000-01-01")

# 2) Yahoo futures (2 series)
yf_long   <- get_yahoo_close(yf_futures)

# 3) Combined LONG (this is your final macro_long with 6 series)
macro_long <- bind_rows(fred_long, yf_long)

# 4) Combined WIDE (this is your final macro_wide)
macro_wide <- macro_long %>%
  dplyr::select(date, series, value) %>%     # <- drop `source`
  dplyr::distinct() %>%                      # guard against accidental dups
  dplyr::group_by(date, series) %>%          # in case any ticker returns 2 rows/day
  dplyr::summarise(value = dplyr::last(value), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = series, values_from = value) %>%
  dplyr::arrange(date)


# Done:
#   - macro_long: date, series, value, source (FRED or YF)
#   - macro_wide: date + one column per series (Core 6)

summary(macro_wide)



# ---- 1. Setup & Library Loading ----
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("reshape2")) install.packages("reshape2")
if (!require("lubridate")) install.packages("lubridate")

library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(lubridate)

# ---- 2. Resample & Align Data (Critical Fix) ----
# We assume 'macro_wide' is loaded. 
# We must collapse daily/sparse rows into one strict row per month.

# Helper function to get the last non-NA value
last_non_na <- function(x) {
  vals <- na.omit(x)
  if (length(vals) == 0) return(NA)
  return(last(vals))
}

macro_monthly <- macro_wide %>%
  # 1. Align all dates to the 1st of the month
  mutate(month_date = floor_date(as.Date(date), "month")) %>%
  # 2. Group by month and take the last known value for every column
  group_by(month_date) %>%
  summarise(across(everything(), last_non_na)) %>%
  # 3. Rename back to 'date' for consistency
  select(-date) %>% # remove original daily date column if it was summarized
  rename(date = month_date) %>%
  arrange(date)

# ---- 3. Calculate Transformations ----
macro_transformed <- macro_monthly %>%
  mutate(
    # --- Growth & Sentiment ---
    # SPX: 12m Return
    SPX_12m = (SPX / lag(SPX, 12)) - 1,
    
    # US IP: 12m % Change
    US_IP_12m = (industrial_prod / lag(industrial_prod, 12)) - 1,
    
    # China CLI: 12m % Change
    China_CLI_12m = (china_cli / lag(china_cli, 12)) - 1,

    # --- Inflation ---
    US_Inflation = (cpi_headline / lag(cpi_headline, 12)) - 1,

    # --- Rates ---
    US_10y_Chg = yield_10y - lag(yield_10y, 12),
    US_2y_Chg  = yield_2y - lag(yield_2y, 12),

    # --- Cycle & Risk ---
    Curve_Slope_Chg = term_spread_10y2y - lag(term_spread_10y2y, 12),
    VIX_Chg = vix_index - lag(vix_index, 12),
    
    # --- Currencies ---
    USD_EM_12m = (usd_em_index / lag(usd_em_index, 12)) - 1
  ) %>%
  select(date, SPX_12m, US_IP_12m, China_CLI_12m, US_Inflation, 
         US_10y_Chg, US_2y_Chg, Curve_Slope_Chg, VIX_Chg, USD_EM_12m)

# ---- 4. Filter Common History ----
# Now drop_na should work because rows are aligned
macro_corr_data <- macro_transformed %>%
  drop_na()

print(paste("Data Start:", min(macro_corr_data$date)))
print(paste("Data End:  ", max(macro_corr_data$date)))

# ---- 5. Heatmap ----
if(nrow(macro_corr_data) > 0) {
  cor_matrix <- cor(macro_corr_data %>% select(-date), use = "pairwise.complete.obs")
  print(round(cor_matrix, 2))

  melted_cor <- melt(cor_matrix)

  p <- ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "#D73027", high = "#4575B4", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          background_grid = element_blank()) +
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = round(value, 2)), color = "black", size = 3) +
    ggtitle(paste("Macro Correlation (", 
                  format(min(macro_corr_data$date), "%Y"), "-", 
                  format(max(macro_corr_data$date), "%Y"), ")", sep=""))
  
  print(p)
} else {
  print("Error: Dataframe is still empty. Check if 'macro_wide' columns contain data.")
}


print(melted_cor)
