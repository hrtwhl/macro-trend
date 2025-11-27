# ------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
# ------------------------------------------------------------------------------
required <- c("tidyquant", "dplyr", "purrr", "tidyr", "stringr", "readr", "lubridate")
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

library(tidyquant)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)

# ------------------------------------------------------------------------------
# 2. BLOOMBERG CSV IMPORT & CLEANING
# ------------------------------------------------------------------------------
# CSV einlesen
bb_data_raw <- read_csv("PMP1.CSV", show_col_types = FALSE)

# Datensäuberung
bb_data_clean <- bb_data_raw %>%
  # 1. "Dates" zu "date" umbenennen
  rename(date = Dates) %>%
  
  # 2. Datumsformat sicherstellen
  mutate(date = as.Date(date)) %>%
  
  # 3. Spaltennamen bereinigen: 
  # Der Regex "[ .]Index$" entfernt " Index" (mit Leerzeichen) ODER ".Index" (mit Punkt)
  rename_with(~ str_remove(., "[ .]Index$"), .cols = -date) %>%
  
  # 4. SICHERSTELLEN, DASS ALLES ZAHLEN SIND
  # Wandelt Text-Fehler in NAs um und verhindert den Pivot-Fehler
  mutate(across(-date, as.numeric)) %>%
  
  arrange(date)

# ------------------------------------------------------------------------------
# 3. YAHOO FINANCE DOWNLOADER
# ------------------------------------------------------------------------------
etfs <- tribble(
  ~group, ~label, ~yf_ticker,
  "Bond", "EM Sov USD",   "IUS7.DE",
  "Bond", "EM Local",     "IUSP.DE",
  "Bond", "EM Corp",      "IS0Q.DE",
  "Bond", "EU Gov 1-3y",  "IBGS.MI",
  "Bond", "EU Gov 7-10y", "IBGM.MI",
  "Bond", "US Tre 1-3y",  "IBTS.MI",
  "Bond", "US Tre 7-10y", "IUSM.DE",
  "Bond", "EU Inflation", "IBCI.DE",
  "Bond", "EU Corp IG",   "EUN5.DE",
  "Bond", "US Corp HY",   "EUNW.DE"
) %>%
  distinct(yf_ticker, .keep_all = TRUE) %>%
  # Entfernt .DE, .MI etc. für den Spaltennamen
  mutate(asset = str_replace(yf_ticker, "\\.[A-Za-z]+$", "")) 

price_col <- "adjusted" 

# Download
safe_tq <- purrr::safely(function(tks) {
  tidyquant::tq_get(tks, get = "stock.prices",
                    from = as.Date("1900-01-01"), 
                    to   = Sys.Date(),
                    complete_cases = FALSE)
})

res <- safe_tq(etfs$yf_ticker)
raw_prices <- res$result

if (is.null(raw_prices)) stop("Fehler: Keine Yahoo-Daten heruntergeladen.")

# Yahoo Wide Format erstellen
yf_wide <- raw_prices %>%
  left_join(etfs, by = c("symbol" = "yf_ticker")) %>%
  select(date, asset, price = all_of(price_col)) %>%
  distinct() %>%
  pivot_wider(names_from = asset, values_from = price) %>%
  arrange(date)

# ------------------------------------------------------------------------------
# 4. MERGE & FINAL OUTPUT (KORRIGIERT)
# ------------------------------------------------------------------------------

# LÖSUNG: left_join statt inner_join
# Das behält ALLE Datumswerte aus 'bb_data_clean', auch wenn Yahoo dort noch keine Daten hat.
merged_data <- left_join(bb_data_clean, yf_wide, by = "date")

# Optional: full_join, falls Yahoo Daten hat, die Bloomberg nicht hat (eher unwahrscheinlich in die Vergangenheit)
# merged_data <- full_join(bb_data_clean, yf_wide, by = "date") %>% arrange(date)

# ASSETS WIDE
assets_wide <- merged_data

# ACHTUNG BEI NA.OMIT:
# Wenn du die folgende Zeile wieder einkommentierst, verlierst du die Daten wieder,
# weil R alle Zeilen löscht, die irgendwo ein NA (fehlenden ETF-Preis) haben.
# assets_wide <- na.omit(merged_data) 

# ASSETS LONG
assets_long <- assets_wide %>%
  pivot_longer(
    cols = -date, 
    names_to = "asset", 
    values_to = "price"
  ) %>%
  arrange(asset, date)

# ------------------------------------------------------------------------------
# 5. CHECK
# ------------------------------------------------------------------------------
cat("Daten erfolgreich erstellt.\n")
cat("Anzahl Assets:", ncol(assets_wide) - 1, "\n")
cat("\nCheck Spaltennamen (sollte ohne 'Index' und ohne '.DE' sein):\n")
print(head(colnames(assets_wide), 15))




