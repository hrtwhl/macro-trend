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
bb_data_raw <- read_csv("EM_Indices_EUR.csv", show_col_types = FALSE)

# Datensäuberung
assets_wide <- bb_data_raw %>%
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


# ASSETS LONG
assets_long <- assets_wide %>%
  pivot_longer(
    cols = -date, 
    names_to = "asset", 
    values_to = "price"
  ) %>%
  arrange(asset, date)

# ------------------------------------------------------------------------------
# 3. CHECK
# ------------------------------------------------------------------------------
cat("Daten erfolgreich erstellt.\n")
cat("Anzahl Assets:", ncol(assets_wide) - 1, "\n")
cat("\nCheck Spaltennamen (sollte ohne 'Index' und ohne '.DE' sein):\n")
print(head(colnames(assets_wide), 15))




