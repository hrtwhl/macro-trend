# -------------------------------------------------------------------------
# Bloomberg Index Downloader (EUR Conversion)
# -------------------------------------------------------------------------

# 1. Load necessary libraries
# If you don't have Rblpapi, install it: install.packages("Rblpapi")
library(Rblpapi)
library(dplyr) # For easier data manipulation

# 2. Connect to Bloomberg
# Note: A Bloomberg Terminal must be running and logged in on this machine.
con <- blpConnect()

# 3. Define the Tickers directly
# We use the correct Bloomberg format (Space instead of Dot) immediately.
tickers <- c(
  "IS3N GY Equity",   # iShares Core MSCI Emerging Markets IMI UCITS ETF (Acc)
  "ICGA GY Equity",   # iShares MSCI China UCITS ETF USD (Acc)
  "QDV5 GY Equity",   # iShares MSCI India UCITS ETF USD (Acc)
  "CEBJ GR Equity",   # iShares MSCI Korea UCITS ETF (Acc)
  "XMTW GY Equity",   # Xtrackers MSCI Taiwan UCITS ETF
  "H4ZT GY Equity",   # HSBC MSCI Indonesia UCITS ETF USD
  "XCS3 GT Equity",   # Xtrackers MSCI Malaysia UCITS ETF
  "XCS4 GY Equity",   # Xtrackers MSCI Thailand UCITS ETF
  "XPQP GY Equity",   # Xtrackers MSCI Philippines UCITS ETF
  "XMBR GY Equity",   # Xtrackers MSCI Brazil UCITS ETF
  "D5BI GY Equity",   # Xtrackers MSCI Mexico UCITS ETF
  "XMLA GT Equity",   # Xtrackers MSCI EM Latin America ESG Swap UCITS ETF
  "IBC4 GY Equity",   # iShares MSCI South Africa UCITS ETF
  "LTUR GY Equity",   # Amundi MSCI Turkey UCITS ETF
  "LYXGRE GY Equity", # Amundi MSCi Greece UCITS ETF
  "IBCJ GY Equity",   # iShares MSCI Poland UCITS ETF
  "IUSS GY Equity",   # iShares MSCI Saudi Arabia Capped UCITS ETF
  "IUS7 GY Equity",   # iShares J.P. Morgan USD Emerging Markets Bond UCITS ETF (Dist)
  "IUSP GY Equity",   # iShares J.P. Morgan EM Local Government Bond UCITS ETF
  #"ISOQ GY Equity",  # iShares J.P. Morgan EM Local Government Bond UCITS ETF
  "IBCA GY Equity",   # iShares Euro Government Bond 1-3yr UCITS ETF (Dist)
  "IBCM GY Equity",   # iShares Euro Government Bond 7-10yr UCITS ETF EUR (Dist)
  #"IUSU GY Equity",  # iShares $ Treasury Bond 1-3yr UCITS ETF
  "IUSM GY Equity",   # iShares USD Treasury Bond 7-10yr UCITS ETF (Dist)
  "IBCI GY Equity",   # iShares Euro Inflation Linked Government Bond UCITS ETF
  "EUN5 GY Equity",   # iShares Core Euro Corporate Bond UCITS ETF (Dist)
  "EUNW GY Equity"    # iShares EUR High Yield Corporate Bond UCITS ETF EUR (Dist)
)

print(paste("Downloading", length(tickers), "ETFs in EUR..."))

# 4. Define Download Parameters
start_date <- as.Date("2020-01-01") 
end_date   <- Sys.Date()
field      <- "PX_LAST"             

# 5. Fetch Data with Currency Conversion
# CORRECT USAGE:
# "currency" is an OPTION, not an override. 
# We remove the 'overrides' argument entirely to prevent API conflicts.
opt <- c("currency" = "EUR", "periodicitySelection" = "DAILY")

data_list <- bdh(
  securities = tickers, 
  fields = field, 
  start.date = start_date, 
  end.date = end_date,
  options = opt
)

# 6. Post-Processing: Convert List to Single Data Frame
# Get the actual names returned by Bloomberg (safer than assuming order)
returned_tickers <- names(data_list)

if (length(returned_tickers) == 0) {
  stop("No data was returned from Bloomberg. Check connection.")
}

# Initialize with the first dataframe
final_df <- data_list[[1]]
colnames(final_df) <- c("Dates", returned_tickers[1]) 

# Loop through the rest and merge by Date
if (length(data_list) > 1) {
  for (i in 2:length(data_list)) {
    temp_df <- data_list[[i]]
    
    # Only process if we actually got data for this ticker
    if (!is.null(temp_df) && nrow(temp_df) > 0) {
      # Rename columns: keep Date, use the ticker name
      colnames(temp_df) <- c("Dates", returned_tickers[i])
      
      # Merge (full_join ensures we don't lose days if calendars differ)
      final_df <- merge(final_df, temp_df, by = "Dates", all = TRUE)
    }
  }
}

# Optional: Save to CSV
write.csv(final_df, "EM_ETFs_EUR.csv", row.names = FALSE)

print("Download complete.")

summary(final_df)


