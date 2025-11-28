# -------------------------------------------------------------------------
# Bloomberg Index Downloader (EUR Conversion)
# -------------------------------------------------------------------------

# 1. Load necessary libraries
# If you don't have Rblpapi, install it: install.packages("Rblpapi")
library(Rblpapi)
library(dplyr) # For easier data manipulation

install.packages("dplyr")

# 2. Connect to Bloomberg
# Note: A Bloomberg Terminal must be running and logged in on this machine.
con <- blpConnect()

# 3. Define the input column names (from your provided data)
# We exclude "Dates" as it's not a ticker.
raw_names <- c(
  "MSDEEEMN.Index", "MXCN.Index", "MXIN.Index", "MXKR.Index", 
  "MXTW000V.Index", "MXID.Index", "MXMY.Index", "MXTH.Index", 
  "MXPH.Index", "MXBR.Index", "MXMX.Index", "MXCL.Index", 
  "MXCO.Index", "MXPE.Index", "MXZA.Index", "MXTR.Index", 
  "MXGR.Index", "MXEG.Index", "MXHU.Index", "MXCZ.Index", 
  "MXPL.Index", "MXSA.Index", "MXAE.Index", "MXQA.Index", 
  "MXKW.Index", 
  "JPEICORE.Index", # IUS7 GY Equity - iShares J.P. Morgan USD Emerging Markets Bond UCITS ETF (Dist)
  "GBIE1001.Index", # IUSP GY Equity - iShares J.P. Morgan EM Local Government Bond UCITS ETF
  #"GBIE1001.Index", # ISOQ GY Equity - iShares J.P. Morgan EM Local Government Bond UCITS ETF
  "BCEX6T.Index", # IBCA GY Equity - iShares Euro Government Bond 1-3yr UCITS ETF (Dist)
  "BCEX4T.Index", # IBCM GY Equity - iShares Euro Government Bond 7-10yr UCITS ETF EUR (Dist)
  #"IDCOT1.Index", # IUSU GY Equity - iShares $ Treasury Bond 1-3yr UCITS ETF
  "IDCOT7.Index", # IUSM GY Equity - iShares USD Treasury Bond 7-10yr UCITS ETF (Dist)
  "BEIG1T.Index", # IBCI GY Equity -  iShares Euro Inflation Linked Government Bond UCITS ETF
  "LECPTREU.Index", # EUN5 GY Equity -  iShares Core Euro Corporate Bond UCITS ETF (Dist)
  "IBOXXMJA.Index" # EUNW GY Equity -  iShares EUR High Yield Corporate Bond UCITS ETF EUR (Dist)
)

# 4. Clean Tickers
# Replace all periods "." with spaces " " to match Bloomberg format
tickers <- gsub("\\.", " ", raw_names)

print(paste("Downloading", length(tickers), "indices in EUR..."))

# 5. Define Download Parameters
start_date <- as.Date("2020-01-01") # Adjust this date as needed
end_date   <- Sys.Date()
field      <- "PX_LAST"             # Closing price

# 6. Fetch Data with Currency Conversion
# The 'currency' option forces Bloomberg to convert the historical values
data_list <- bdh(
  securities = tickers, 
  fields = field, 
  start.date = start_date, 
  end.date = end_date,
  options = c("currency" = "EUR") # <--- CRITICAL: Request data in Euros
)

# 7. Post-Processing: Convert List to Single Data Frame
# bdh returns a list of data frames. We need to merge them into one wide table.

# Initialize with the first dataframe
final_df <- data_list[[1]]
colnames(final_df) <- c("Dates", raw_names[1]) # Rename columns to match your CSV style

# Loop through the rest and merge by Date
if (length(data_list) > 1) {
  for (i in 2:length(data_list)) {
    temp_df <- data_list[[i]]
    
    # Rename columns: keep Date, change 'PX_LAST' to the original R variable name
    colnames(temp_df) <- c("Dates", raw_names[i])
    
    # Merge (full_join ensures we don't lose days if calendars differ)
    final_df <- merge(final_df, temp_df, by = "Dates", all = TRUE)
  }
}

# 8. View and Save
head(final_df)

# Optional: Save to CSV
write.csv(final_df, "EM_Indices_EUR.csv", row.names = FALSE)

print("Download complete. File saved as PMP1_EUR.csv")