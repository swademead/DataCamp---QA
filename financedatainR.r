#CHAPTER 1
# Load the quantmod package
library(quantmod)

# Import QQQ data from Yahoo! Finance
QQQ <- getSymbols("QQQ", auto.assign = FALSE)

# Look at the structure of the object getSymbols created
str(QQQ)

# Look at the first few rows of QQQ
head(QQQ)

# Import QQQ data from Google Finance
getSymbols("QQQ", src = "google")

# Look at the structure of QQQ
str(QQQ)

# Import GDP data from FRED
getSymbols("GDP", src = "FRED")

# Look at the structure of GDP
str(GDP)

# Load the quantmod package
library(quantmod)

# Assign SPY data to 'spy' using auto.assign argument
spy <- getSymbols("SPY", auto.assign = FALSE)

# Look at the structure of the 'spy' object
str(spy)

# Assign JNJ data to 'jnj' using env argument
jnj <- getSymbols("JNJ", env = NULL)

# Look at the structure of the 'jnj' object
str(jnj)

# Load the Quandl package
library(Quandl)

# Import GDP data from FRED
gdp <- Quandl(code = "FRED/GDP")

# Look at the structure of the object returned by Quandl
str(gdp)

# Import GDP data from FRED as xts
gdp_xts <- Quandl(code = "FRED/GDP", type = "xts")

# Look at the structure of gdp_xts
str(gdp_xts)

# Import GDP data from FRED as zoo
gdp_zoo <- Quandl(code = "FRED/GDP", type = "zoo")

# Look at the structure of gdp_zoo
str(gdp_zoo)

# Create an object containing the Pfizer ticker symbol
symbol = "PFE"

# Use getSymbols to import the data
getSymbols(symbol, src = "google")

# Look at the first few rows of data
head(PFE)

# Create a currency_pair object
currency_pair <- "GBP/CAD"

# Load British Pound to Canadian Dollar exchange rate data
getSymbols(currency_pair, src = "oanda")

# Examine object using str()
str(GBPCAD)

# Try to load data from 190 days ago
getSymbols(currency_pair, from = Sys.Date() - 190, to = Sys.Date(), src = "oanda")

# Create a series_name object
series_name <- "UNRATE"

# Load the data using getSymbols
getSymbols(Symbols = series_name, src = "FRED")

# Create a quandl_code object
quandl_code = "FRED/UNRATE"

# Load the data using Quandl
unemploy_rate <- Quandl(quandl_code)

#CHAPTER 2
# Look at the head of DC
head(DC)

# Extract the close column
dc_close <- Cl(DC)

# Look at the head of dc_close
head(dc_close)

# Extract the volume column
dc_volume <- Vo(DC)

# Look at the head of dc_volume
head(dc_volume)

# Extract the high, low, and close columns
dc_hlc <- HLC(DC)

# Look at the head of dc_hlc
head(dc_hlc)

# Extract the open, high, low, close, and volume columns
dc_ohlcv <- OHLCV(DC)

# Look at the head of dc_ohlcv
head(dc_ohlcv)

# Download CME data for CL and BZ as an xts object
oil_data <- Quandl(code = c("CME/CLH2016", "CME/BZH2016"), type = "xts")

# Look at the column names of the oil_data object
colnames(oil_data)

# Extract the Open price for CLH2016
cl_open <- getPrice(oil_data, symbol = "CLH2016", prefer = "Open$")

# Look at January, 2016 using xts' ISO-8601 subsetting
cl_open["2016-01"]

# CL and BZ Quandl codes
quandl_codes <- c("CME/CLH2016","CME/BZH2016")

# Download quarterly CL and BZ prices
qtr_price <- Quandl(quandl_codes, collapse = "quarterly", type = "xts")

# View the high prices for both series
Hi(qtr_price)

# Download quarterly CL and BZ returns
qtr_return <- Quandl(quandl_codes, transform = "rdiff", collapse = "quarterly", type = "xts")

# View the settle price returns for both series
getPrice(qtr_return, prefer = "Settle")

# Call head on each object in data_env using eapply
data_list <- eapply(data_env, head)

# Merge all the list elements into one xts object
data_merged <- do.call(merge, data_list)

# Ensure the columns are ordered: open, high, low, close
data_ohlc <- OHLC(data_merged)

# Symbols
symbols <- c("AAPL", "MSFT", "IBM")

# Create new environment
data_env <- new.env()

# Load symbols into data_env
getSymbols(symbols, env = data_env, auto.assign = TRUE)

# Extract the close column from each object and combine into one xts object
close_data <- do.call(merge, eapply(data_env, Cl))

# View the head of close_data
head(close_data)

#CHAPTER 3
# Set the default to pull data from Google Finance
setDefaults(getSymbols, src = "google")

# Get GOOG data
getSymbols("GOOG")

# Verify the data was actually pulled from Google Finance
str(GOOG)

# Look at getSymbols.yahoo arguments
args(getSymbols.yahoo)

# Set default 'from' value for getSymbols.yahoo
setDefaults(getSymbols.yahoo, from = "2000-01-01")

# Confirm defaults were set correctly
getDefaults("getSymbols.yahoo")

# Look at the first few rows of CP
head(CP)

# Set the source for CP to FRED
setSymbolLookup(getSymbols, src = "FRED")

# Load CP data again
getSymbols("CP")

# Look at the first few rows of CP
head(CP)

# Save symbol lookup table
saveSymbolLookup("my_symbol_lookup.rda")

# Set default source for CP to "yahoo"
setSymbolLookup(CP = "yahoo")

# Verify the default source is "yahoo"
getSymbolLookup("CP")

# Load symbol lookup table
loadSymbolLookup("my_symbol_lookup.rda")

# Verify the default source is "FRED"
getSymbolLookup("CP")

# Load BRK-A data
getSymbols("BRK-A")

# Use backticks and head() to look at the loaded data
head(`BRK-A`)

# Use get() to assign the BRK-A data to an object named BRK.A
BRK.A <- get("BRK-A")

# Create BRK.A object
BRK.A <- getSymbols("BRK-A", auto.assign = FALSE)

# Create col_names object with the column names of BRK.A
col_names <- colnames(BRK.A)

# Set BRK.A column names to syntactically valid names
colnames(BRK.A) <- make.names(col_names)

# Set name for BRK-A to BRK.A
setSymbolLookup(BRK.A = list(name = "BRK-A"))

# Set name for T (AT&T) to ATT
setSymbolLookup(ATT = list(name = "T"))

# Load BRK.A and ATT data
getSymbols(c("BRK.A", "ATT"))

#CHAPTER 4
# Extract the start date of the series
start_date <- start(irregular_xts)

# Extract the end date of the series
end_date <- end(irregular_xts)

# Create a regular date sequence
regular_index <- seq(from = start_date, to = end_date, by = "day")

# Create a zero-width xts object
regular_xts <- xts(,order.by = regular_index)

# Merge irregular_xts and regular_xts
merged_xts <- merge(irregular_xts, regular_xts)

# Look at the first few rows of merged_xts
head(merged_xts)

# Use the fill argument to fill NA with their previous value
merged_filled_xts <- merge(irregular_xts, regular_xts, fill = na.locf)

# Look at the first few rows of merged_filled_xts
head(merged_filled_xts)

# Aggregate DFF to monthly
monthly_fedfunds <- apply.monthly(DFF, mean)

# Convert index to yearmon
index(monthly_fedfunds) <- as.yearmon(index(monthly_fedfunds))


# Merge FEDFUNDS with the monthly aggregate
merged_fedfunds <- merge(FEDFUNDS, monthly_fedfunds)

# Look at the first few rows of the merged object
head(merged_fedfunds)

# Look at the first few rows of merged_fedfunds
head(merged_fedfunds)

# Fill NA forward
merged_fedfunds_locf <- na.locf(merged_fedfunds)

# Extract index values containing last day of month
aligned_last_day <- merged_fedfunds_locf[index(monthly_fedfunds)]

# Fill NA backward
merged_fedfunds_locb <- na.locf(merged_fedfunds, fromLast = TRUE)

# Extract index values containing first day of month
aligned_first_day <- merged_fedfunds_locb[index(FEDFUNDS)]

# Extract index weekdays
index_weekdays <- .indexwday(DFF)

# Find locations of Wednesdays
wednesdays <- which(index_weekdays == 3)

# Create custom end points
end_points <- c(0, wednesdays, length(index_weekdays))

# Calculate weekly mean using custom end points
weekly_mean <- period.apply(DFF, end_points, mean)

# Create merged object with a Europe/London timezone
tz_london <- merge(london, chicago)

# Look at tz_london structure
str(tz_london)

# Create merged object with a America/Chicago timezone
tz_chicago <- merge(chicago, london)

# Look at tz_chicago structure
str(tz_chicago)

# Create a regular date-time sequence
regular_index <- seq(as.POSIXct("2010-01-04 09:00"), as.POSIXct("2010-01-08 16:00"), by = "30 min")

# Create a zero-width xts object
regular_xts <- xts(, order.by = regular_index)

# Merge irregular_xts and regular_xts, filling NA with their previous value
merged_xts <- merge(irregular_xts, regular_xts, fill = na.locf)

# Subset to trading day (9AM - 4PM)
trade_day <- merged_xts["T09:00/T16:00"]

# Split trade_day into days
daily_list <- split(trade_day , f = "days")

# Use lapply to call na.locf for each day in daily_list
daily_filled <- lapply(daily_list, FUN = na.locf)

# Use do.call to rbind the results
filled_by_trade_day <- do.call(rbind, daily_filled)

# Convert raw prices to 5-second prices
xts_5sec <- to.period(intraday_xts, period = "seconds", k = 5)

# Convert raw prices to 10-minute prices
xts_10min <-to.period(intraday_xts, period = "minutes", k = 10)

# Convert raw prices to 1-hour prices
xts_1hour <- to.period(intraday_xts, period = "hours", k = 1)

#CHAPTER 5
# Load AMZN.csv
getSymbols("AMZN", src = "csv")

# Look at AMZN structure
str(AMZN)

# Import AMZN.csv using read.zoo
amzn_zoo <- read.zoo("AMZN.csv", sep = ",", header = TRUE)

# Convert to xts
amzn_xts <- as.xts(amzn_zoo)

# Look at the first few rows of amzn_xts
head(amzn_xts)

# Read data with read.csv
une_data <- read.csv("UNE.csv", nrows = 5)

# Look at the structure of une_data
str(une_data)

# Read data with read.zoo, specifying index columns
une_zoo <- read.zoo("UNE.csv", index.column = c("Date", "Time"), sep = ",", header = TRUE)

# Look at first few rows of data
head(une_zoo)

# Read data with read.csv
two_symbols_data <- read.csv("two_symbols.csv", nrows = 5)

# Look at the structure of two_symbols_data
str(two_symbols_data)

# Read data with read.zoo, specifying index columns
two_symbols_zoo <- read.zoo("two_symbols.csv", split = c("Symbol", "Type"), sep = ",", header = TRUE)

# Look at first few rows of data
head(two_symbols_zoo)

# fill NA using last observation carried forward
locf <- na.locf(DGS10)

# fill NA using linear interpolation
approx <- na.approx(DGS10)

# fill NA using spline interpolation
spline <- na.spline(DGS10)

# merge into one object
na_filled <- merge(locf, approx, spline)

# plot combined object
plot(na_filled, col = c("black", "red", "green"))

# Look at the last few rows of AAPL data
tail(AAPL)

# Plot close price
plot(AAPL$AAPL.Close)

# Plot adjusted close price
plot(AAPL$AAPL.Adjusted)

# Look at first few rows aapl_yahoo
head(aapl_yahoo)

# Look at first few rows aapl_google
head(aapl_google)

# Plot difference between Yahoo adjusted close and Google close
plot(Ad(aapl_yahoo) - Cl(aapl_google))

# Plot difference between volume from Yahoo and Google
plot(Vo(aapl_yahoo) - Vo(aapl_google))

# Look at first few rows of AAPL
head(AAPL)

# Adjust AAPL for splits and dividends
aapl_adjusted <- adjustOHLC(AAPL)

# Look at first few rows of aapl_adjusted
head(aapl_adjusted)

# Download AAPL split data
splits <- getSplits("AAPL")

# Print the splits object
splits

# Download AAPL dividend data
dividends <- getDividends("AAPL")

# Look at the first few rows of dividends
head(dividends)

# Download unadjusted AAPL dividend data
raw_dividends <- getDividends("AAPL", split.adjust = FALSE)

# Look at the first few rows of raw_dividends
head(raw_dividends)

# Calculate split and dividend adjustment ratios
ratios <- adjRatios(splits = splits, dividends = raw_dividends, close = Cl(AAPL))

# Calculate adjusted close for AAPL
aapl_adjusted <- Cl(AAPL) * ratios[, "Split"] * ratios[, "Div"]

# Look at first few rows of Yahoo adjusted close
head(Ad(AAPL))

# Look at first few rows of aapl_adjusted
head(aapl_adjusted)