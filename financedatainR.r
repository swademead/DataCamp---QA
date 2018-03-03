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