#CHAPTER 1
# Get SPY from yahoo
getSymbols("SPY", 
           from = "2000-01-01", 
           to = "2016-06-30", 
           src =  "yahoo", 
           adjust =  TRUE)

# Plot the closing price of SPY
plot(Cl(SPY))

# Plot the closing prices of SPY
plot(Cl(SPY))

# Add a 200-day SMA using lines()
lines(SMA(Cl(SPY), n = 200), col = "red")

#CHAPTER 2
# Load the quantstrat package
library(quantstrat)

# Create initdate, from, and to strings
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- "2015-12-31"

# Set the timezone to UTC
Sys.setenv(TZ = "UTC")

# Set the currency to USD 
currency("USD")

# Load the quantmod package
library(quantmod)

# Retrieve SPY from yahoo
getSymbols("SPY", src = "yahoo", from = from, to = to, adjust = TRUE)

# Use stock() to initialize SPY and set currency to USD
stock("SPY", currency = "USD")

# Define your trade size and initial equity
tradesize <- 100000
initeq <- 100000

# Define the names of your strategy, portfolio and account
strategy.st <- "firststrat"
portfolio.st <- "firststrat"
account.st <- "firststrat"

# Remove the existing strategy if it exists
rm.strat(strategy.st)

# Initialize the portfolio
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")

# Initialize the account
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)

# Initialize the orders
initOrders(portfolio.st, initDate = initdate)

# Store the strategy
strategy(strategy.st, store = TRUE)

#CHAPTER 3
# Create a 200-day SMA
spy_sma <- SMA(Cl(SPY), n = 200)

# Create an RSI with a 3-day lookback period
spy_rsi <- RSI(Cl(SPY), n = 3)

# Plot the closing prices of SPY
plot(Cl(SPY))

# Overlay a 200-day SMA
lines(SMA(Cl(SPY), n = 200), col = "red")

# What kind of indicator?
"trend"

# Plot the closing price of SPY
plot(Cl(SPY))

# Plot the RSI 2
plot(RSI(Cl(SPY), n = 2))

# What kind of indicator?
"reversion"

# Add a 200-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the SMA function
              name = "SMA", 
              
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 200), 
              
              # Label your indicator SMA200
              label = "SMA200")

# Add a 50-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the SMA function
              name = "SMA", 
              
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 50), 
              
              # Label your indicator SMA50
              label = "SMA50")

# Add an RSI 3 indicator to strategy.st
add.indicator(strategy = strategy.st, 
              
              # Add the RSI 3 function
              name = "RSI", 
              
              # Create a lookback period
              arguments = list(x = quote(Cl(mktdata)), n = 3), 
              
              # Label your indicator RSI_3
              label = "RSI_3")

# Write the RSI_avg function
RSI_avg <- function(price, n1, n2) {
  
  # RSI 1 takes an input of the price and n1
  rsi_1 <- RSI(price = price, n = n1)
  
  # RSI 2 takes an input of the price and n2
  rsi_2 <- RSI(price = price, n = n2)
  
  # RSI_avg is the average of rsi_1 and rsi_2
  RSI_avg <- (rsi_1 + rsi_2)/2
  
  # Your output of RSI_avg needs a column name of RSI_avg
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}

# Add this function as RSI_3_4 to your strategy with n1 = 3 and n2 = 4
add.indicator(strategy.st, name = "RSI_avg", arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4), label = "RSI_3_4")

# Declare the DVO function
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  
  # Compute the ratio between closing prices to the average of high and low
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# Add the DVO indicator to your strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126),
              label = "DVO_2_126")

# Use applyIndicators to test out your indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))

# Subset your data between Sep. 1 and Sep. 5 of 2013
test_subset <- test["2013-09-1/2013-09-05"]