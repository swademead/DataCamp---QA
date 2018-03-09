#CHAPTER 1
# Define ko_pep 
ko_pep <- ko/pep

# Make a time series plot of ko_pep
plot.zoo(ko_pep)
  
# Add as a reference, a horizontal line at 1
abline(h = 1)  

# Define the vector values
values <- c(4000, 4000, 2000)

# Define the vector weights
weights <- values/sum(values)

# Print the resulting weights
weights

# Define marketcaps
 marketcaps <- c(5, 8, 9, 20, 25, 100, 100, 500, 700, 2000)
  
# Compute the weights
weights <- marketcaps/sum(marketcaps)
  
# Inspect summary statistics
summary(weights)
  
# Create a barplot of weights
barplot(weights)

# Vector of initial value of the assets
in_values <- c(1000, 5000, 2000)
  
# Vector of final values of the assets
fin_values <-c(1100, 4500, 3000)

# Weights as the proportion of total value invested in each assets
weights <-in_values/sum(in_values)

# Vector of simple returns of the assets 
returns <- (fin_values - in_values)/in_values
  
# Compute portfolio return using the portfolio return formula
preturns <- sum(weights*returns)
 
# Load package PerformanceAnalytics 
library(PerformanceAnalytics)

# Print the first and last six rows of prices
head(prices)
tail(prices)

# Create the variable returns using Return.calculate()  
 returns <- Return.calculate(prices)
  
# Print the first six rows of returns. Note that the first observation is NA, because there is no prior price.
head(returns, n = 6)
  
# Remove the first row of returns
returns <- returns[-1, ]

# Create the weights
eq_weights <- c(0.5, 0.5)

# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(R = returns, weights = eq_weights)

# Create a portfolio rebalancing monthly 
pf_rebal <- Return.portfolio(R = returns, weights = eq_weights, rebalance_on = "months")

# Plot the time-series
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(pf_bh)
plot.zoo(pf_rebal)

# Create the weights
eq_weights <- c(0.5, 0.5)

# Create a portfolio using buy and hold
pf_bh <- Return.portfolio(returns, weights = eq_weights, verbose = TRUE)

# Create a portfolio that rebalances monthly
pf_rebal <- Return.portfolio(returns, weights = eq_weights, rebalance_on = "months", verbose = TRUE )

# Create eop_weight_bh
eop_weight_bh <- pf_bh$EOP.Weight

# Create eop_weight_rebal
eop_weight_rebal <- pf_rebal$EOP.Weight

# Plot end of period weights
par(mfrow = c(2, 1), mar=c(2, 4, 2, 2))
plot.zoo(eop_weight_bh$AAPL)
plot.zoo(eop_weight_rebal$AAPL)

#CHAPTER 2
# Convert the daily frequency of sp500 to monthly frequency: sp500_monthly
sp500_monthly <- to.monthly(sp500)

# Print the first six rows of sp500_monthly
head(sp500_monthly, n = 6)

# Create sp500_returns using Return.calculate using the closing prices
sp500_returns <- Return.calculate(sp500_monthly$sp500.Close)

# Time series plot
plot.zoo(sp500_returns)

# Produce the year x month table
table.CalendarReturns(sp500_returns)

# Compute the mean monthly returns
mean(sp500_returns)

# Compute the geometric mean of monthly returns
mean.geometric(sp500_returns)

# Compute the standard deviation
sd(sp500_returns)

# Compute the annualized risk free rate
annualized_rf <- (1 + rf)^12 - 1

# Plot the annualized risk free rate
plot.zoo(annualized_rf)

# Compute the series of excess portfolio returns 
sp500_excess <- sp500_returns - rf

# Compare the mean
mean(sp500_returns)
mean(sp500_excess)

# Compute the Sharpe ratio
sp500_sharpe <- mean(sp500_excess) / sd(sp500_returns)

# Compute the annualized mean
Return.annualized(sp500_returns)

# Compute the annualized standard deviation
StdDev.annualized(sp500_returns)

# Compute the annualized Sharpe ratio: ann_sharpe
ann_sharpe <- SharpeRatio.annualized(sp500_returns)

# Compute all of the above at once using table.AnnualizedReturns()
table.AnnualizedReturns(sp500_returns)

# Calculate the mean, volatility, and sharpe ratio of sp500_returns
returns_ann <- Return.annualized(sp500_returns)
sd_ann <- StdDev.annualized(sp500_returns)
sharpe_ann <- SharpeRatio.annualized(sp500_returns, rf)

# Plotting the 12-month rolling annualized mean
chart.RollingPerformance(R = sp500_returns, width = 12, FUN = "Return.annualized")
abline(h = returns_ann)

# Plotting the 12-month rolling annualized standard deviation
chart.RollingPerformance(R = sp500_returns, width = 12, FUN = "StdDev.annualized")
abline(h = sd_ann)

# Plotting the 12-month rolling annualized Sharpe ratio
chart.RollingPerformance(R = sp500_returns, width = 12, FUN = "SharpeRatio.annualized", Rf= rf)
abline(h = sharpe_ann)

# Fill in window for 2008
sp500_2008 <- window(sp500_returns, start = "2008-01-01", end = "2008-12-31")

# Create window for 2014
sp500_2014 <- window(sp500_returns, start = "2014-01-01", end = "2014-12-31")

# Plotting settings
par(mfrow = c(1, 2) , mar=c(3, 2, 2, 2))
names(sp500_2008) <- "sp500_2008"
names(sp500_2014) <- "sp500_2014"

# Plot histogram of 2008
chart.Histogram(sp500_2008, methods = c("add.density", "add.normal"))

# Plot histogram of 2014
chart.Histogram(sp500_2014, methods = c("add.density", "add.normal"))

#  Compute the skewness 
  skewness(sp500_daily)
  skewness(sp500_monthly)

# Compute the excess kurtois 
kurtosis(sp500_daily)
kurtosis(sp500_monthly)
   
   # Calculate the SemiDeviation
SemiDeviation(sp500_monthly)

# Calculate the value at risk
VaR(sp500_monthly, p = 0.025)
VaR(sp500_monthly, p = 0.05)

# Calculate the expected shortfall
ES(sp500_monthly, p = 0.025)
ES(sp500_monthly, p = 0.05)

# Table of drawdowns
table.Drawdowns(sp500_monthly)

# Plot of drawdowns
chart.Drawdown(sp500_monthly)