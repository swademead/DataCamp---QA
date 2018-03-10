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

#CHAPTER 3
# Create a grid
grid <- seq(from = 0, to = 1, by = 0.01)

# Initialize an empty vector for sharpe ratios
vsharpe <- rep(NA, times = length(grid) )

# Create a for loop to calculate Sharpe ratios
for(i in 1:length(grid)) {
	weight <- grid[i]
	preturns <- weight * returns_equities + (1 - weight) * returns_bonds
	vsharpe[i] <- SharpeRatio.annualized(preturns)
}

# Plot weights and Sharpe ratio
plot(grid, vsharpe, xlab = "Weights", ylab= "Ann. Sharpe ratio")
abline(v = grid[vsharpe == max(vsharpe)], lty = 3)

# Create a scatter plot
chart.Scatter(returns_bonds, returns_equities)

# Find the correlation
cor(returns_equities, returns_bonds)

# Merge returns_equities and returns_bonds 
returns <- merge(returns_equities, returns_bonds)

# Find and visualize the correlation using chart.Correlation
chart.Correlation(returns)

# Visualize the rolling estimates using chart.RollingCorrelation
chart.RollingCorrelation(returns_equities, returns_bonds, width = 24)

# Create a vector of returns 
means <- apply(returns, 2, "mean")
  
# Create a vector of standard deviation
sds <- apply(returns, 2, "sd")

# Create a scatter plot
plot(sds, means)
text(sds, means, labels = colnames(returns), cex = 0.7)
abline(h = 0, lty = 3)

# Create a matrix with variances on the diagonal
diag_cov <- diag(sds^2)

# Create a covariance matrix of returns
cov_matrix <- cov(returns)

# Create a correlation matrix of returns
cor_matrix <- cor(returns)

# Verify covariances equal the product of standard deviations and correlation
all.equal(cov_matrix[1,2], cor_matrix[1,2] * sds[1] * sds[2])

# Create a weight matrix w
w <- as.matrix(weights)

# Create a matrix of returns
mu <- as.matrix(vmeans)

# Calculate portfolio mean monthly returns
t(w) %*% mu

# Calculate portfolio volatility
sqrt(t(w) %*% sigma %*% w)

# Create portfolio weights
weights <- c(0.4, 0.4, 0.1, 0.1)

# Create volatility budget
vol_budget <- StdDev(returns, portfolio_method = "component", weights = weights)

# Make a table of weights and risk contribution
weights_percrisk <- cbind(weights, vol_budget$pct_contrib_StdDev)
colnames(weights_percrisk) <- c("weights", "perc vol contrib")

# Print the table
weights_percrisk

#CHAPTER 4
# Verify the class of returns 
class(returns)

# Investigate the dimensions of returns
dim(returns)

# Create a vector of row means
ew_preturns <- rowMeans(returns)

# Cast the numeric vector back to an xts object
ew_preturns <- xts(ew_preturns, order.by = index(returns))

# Plot ew_preturns
plot.zoo(ew_preturns)

# Load tseries
library(tseries)

# Create an optimized portfolio of returns
opt <- portfolio.optim(returns)

# Create pf_weights
pf_weights <- opt$pw

# Assign asset names
names(pf_weights) <- colnames(returns)

# Select optimum weights opt_weights
opt_weights <- pf_weights[pf_weights >= 0.01]

# Barplot of opt_weights
barplot(opt_weights)

# Print expected portfolio return and volatility
opt$pm
opt$ps
 
# Create portfolio with target return of average returns 
pf_mean <- portfolio.optim(returns, pm = mean(returns))

# Create portfolio with target return 10% greater than average returns
pf_10plus <- portfolio.optim(returns, pm = 1.1 * mean(returns))

# Print the standard deviations of both portfolios
pf_mean$ps
pf_10plus$ps

# Calculate the proportion increase in standard deviation
(pf_10plus$ps - pf_mean$ps) / (pf_mean$ps)

# Create vectors of maximum weights
max_weights1 <- rep(1, ncol(returns))
max_weights2 <- rep(0.1, ncol(returns))
max_weights3 <- rep(0.05, ncol(returns))

# Create an optimum portfolio with max weights of 100%
opt1 <- portfolio.optim(returns, reshigh = max_weights1)

# Create an optimum portfolio with max weights of 10%
opt2 <- portfolio.optim(returns, reshigh = max_weights2)

# Create an optimum portfolio with max weights of 5%
opt3 <- portfolio.optim(returns, reshigh = max_weights3)

# Calculate how many assets have a weight that is greater than 1% for each portfolio
sum(opt1$pw > .01)
sum(opt2$pw > .01)
sum(opt3$pw > .01)

# Print portfolio volatilites 
opt1$ps
opt2$ps
opt3$ps

# Calculate each stocks mean returns
stockmu <- colMeans(returns)

# Create a grid of target values
grid <- seq(from = 0.01, to = max(stockmu), length.out = 50)

# Create empty vectors to store means and deviations
vpm <- vpsd <- rep(NA, length(grid))

# Create an empty matrix to store weights
mweights <- matrix(NA, 50, 30)

# Create your for loop
for(i in 1:length(grid)) {
  opt <- portfolio.optim(x = returns, pm = grid[i])
  vpm[i] <- opt$pm
  vpsd[i] <- opt$ps
  mweights[i, ] <- opt$pw
}

# Create weights_minvar as the portfolio with the least risk
weights_minvar <- mweights[vpsd == min(vpsd), ]

# Calculate the Sharpe ratio
vsr <- (vpm - 0.0075) / vpsd

# Create weights_max_sr as the portfolio with the maximum Sharpe ratio
weights_max_sr <- mweights[vsr == max(vsr)]

# Create barplot of weights_minvar and weights_max_sr
par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
barplot(weights_minvar[weights_minvar > 0.01])
barplot(weights_max_sr[weights_max_sr > 0.01])

# Create returns_estim 
returns_estim <- window(returns, start = "1991-01-01", end = "2003-12-31")

# Create returns_eval
returns_eval <- window(returns, start = "2004-01-01", end = "2015-12-31")

# Create vector of max weights
max_weights <- rep(0.1, ncol(returns))

# Create portfolio with estimation sample 
pf_estim <- portfolio.optim(returns_estim, reshigh = max_weights)

# Create portfolio with evaluation sample
pf_eval <- portfolio.optim(returns_eval, reshigh = max_weights)

# Create a scatter plot
plot(pf_estim$pw, pf_eval$pw)
abline(h = 0, b = 1, lty = 3)

# Create returns_pf_estim
returns_pf_estim <- Return.portfolio(returns_estim, pf_estim$pw, rebalance_on = "months")


# Create returns_pf_eval
returns_pf_eval <- Return.portfolio(returns_eval, pf_estim$pw, rebalance_on = "months")

# Print a table for your estimation portfolio
table.AnnualizedReturns(returns_pf_estim)

# Print a table for your evaluation portfolio
table.AnnualizedReturns(returns_pf_eval)