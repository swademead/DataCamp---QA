#CHAPTER 1
# Load the package
library(PortfolioAnalytics)

# Load the data
data(indexes)

# Subset the data
index_returns <- indexes[, 1:4]

# Print the head of the data
head(index_returns)

# Create the portfolio specification
port_spec <- portfolio.spec(colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Solve the optimization problem
opt <- optimize.portfolio(index_returns, portfolio = port_spec, optimize_method = "ROI")

# Print the results of the optimization
print(opt)

# Extract the optimal weights
extractWeights(opt)

# Chart the optimal weights
chart.Weights(opt)

# Create the portfolio specification
port_spec <- portfolio.spec(assets = colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to maximize portfolio mean return
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

# Add an objective to minimize portfolio variance
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "var", risk_aversion = 10)

# Solve the optimization problem
opt <- optimize.portfolio(R = index_returns, portfolio = port_spec, optimize_method = "ROI")

#CHAPTER 2
# Get the column names of the returns data
asset_names <- colnames(asset_returns)

# Create a portfolio specification object using asset_names
port_spec <- portfolio.spec(asset_names)

# Get the class of the portfolio specification object
class(port_spec)

# Print the portfolio specification object
print(port_spec)

# Add the weight sum constraint
port_spec <- add.constraint(portfolio = port_spec, type = "weight_sum", min_sum = 1, max_sum = 1)

# Add the box constraint
port_spec <- add.constraint(portfolio = port_spec, type = "box", min = c(rep(0.1, 5), rep(0.05, 8)), max = 0.4)

# Add the group constraint
port_spec <- add.constraint(portfolio = port_spec, type = "group", groups = list(c(1,5, 7, 9, 10, 11), c(2, 3, 4, 6, 8, 12)), group_min = 0.4, group_max = 0.6)

# Print the portfolio specification object
print(port_spec)

# Add a return objective to maximize mean return
port_spec <- add.objective(portfolio = port_spec, type = "return", name = "mean")

# Add a risk objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Add a risk budget objective
port_spec <- add.objective(portfolio = port_spec, type = "risk_budget", name = "StdDev", min_prisk = 0.05, max_prisk = 0.1)

# Print the portfolio specification object
print(port_spec)

# Run a single period optimization using random portfolios as the optimization method
opt <- optimize.portfolio(R = asset_returns, portfolio = port_spec, optimize_method = "random", rp = rp, trace = TRUE)

# Print the output of the single-period optimization
print(opt)

# Run the optimization backtest with quarterly rebalancing
opt_rebal <- optimize.portfolio.rebalancing(R = asset_returns, portfolio = port_spec, optimize_method = "random", rp = rp, trace = TRUE, search_size = 1000, rebalance_on = "quarters", training_period = 60, rolling_window = 60)

# Print the output of the optimization backtest
print(opt_rebal)

# Extract the objective measures for the single period optimization
extractObjectiveMeasures(opt)

# Extract the objective measures for the optimization backtest
extractObjectiveMeasures(opt_rebal)

# Extract the optimal weights for the single period optimization
extractWeights(opt)

# Chart the weights for the single period optimization
chart.Weights(opt)

# Extract the optimal weights for the optimization backtest
extractWeights(opt_rebal)

# Chart the weights for the optimization backtest
chart.Weights(opt_rebal)