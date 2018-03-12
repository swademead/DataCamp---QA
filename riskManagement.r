#CHAPTER 1
# Load DJ index
data("DJ")

# Show head() and tail() of DJ index
head(DJ)
tail(DJ)

# Plot DJ index
plot(DJ)

# Extract 2008-2009 and assign to dj0809
dj0809 <- DJ["2008/2009"]

# Plot dj0809
plot(dj0809)

# Load DJ constituents data
data("DJ_const")

# Apply names() and head() to DJ_const
names(DJ_const)
head(DJ_const)

# Extract AAPL and GS in 2008-09 and assign to stocks
stocks <- DJ_const["2008/2009", c("AAPL", "GS")]

# Plot stocks with plot.zoo()
plot.zoo(stocks)

# Load exchange rate data
data("GBP_USD")
data("EUR_USD")

# Plot the two exchange rates
plot(GBP_USD)
plot(EUR_USD)

# Plot a USD_GBP exchange rate
plot(1/GBP_USD, EUR_USD)

# Merge the two exchange rates GBP_USD and EUR_USD
fx <- merge(GBP_USD, EUR_USD, all = TRUE)

# Extract 2010-15 data from fx and assign to fx0015
fx0015 <- fx["2010/2015"]

# Plot the exchange rates in fx0015
plot.zoo(fx0015)

# Compute the log-returns of dj0809 and assign to dj0809_x
dj0809_x <- diff(log(dj0809))

# Plot the log-returns
plot(dj0809_x)

# Compute the log-returns of djstocks and assign to djstocks_x
djstocks_x <- diff(log(djstocks))

# Plot the two share returns
plot.zoo(djstocks_x)

# Compute the log-returns of GBP_USD and assign to erate_x
erate_x <- diff(log(GBP_USD))

# Plot the log-returns
plot(erate_x)

# Plot djstocks in four separate plots
plot.zoo(djstocks)

# Plot djstocks in one plot and add legend
plot.zoo(djstocks, plot.type = "single", col = c(1:4))
legend(julian(x = as.Date("2009-01-01")), y = 70, legend = names(DJ_const)[1:4], fill = 1:4)

# Compute log-returns and assign to djstocks_x
djstocks_x <- diff(log(djstocks))

# Plot djstocks_x in four separate plots
plot.zoo(djstocks_x)

# Plot djstocks_x with vertical bars
plot.zoo(djstocks_x, type = "h")

# Plot djx
plot(djx)

# Plot weekly log-returns of djx
plot(apply.weekly(djx, sum), type = "h")

# Plot monthly log-returns of djx
plot(apply.monthly(djx, sum), type = "h")

# Plot djreturns
plot.zoo(djreturns)

# Plot monthly log-returns of djreturns
plot.zoo(apply.monthly(djreturns, colSums), type = "h")

# Plot gold and oil prices
plot(gold)
plot(oil)

# Calculate daily log-returns
goldx <- diff(log(gold))
oilx <- diff(log(oil))

# Calculate monthly log-returns
goldx_m <- apply.monthly(goldx, sum)
oilx_m <- apply.monthly(oilx, sum)

# Merge goldx_m and oilx_m into coms
coms <- merge(goldx_m, oilx_m)

# Plot coms with vertical bars
plot.zoo(coms, type = "h")

# Make a pairwise scatterplot of coms
pairs(as.zoo(coms))

# Identify and create vector containing column names for 1, 5, 10 year yields
names(zcb)
yield_cols <- c("1.00y", "5.00y", "10.00y")

# Compute log-returns as zcb_x and plot them for same maturities
zcb_x <- diff(log(zcb))
plot.zoo(zcb_x[, yield_cols])

# Compute simple returns as zcb_x2 and plot them for same maturities
zcb_x2 <- diff(zcb)
plot.zoo(zcb_x2[, yield_cols])

# Make a vector containing the maturities                             
maturity <- (1:120)/4

# Plot the yield curve for the first day of zcb
plot(maturity, first(zcb), ylim = range(zcb), type = "l", ylab = "yield (%)", col = "red")

# Add a line for the last day of zcb
lines(maturity, last(zcb))

#CHAPTER 2
# Calculate average and standard deviation of djx
mu <- mean(djx)
sigma <- sd(djx)

# Plot histogram of djx
hist(djx, nclass = 20, probability = TRUE)

# Add the normal density as a red line to histogram
lines(djx, dnorm(djx, mu, sigma), col = "red")

# Plot non-parametric KDE of djx
plot(density(djx))

# Add the normal density as red line to KDE
lines(djx, dnorm(djx, mu, sigma), col = "red")

# Make a Q-Q plot of djx and add a red line
qqnorm(djx)
qqline(djx, col = "red")

# Calculate the length of djx as n
n = length(djx)

# Generate n standard normal variables, make a Q-Q plot, add a red line
x1 <- rnorm(n)
qqnorm(x1)
qqline(x1, col = "red")

# Generate n Student t variables, make a Q-Q plot, add a red line
x2 <- rt(n, df = 4)
qqnorm(x2)
qqline(x2, col = "red")

# Generate n standard uniform variables, make a Q-Q plot, add red line
x3 <- runif(n)
qqnorm(x3)
qqline(x3, col = "red")

# Calculate skewness and kurtosis of djx
skewness(djx)
kurtosis(djx)

# Carry out a Jarque-Bera test for djx
jarque.test(djx)

# Calculate skewness and kurtosis of djreturns 
s <- apply(djreturns, 2, skewness)
k <- apply(djreturns, 2, kurtosis)

# Plot k against s and add text labels to identify stocks
plot(s, k, type = "n")
text(s, k, names(s), cex = 0.6)

# Carry out Jarque-Bera tests for each constituent in djreturns
apply(djreturns, 2, jarque.test)

# Calculate weekly and monthly log-returns from djx_d
djx_w <- apply.weekly(djx_d,colSums)
djx_m <- apply.monthly(djx_d, colSums)

# Calculate the p-value for each series in djx_d
apply(djx_d, 2, function(v){jarque.test(v)$p.value})

# Calculate the p-value for each series in djx_w
apply(djx_w, 2, function(v){jarque.test(v)$p.value})

# Calculate the p-value for each series in djx_m
apply(djx_m, 2, function(v){jarque.test(v)$p.value})

# Calculate a 21-day moving sum of djx
djx21 <- rollapplyr(djx, width = 21, FUN = sum)[-c(1:20)]

# Calculate a 63-day moving sum of djx
djx63 <- rollapplyr(djx, width = 63, FUN = sum)[-c(1:62)]

# Merge the three series and plot
djx2 <- merge(djx, djx21, djx63, all = FALSE)
plot.zoo(djx2)

# Compute the skewness and kurtosis for each series in djx2
apply(djx2, 2, skewness)
apply(djx2, 2, kurtosis)

# Conduct the Jarque-Bera test to each series in djx2
apply(djx2, 2, jarque.test)

# Fit a Student t distribution to djx
tfit <- fit.st(djx)

# Define tpars, nu, mu, and sigma
tpars <- tfit$par.ests
nu <- tpars["nu"]
mu <- tpars["mu"]
sigma <- tpars["sigma"]

# Plot a histogram of djx
hist(djx, nclass = 20, probability = TRUE, ylim = range(0, 40))

# Compute the fitted t density at the values djx
yvals <- dt((djx - mu)/sigma, df = nu)/sigma

# Superimpose a red line to show the fitted t density
lines(djx, yvals, col = "red")

# Plot the daily log-return series in fx_d
plot.zoo(fx_d)

# Apply the Jarque-Bera test to each of the series in fx_d
apply(fx_d, 2, jarque.test)

# Plot the monthly log-return series in fx_m
plot.zoo(fx_m, type = "h")

# Apply the Jarque-Bera test to each of the series in fx_m
apply(fx_m, 2, jarque.test)

# Fit a Student t distribution to each of the series in fx_m
apply(fx_m, 2, function(v){fit.st(v)$par.ests})

# Plot the interest-rate return series zcbx_m and zcbx2_m
plot.zoo(zcbx_m, type = "h")
plot.zoo(zcbx2_m, type = "h")

# Make Q-Q plots of the 3rd component series of zcbx_m and zcbx2_m
qqnorm(zcbx_m[,3])
qqnorm(zcbx2_m[, 3])

# Compute the kurtosis of each series in zcbx_m and zcbx2_m
apply(zcbx_m, 2, kurtosis)
apply(zcbx2_m, 2, kurtosis)

# Conduct the Jarque-Bera test on each series in zcbx_m and zcbx2_m
apply(zcbx_m, 2, jarque.test)
apply(zcbx2_m, 2, jarque.test)