#CHAPTER 1
# View a detailed description of AirPassengers
help(AirPassengers)

# Plot AirPassengers
plot(AirPassengers)

# Plot the DJIA daily closings
plot(djia$Close)

# Plot the Southern Oscillation Index
plot(soi)

# Plot detrended y (trend stationary)
plot(diff(y))

# Plot detrended x (random walk)
plot(diff(x))

# Plot globtemp and detrended globtemp
par(mfrow = c(2,1))
plot(globtemp) 
plot(diff(globtemp))

# Plot cmort and detrended cmort
par(mfrow = c(2,1))
plot(cmort)
plot(diff(cmort))

# astsa and xts are preloaded 

# Plot GNP series (gnp) and its growth rate
par(mfrow = c(2,1))
plot(gnp)
plot(diff(log(gnp)))

# Plot DJIA closings (djia$Close) and its returns
par(mfrow = c(2,1))
plot(djia$Close)
plot(diff(log(djia$Close)))

 # Generate and plot white noise
WN <- arima.sim(model = list(order = c(0, 0, 0)), n = 200)
plot(WN)

# Generate and plot an MA(1) with parameter .9 
MA <- arima.sim(model = list(order = c(0, 0, 1), ma = 0.9), n = 200)
plot(MA)

# Generate and plot an AR(2) with parameters 1.5 and -.75
AR <- arima.sim(model = list(order = c(2, 0 , 0), ar = c(1.5, -.75)), n = 200)
plot(AR)

#CHAPTER 2
# Generate 100 observations from the AR(1) model
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 

# Plot the generated data 
plot(x)

# Plot the sample P/ACF pair
acf2(x)

# Fit an AR(1) to the data and examine the t-table
sarima(x, p = 1, d = 0, q = 0)

# astsa is preloaded

# Plot x
plot(x)

# Plot the sample P/ACF of x
acf2(x)

# Fit an AR(2) to the data and examine the t-table
sarima(x, 2, 0, 0)

# astsa is preloaded

# Plot x
plot(x)

# Plot the sample P/ACF of x
acf2(x)

# Fit an MA(1) to the data and examine the t-table

sarima(x, 0, 0, 1)
# astsa is preloaded

# Plot x
plot(x)

# Plot the sample P/ACF of x
acf2(x)

# Fit an ARMA(2,1) to the data and examine the t-table
sarima(x, 2, 0, 1)

# Fit an MA(1) to dl_varve.   
sarima(dl_varve, 0, 0, 1)
# Fit an MA(2) to dl_varve. Improvement?
sarima(dl_varve, 0, 0, 2)

# Fit an ARMA(1,1) to dl_varve. Improvement?
sarima(dl_varve, 1, 0, 1)

# Fit an MA(1) to dl_varve. Examine the residuals  
sarima(dl_varve, 0, 0, 1)

# Fit an ARMA(1,1) to dl_varve. Examine the residuals
sarima(dl_varve, 1, 0, 1)

# Calculate approximate oil returns
oil_returns <- diff(log(oil))

# Plot oil_returns. Notice the outliers.
plot(oil_returns)

# Plot the P/ACF pair for oil_returns
acf2(oil_returns)

# Assuming both P/ACF are tailing, fit a model to oil_returns
sarima(oil_returns, 1, 0, 1)

#CHAPTER 3
# Plot x
plot(x)

# Plot the P/ACF pair of x
acf2(x)

# Plot the differenced data
acf2(diff(x))

# Plot the P/ACF pair of the differenced data
plot(diff(x))

# Plot sample P/ACF of differenced data and determine model
acf2(diff(x))


# Estimate parameters and examine output
sarima(x, 2, 1, 0)

# Plot the sample P/ACF pair of the differenced data 
acf2(diff(globtemp))

# Fit an ARIMA(1,1,1) model to globtemp
sarima(globtemp, 1, 1, 1)

# Fit an ARIMA(0,1,2) model to globtemp. Which model is better?
sarima(globtemp, 0, 1, 2)

# Plot sample P/ACF pair of the differenced data
acf2(diff(x))

# Fit the first model, compare parameters, check diagnostics
sarima(x, 0, 1, 1)

# Fit the second model and compare fit
sarima(x, 0, 1, 2)

# Fit ARIMA(0,1,2) to globtemp and check diagnostics  
 sarima(globtemp, 0, 1, 2)

# Fit ARIMA(1,1,1) to globtemp and check diagnostics
sarima(globtemp, 1, 1, 1)

# Which is the better model?
"ARIMA(0, 1, 2)"

# Plot P/ACF pair of differenced data 
acf2(diff(x))

# Fit model - check t-table and diagnostics
sarima(x, 1, 1, 0)

# Forecast the data 20 time periods ahead
sarima.for(x, n.ahead = 20, p = 1, d = 1, q = 0) 
lines(y)  

# Fit an ARIMA(0,1,2) to globtemp and check the fit
sarima(globtemp, 0, 1, 2)

# Forecast data 35 years into the future
 sarima.for(globtemp, n.ahead = 35, 0, 1, 2)