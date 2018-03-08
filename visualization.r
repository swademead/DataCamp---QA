#CHAPTER 1
# Display the first few lines of the data
head(data)

# Display the column names of the data
colnames(data)

# Plot yahoo data and add title
plot(data$yahoo, main = "yahoo")

# Replot yahoo data with labels for X and Y axes
plot(data$yahoo, main = "yahoo", xlab = "date", ylab = "price")

# Plot the second time series and change title
plot(data$microsoft, main = "microsoft")

# Replot with same title, add subtitle, use bars
plot(data$microsoft, main = "microsoft", type = "h", sub = "Daily closing price since 2015")

# Change line color to red
lines(data$microsoft, col = "red")

# Plot two charts on same graphical window
par(mfrow = c(2, 1))
plot(data$yahoo, main = "yahoo")
plot(data$microsoft, main = "microsoft")

# Replot with reduced margin and character sizes
par(mfrow = c(2, 1), mex = 0.6, cex = 0.8)
plot(data$yahoo, main = "yahoo")
plot(data$microsoft, main = "microsoft")

# Plot the "microsoft" series
plot(data$microsoft, main = "Stock prices since 2015")

# Add the "dow_chemical" series in red
lines(data$dow_chemical, col = "red")

# Add a Y axis on the right side of the chart
axis(side = 4, at = pretty(data$dow_chemical))

# Add a legend in the bottom right corner
legend(x = "bottomright",
       legend = c("microsoft", "dow_chemical"),
       col = c("black", "red"),
       lty = c(1, 1)
)

# Plot the "citigroup" time series
plot(data$citigroup, main = "Citigroup")

# Create vert_line to identify January 4th, 2016 in citigroup
vert_line <- which(index(data$citigroup) == as.Date("2016-01-04"))

# Add a red vertical line using vert_line
abline(v = .index(data$citigroup)[vert_line], col = "red")

# Create hori_line to identify average price of citigroup
hori_line <- mean(data$citigroup)

# Add a blue horizontal line using hori_line
abline(h = hori_line, col = "blue")

# Create period to hold the 3 months of 2015
period <- c("2015-01/2015-03")

# Highlight the first three months of 2015 
chart.TimeSeries(data$citigroup, period.area = period)

# Highlight the first three months of 2015 in light grey
 chart.TimeSeries(data$citigroup, period.area = period, period.color = "lightgrey")

# Plot the microsoft series
plot(data$microsoft, main = "Dividend date and amount")

# Add the citigroup series
lines(data$citigroup, col = "orange", lwd = 2)

# Add a new y axis for the citigroup series
axis(side = 4, at = pretty(data$citigroup), col = "orange")

# Same plot as the previous exercise
plot(data$microsoft, main = "Dividend date and amount")
lines(data$citigroup, col = "orange", lwd = 2)
axis(side = 4, at = pretty(data$citigroup), col = "orange")

# Create the two legend strings
micro <- paste0("Microsoft div. of ", micro_div_value," on ", micro_div_date)
citi <- paste0("Citigroup div. of ", citi_div_value," on ", citi_div_date)

# Create the legend in the bottom right corner
legend(x = "bottomright", legend = c(micro, citi), col = c("black", "orange"), lty = c(1, 1))

#CHAPTER 2
# Plot Apple's stock price 
plot(data, main = "Apple stock price")

# Create a time series called rtn
rtn <- ROC(data)

# Plot Apple daily price and daily returns 
par(mfrow = c(1,2))
plot(data, main = "Apple stock price")
plot(rtn)

# Create a histogram of Apple stock returns
hist(rtn, main = "Apple stock return distribution", probability = TRUE)

# Add a density line
lines(density(rtn))

# Redraw a thicker, red density line
lines(density(rtn), col = "red", lwd = 2)

# Draw box and whisker plot for the Apple returns
boxplot(rtn)

# Draw a box and whisker plot of a normal distribution
boxplot(rnorm(1000))

# Redraw both plots on the same graphical window
par(mfrow = c(2, 1))
boxplot(rtn, horizontal = TRUE)
boxplot(rnorm(1000), horizontal = TRUE)

# Draw autocorrelation plot
acf(rtn, main = "Apple return autocorrelation")

# Redraw with a maximum lag of 10
acf(rtn, main = "Apple return autocorrelation", lag.max = 10)

# Create q-q plot
qqnorm(rtn, main = "Apple return QQ-plot")

# Add a red line showing normality
qqline(rtn, col = "red")

# Draw histogram and add red density line
hist(rtn, probability = TRUE)
lines(density(rtn), col = "red")

# Draw box and whisker plot
boxplot(rtn)

# Draw autocorrelogram
acf(rtn)

# Draw q-q plot and add a red line for normality
qqnorm(rtn)
qqline(rtn, col = "red")

# Set up 2x2 graphical window
par(mfrow = c(2, 2))

# Recreate all four plots
hist(rtn, probability = TRUE)
lines(density(rtn), col = "red")

boxplot(rtn)

acf(rtn)

qqnorm(rtn)
qqline(rtn, col = "red")