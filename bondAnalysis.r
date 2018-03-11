#CHAPTER 1
# Create pv
pv <- 100

# Create r
r <- 0.1

# Calculate fv1
fv1 <- pv * (1 + r)

# Calculate fv2
fv2 <- fv1 * (1 + r)

# Calculate pv1
pv1 <- fv1 / (1 + r)

# Calculate pv2
pv2 <- fv2/ (1+ r)^2

# Print pv1 and pv2
pv1
pv2

# Create vector of cash flows
cf <- c(5, 5, 5, 5, 105)

# Convert to data frame
cf <- data.frame(cf)
cf

# Add column t to cf
cf$t <- as.numeric(rownames(cf))

# Calculate pv_factor
cf$pv_factor <- 1 / (1 + 0.06)^cf$t

# Calculate pv
cf$pv <- cf$cf * cf$pv_factor

# Calculate the bond price
sum(cf$pv)

# Create function
bondprc <- function(p, r, ttm, y) {
  cf <- c(rep(p * r, ttm - 1), p * (1 + r))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + y)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  sum(cf$pv)
}

# Verify prior result
bondprc(100, 0.05, 5, 0.06)

#CHAPTER 2
# Load Quandl package
library(Quandl)

# Obtain Moody's Baa index data
baa <- Quandl("FED/RIMLPBAAR_N_M")

# Identify 9/30/16 yield
baa_yield <- subset(baa, baa$Date == "2016-09-30")

# Convert yield to decimals and view
baa_yield <- baa_yield$Value / 100
baa_yield

# Value bond
bondprc(p = 100, r = 0.05, ttm = 5, y = baa_yield)

# Generate prc_yld
prc_yld <- seq(0.02, 0.4, by = 0.01)

# Convert prc_yld to data frame
prc_yld <- data.frame(prc_yld)

# Calculate bond price given different yields
for (i in 1:nrow(prc_yld)) {
     prc_yld$price[i] <- bondprc(100, 0.10, 20, prc_yld$prc_yld[i])  
}

# Plot P/YTM relationship
plot(prc_yld,
     type = "l",
     col = "blue",
     main = "Price/YTM Relationship")

# Load quantmod package
library(quantmod)

# Obtain Treasury yield data
t10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)

# Subset data
t10yr <- t10yr["2006-01/2016-09"]

# Plot yields
plot(x = index(t10yr),
     y = t10yr$DGS10,
     xlab = "Date",
     ylab = "Yield (%)",
     type = "l",
     col = "red",
     main = "10-Year US Treasury Yields")

# Examine first and last six elements in spread
head(spread)
tail(spread)

# Calculate spread$diff
spread$diff <- -(spread$aaa - spread$baa) * 100

# Plot spread
plot(x = spread$date,
     y = spread$diff,
     type = "l",
     xlab = "Date",
     ylab = "Spread (bps)",
     col = "red",
     main = "Baa - Aaa Spread")

# Value bond using 5% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.05)

# Value bond using 7% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.07)

# Value bond using 6% yield
bondprc(p = 100, r = 0.05, ttm = 5, y = 0.06)

# Create cash flow vector
cf <- c(-95.79, 5, 5, 5, 5, 105)

# Create bond valuation function
bval <- function(i, cf,
     t=seq(along = cf))
     sum(cf / (1 + i)^t)

# Create ytm() function using uniroot
ytm <- function(cf) {
    uniroot(bval, c(0, 1), cf = cf)$root
}

# Use ytm() function to find yield
ytm(cf)
