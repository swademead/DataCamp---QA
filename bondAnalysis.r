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
