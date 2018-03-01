#CHAPTER 1
# What is the current date?
Sys.Date()

# What is the current date and time?
Sys.time()

# Create the variable today
today <- Sys.Date()

# Confirm the class of today
class(today)

# Create crash
crash <- as.Date("2008-09-29")

# Print crash
crash

# crash as a numeric
as.numeric(crash)

# Current time as a numeric
as.numeric(Sys.time())

# Incorrect date format
as.Date("09/29/2008")

# Create dates from "2017-02-05" to "2017-02-08" inclusive.
dates <- c("2017-02-05", "2017-02-06", "2017-02-07", "2017-02-08")

# Add names to dates
names(dates) <- c("Sunday", "Monday", "Tuesday", "Wednesday")

# Subset dates to only return the date for Monday
dates["Monday"]

# "08,30,30"
as.Date("08,30,1930", format = "%m,%d,%Y")

# "Aug 30,1930"
as.Date("Aug 30,1930", format = "%B %d,%Y")

# "30aug1930"
as.Date("30aug1930", format = "%d%b%Y")

# char_dates
char_dates <- c("1jan17", "2jan17", "3jan17", "4jan17", "5jan17")

# Create dates using as.Date() and the correct format 
dates <- as.Date(char_dates, format = "%d%b%y")

# Use format() to go from "2017-01-04" -> "Jan 04, 17"
format(dates, format = "%b %d, %y")

# Use format() to go from "2017-01-04" -> "01,04,2017"
format(dates, format = "%m,%d,%Y")

# Dates
dates <- as.Date(c("2017-01-01", "2017-01-02", "2017-01-03"))

# Create the origin
origin <- as.Date("1970-01-01")

# Use as.numeric() on dates
as.numeric(dates)

# Find the difference between dates and origin
dates - origin

# dates
dates <- as.Date(c("2017-01-02", "2017-05-03", "2017-08-04", "2017-10-17"))

# Extract the months
months(dates)

# Extract the quarters
quarters(dates)

# dates2
dates2 <- as.Date(c("2017-01-02", "2017-01-03", "2017-01-04", "2017-01-05"))

# Assign the weekdays() of dates2 as the names()
names(dates2) <- weekdays(dates2)

# Print dates2
dates2

#CHAPTER 2
# Stock prices
apple <- 48.99
micr <- 77.93

# Apple vs Microsoft
apple > micr

# Not equals
apple != micr

# Dates - today and tomorrow
today <- as.Date(Sys.Date())
tomorrow <- as.Date(Sys.Date() + 1)

# Today vs Tomorrow
tomorrow < today

# Print stocks
stocks

# IBM range
stocks$ibm_buy <- stocks$ibm < 175 

# Panera range
stocks$panera_sell <- stocks$panera > 213 

# IBM vs Panera
stocks$ibm_vs_panera <- stocks$panera < stocks$ibm 

# Print stocks
stocks

# IBM buy range
stocks$ibm_buy_range <- stocks$ibm > 171 & stocks$ibm < 176

# Panera spikes
stocks$panera_spike <- stocks$panera > 216.5 | stocks$panera < 213.2

# Date range
stocks$good_dates <- stocks$date > as.Date("2017-01-21") & stocks$date < as.Date("2017-01-25")

# Print stocks
stocks

# IBM range
!stocks$ibm > 176

# Missing data
missing <- c(24.5, 25.7, NA, 28, 28.6, NA)

# Is missing?
is.na(missing)

# Not missing?
!is.na(missing)

# Panera range
subset(stocks, panera > 216)

# Specific date
subset(stocks, date == as.Date("2017-01-23"))

# IBM and Panera joint range
subset(stocks, ibm < 175 & panera < 216.5)

# View stocks
stocks

# Weekday investigation
stocks$weekday <- weekdays(stocks$date)

# View stocks again
stocks

# Remove missing data
stocks_no_NA <- subset(stocks, !is.na(apple))

# Apple and Microsoft joint range
subset(stocks_no_NA, apple > 117 | micr > 63)
# micr
micr <- 57.44

# Fill in the blanks

# micr
micr <- 105.67
shares <- 1

# Fill in the blanks
if( micr < 55 ) {
    print("Buy!")
} else if( micr >= 55 & micr < 75 ) {
    print("Do nothing!")
} else { 
    if( shares >= 1 ) {
        print("Sell!")
    } else {
        print("Not enough shares to sell!")
    }
}
# Microsoft test
stocks$micr_buy <- ifelse(test = stocks$micr > 60 & stocks$micr < 62, yes = 1, no = 0)

# Apple test
stocks$apple_date <- ifelse(test = stocks$apple > 117, yes = stocks$date, no = NA)

# Print stocks
stocks

# Change the class() of apple_date.
class(stocks$apple_date) <- "Date"

# Print stocks again
stocks

#CHAPTER 3
# Stock price
stock_price <- 126.34

repeat {
  # New stock price
  stock_price <- stock_price * runif(1, .985, 1.01)
  print(stock_price)
  
  # Check
  if(stock_price < 125) {
    print("Stock price is below 124.5! Buy it while it's cheap!")
    break
  }
}

# Stock price
stock_price <- 67.55

repeat {
  # New stock price
  stock_price <- stock_price * .995
  
  # Check
  if(stock_price < 66) {
    print("Stock price is below 66! Buy it while it's cheap!")
    print(stock_price)

    break
  }
  
}

# Initial debt
debt <- 5000

# While loop to pay off your debt
while (debt > 0) {
  debt <- debt - 500
  print(paste("Debt remaining", debt))
}

debt <- 5000    # initial debt
i <- 0          # x axis counter
x_axis <- i     # x axis
y_axis <- debt  # y axis

# Initial plot
plot(x_axis, y_axis, xlim = c(0,10), ylim = c(0,5000))

# Graph your debt
while (debt > 0) {

  # Updating variables
  debt <- debt - 500
  i <- i + 1
  x_axis <- c(x_axis, i)
  y_axis <- c(y_axis, debt)
  
  # Next plot
  plot(x_axis, y_axis, xlim = c(0,10), ylim = c(0,5000))
}

# debt and cash
debt <- 5000
cash <- 4000

# Pay off your debt...if you can!
while (debt > 0) {
  debt <- debt - 500
  cash <- cash - 500
  print(paste("Debt remaining:", debt, "and Cash remaining:", cash))

  if (cash == 0) {
    print("You ran out of cash!")
     break
  }
}

# Sequence
seq <- c(1:10)

# Print loop
for (value in seq) {
    print(value)
}

# A sum variable
sum <- 0

# Sum loop
for (value in seq) {
    sum <- sum + value
    print(sum)
}
# Sequence
seq <- c(1:10)

# Print loop
for (value in seq) {
    print(value)
}

# A sum variable
sum <- 0

# Sum loop
for (value in seq) {
    sum <- sum + value
    print(sum)
}

# Print out corr
corr

# Create a nested loop
for(row in 1:nrow(corr)) {
    for(col in 1:ncol(corr)) {
        print(paste(colnames(corr)[col], "and", rownames(corr)[row], 
                    "have a correlation of", corr[row,col]))
    }
}

# Print apple
apple

# Loop through apple. Next if NA. Break if above 117.
for (value in apple) {
    if(is.na(value)) {
        print("Skipping NA")
        next
    }
    
    if(value > 117) {
        print("Time to sell!")
        break
    } else {
        print("Nothing to do here!")
    }
}