# Demo of temporal_agg(): temporal aggregation of time series

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('temporal_agg.R')

# Data (Spanish Overnight Stays in Hotels, 1974.01 - 2023.12)
load('overnight_stays_spain.Rdata')

# Temporal aggregation: average
ta <- 1

# Frequency conversion: month->year (12->1)
sc <- 12

# Calling function
Z <- temporal_agg(z, 
                  ta,
                  sc)

# Converting to time series object 
z_ts <- ts(z, start = c(1974,1), frequency = 12)
Z_ts <- ts(Z, start = 1974, frequency = 1)

# Graph: quarterly case and annual case
windows()
par(mfrow = c(2,1))
plot.ts(z_ts/10000,
        main = 'Original: monthly',
        xlab = 'Months',
        ylab = '',
        col = 'red')
plot.ts(Z_ts/10000,
        main = 'Temporal aggregate: annual',
        xlab = 'Years',
        ylab = '',
        col = 'blue')
