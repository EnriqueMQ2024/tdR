# Demo of moving_sum(): moving sum/average

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('moving_sum.R')
source('systematic_sampler.R')

# Data (Spanish Overnight Stays in Hotels, 1974.01 - 2023.12)
load('overnight_stays_spain.Rdata')

# Temporal aggregation: average
ta <- 1

# Frequency conversion: month->year (12->1)
sc <- 12

# Calling functions
zs <- moving_sum(z, 
                 ta, 
                 sc)

zss <- systematic_sampler(zs,
                          sc)

# Converting to time series object 
z_ts <- ts(z, start = c(1974,1), frequency = 12)
zs_ts <- ts(zs, start = c(1974,1), frequency = 12)
zss_ts <- ts(zss, start = c(1974,1), frequency = 12)

# Graph: quarterly case and annual case
windows()
par(mfrow = c(3,1))
plot.ts(z_ts/10000,
        main = 'Original',
        xlab = 'Months',
        ylab = '',
        col = 'red')
plot.ts(zs_ts/10000,
        main = 'Annual Moving Sum',
        xlab = 'Months',
        ylab = '',
        col = 'blue')
plot.ts(zss_ts/10000,
        main = 'Moving Sum: Annually Sampled',
        xy.lines = FALSE,
        xlab = 'Months',
        ylab = '',
        col = 'blue')

