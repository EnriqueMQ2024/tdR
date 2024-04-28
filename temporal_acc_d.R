# Demo of temporal_acc(): temporal accumulation matrix, function and looped version

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('temporal_acc.R')
source('temporal_acc_loop.R')

# Data (Spanish Overnight Stays in Hotels, 1974.01 - 2023.12)
load('overnight_stays_spain.Rdata')

# Frequency conversion: month->year (12->1)
sc <- 12

# Type of aggregation
ta <- 1

# Calling function
zc <- temporal_acc(z, 
                   ta,
                   sc)

# Calling function
zcc <- temporal_acc_loop(z, 
                         ta,
                         sc)

# Converting to time series object
z_ts <- ts(z, start = c(1974,1), frequency = 12)
zc_ts <- ts(zc, start = c(1974,1), frequency = 12)
zcc_ts <- ts(zcc, start = c(1974,1), frequency = 12)

# Graphs
windows()
plot.ts(cbind(z_ts/10000, zc_ts/10000),
        main = '',
        xlab = 'Months',
        ylab = '',
        col = c('red', 'blue'),
        lty=1:2,
        plot.type = "single")
legend('topleft', 
       legend=c('Original', 'Cumulant'),
       col = c('red', 'blue'),
       lty=1:2, 
       cex=0.8)

