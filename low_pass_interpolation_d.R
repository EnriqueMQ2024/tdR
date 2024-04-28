# Demo of low_pass_interpolation(): combining HP and Denton for interpolation

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('low_pass_interpolation.R')

# Data: Annual Spanish GDP, 1995-2023
load('gdp_spain.Rdata')

# Sample conversion
sc <- 4

# Hodrick-Prescott parameter
lambda <- 1600

# --------------------------------------------------------------------
# Denton parameters

# Type of aggregation
ta <- 2

# Minimizing the volatility of d-differenced series
d <- 1

# Calling function
res <- low_pass_interpolation(Z,
                              ta,
                              d,
                              sc,
                              lambda)

# Converting to time series object
source('copy_low.R')

Z_ts <- ts(copy_low(Z, 1, sc), start = c(1995,1), frequency = sc)
z_ts <- ts(res$y, start = c(1995,1), frequency = sc)
w_ts <- ts(res$w, start = c(1995,1), frequency = sc)

# Graphs
windows()
plot.ts(cbind(Z_ts, w_ts, z_ts),
        main = 'Low-pass interpolation',
        xlab = 'Quarters',
        ylab = '',
        col = c('blue', 'red', 'red'),
        lty = c(1,2,1),
        plot.type = 'single')
legend('topleft', 
       legend=c('Naive', 'Intermediate', 'Final'),
       col = c('red', 'blue'),
       lty=1:2, 
       cex=0.8)

