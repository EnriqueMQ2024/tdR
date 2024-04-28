# Demo of bfl(): temporal disaggregation using the Boot-Feibes-Lisman method

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
library(rio)
source('bfl.R')

# Data: Annual Spanish GDP, 1995-2023
load('gdp_spain.Rdata')

# Calling the function
res <- bfl(Z,
           ta = 1,    # Type of aggregation
           d = 1,     # Number of unit roots (high frequency time series)
           sc = 12 )  # Frequency conversion

# Graphs
sc <- 12
z_hf <- ts(res$y, start = c(1995, 1), frequency = sc)

source('copy_low.R')
Z_lf <- ts(copy_low(Z, 2, sc),
           start = c(1995, 1),
           frequency = 12)

# Graphs
windows()
plot.ts(cbind(z_hf, Z_lf),
        main = 'High frequency frequency esimates',
        xlab = 'Months',
        ylab = '',
        col = c('red', 'blue'),
        lty=1:2,
        plot.type = "single")
legend('topleft', 
       legend=c('BFL', 'Naive'),
       col = c('red', 'blue'),
       lty=1:2, 
       cex=0.8)

