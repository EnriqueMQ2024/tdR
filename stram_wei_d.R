# Demo of stram_wei(): temporal disaggregation using the Stram-Wei method

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('stram_wei.R')

# Loading Y: Spain's Exports of Goods at constant (1995) prices, 1995-2016.
# and v: (n-d)x(n-d) VCV matrix of high frequency stationary series
load('exports_spain.Rdata')
Z <- Zv_list$Z  #Z: Exports
v <- Zv_list$v  #v: VCV

# Type of aggregation
ta <- 1

# Number of unit roots (high frequency time series)
d <- 2

# Frequency conversion
sc <- 4

# Calling the function
res <- stram_wei(Z,
                 ta,
                 d,
                 sc,
                 v )

# Graphs
source('copy_low.R')

z_hf <- ts(res$y, start = c(1995, 1), frequency = sc)
Z_lf <- ts(copy_low(Z, 2, sc),
           start = c(1995, 1),
           frequency = sc)

windows()
plot.ts(cbind(z_hf, Z_lf),
        main = 'High frequency frequency esimates',
        xlab = 'Quarters',
        ylab = '',
        col = c('red', 'blue'),
        lty=1:2,
        plot.type = "single")
legend('topleft', 
       legend=c('Stram-Wei', 'Naive'),
       col = c('red', 'blue'),
       lty=1:2, 
       cex=0.8)
