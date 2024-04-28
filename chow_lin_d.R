# Demo of chow_lin()
#         Temporal disaggregation with indicators
#         Chow-Lin method

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('chow_lin.R')

# Data (from Bournay-Laroque, 1979)
load('output_iip_france.Rdata')
Y <- Yx$Y  #Y: GDP
x <- Yx$x  #x: IIP

# ---------------------------------------------
# Inputs

# Type of aggregation
ta <- 1   
# Frequency conversion 
sc <- 4    
# Method of estimation
type_estim <- 1
# Intercept
opC <- -1

# Interval of grid search for rho
# rl <- NULL # Default: search on [0.05 0.99] with 100 grid points
# rl <- 0.75 # Fixed value
rl <- c(-0.99, 0.999999999, 1000) # Specific range: min, max and number of grid points

# Calling the function: output is loaded in a list called res
res <- chow_lin(Y, 
                x, 
                ta, 
                sc, 
                type_estim, 
                opC, 
                rl)

# Graphs
source('copy_low.R')

# Time series
y_ts <- ts(res$y, start = c(1949,1), frequency = sc)
y_hi_ts <- ts(res$y_hi, start = c(1949,1), frequency = sc)
y_lo_ts <- ts(res$y_lo, start = c(1949,1), frequency = sc)

windows()
plot.ts(cbind(rbind(copy_low(Y, 2, sc), matrix(NA,4,1)), y_ts, y_hi_ts, y_lo_ts),
        main = '',
        xlab = 'Quarters',
        ylab = '',
        col = c('blue', 'red', 'red', 'red'),
        lty=c(1, 1,2,2),
        plot.type = "single"
)
legend('topleft',
  legend = c('Benchmark', 'Estimate', 'Estimate: Hi', 'Estimate: Lo'),
  col = c('blue', 'red', 'red', 'red'),
  lty = c(1, 1, 2, 2),
  cex = 0.8
)

# WLS / Log-like
windows()
par(mfrow = c(1, 2))
plot(res$r, 
     res$wls, 
     type='l', 
     main = 'WLS', 
     xlab = expression(rho), 
     ylab = '',
     col = 'red'
)
plot(res$r, 
     res$loglik, 
     type='l', 
     main = 'Log-likelihood', 
     xlab = expression(rho),
     ylab = '',
     col = 'blue'
)

# Log-like
windows()
plot(res$r, 
     res$loglik, 
     type='b', 
     main = 'Log-likelihood', 
     xlab = expression(rho),
     ylab = '',
     col = 'blue'
)
