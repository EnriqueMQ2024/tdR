# Demo of guerrero()
#         Temporal disaggregation with indicators
#         Guerrero method (ARIMA model-based)

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('guerrero.R')

# Data (from Guerrero, 1990)
load('gdp_iip_mexico.Rdata')
Y <- Yx$Y  #Y: GDP
x <- Yx$x  #x: IIP

# ---------------------------------------------
# Inputs

# Type of aggregation
ta <- 1   
# Frequency conversion 
sc <- 12
# Intercept
opC <- -1

# Model for w: (0,1,1)(1,0,1) [Scaled indicator w=beta*x]
rexw <- list(
  ar_reg = c(1),
  d = 1,
  ma_reg = c(1, -0.40),
  ar_sea = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.85),
  bd = 0,
  ma_sea = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -0.79),
  sigma = (4968.716)**2
)

# Model for the discrepancy: (1,2,0)(1,0,0)
# See: Martinez and Guerrero, 1995, Test, 4(2), 359-76.
rexd <- list(
  ar_reg = c(1, -0.43),
  d = 2,
  ma_reg = c(1),
  ar_sea = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.62),
  bd = 0,
  ma_sea = c(1),
  sigma = (76.95)**2
)

# Calling the function: output is loaded in a list called res
res <- guerrero(Y, 
                x, 
                ta, 
                sc, 
                rexw, 
                rexd, 
                opC)

# Graphs
source('copy_low.R')

# Time series
y_ts <- ts(res$y, start = c(1983,1), frequency = sc)
y_lo_ts <- y_ts - 2*res$y_se
y_hi_ts <- y_ts + 2*res$y_se

windows()
plot.ts(cbind(copy_low(Y, 2, sc), y_ts, y_hi_ts, y_lo_ts),
        main = '',
        xlab = 'Months',
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

# PSI-weights
windows()
par(mfrow = c(1, 2))
plot(1:(length(x)-1),
     res$PSI[-1,1], 
     type='l', 
     main = 'Indicator', 
     xlab = 'Months',
     ylab = '',
     col = 'red'
)
plot(1:(length(x)-1),
     res$PSIh[-1,1], 
     type='l', 
     main = 'Discrepancy', 
     xlab = 'Months',
     ylab = '',
     col = 'blue'
)
