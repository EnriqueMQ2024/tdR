# Demo of santos_cardoso()
#         Temporal disaggregation with indicators
#         Santos-Cardoso method

# Cleaning the workspace
rm(list=ls())

# Cleaning the console
cat('\014')

# Closing the graphic windows
graphics.off()

# Expanded thegraphic window
par(mar=c(1,1,1,1))

# Plugging packages and functions
source('santos_cardoso.R')

# Data (from Santos-Cardoso, 2001)
load('consumption_income_USA.Rdata')
Y <- Yx$Y  #Y: Consumption, USA
x <- Yx$x  #x: Disposable income, USA

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
# Interval of rho for grid search
# rl <- NULL
# rl <- 0.55
rl <- c(-0.99, 0.99, 500)

# Calling the function: output is loaded in a list called res
res <- santos_cardoso(Y,
                      x,
                      ta,
                      sc,
                      type_estim,
                      opC,
                      rl)

# Graphs
source('copy_low.R')

# Time series
y_ts <- ts(res$y, start = c(1953,1), frequency = 4)
y_lo_ts <- y_ts - 2*res$y_se
y_hi_ts <- y_ts + 2*res$y_se

windows()
plot.ts(cbind(copy_low(Y, 2, sc), y_ts, y_hi_ts, y_lo_ts),
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
     xlab = expression(phi),
     ylab = '',
     col = 'red'
)
plot(res$r, 
     res$loglik, 
     type='l', 
     main = 'Log-likelihood', 
     xlab = expression(phi),
     ylab = '',
     col = 'blue'
)

# Log-like
windows()
plot(res$r, 
     res$loglik, 
     type='b', 
     main = 'Log-likelihood', 
     xlab = expression(phi),
     ylab = '',
     col = 'blue'
)
