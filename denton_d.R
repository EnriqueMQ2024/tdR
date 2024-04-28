# Demo of denton(): temporal disaggregation: Denton method

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('denton.R')

# Data (from Denton, 1971)
load('denton.Rdata')
Y <- Yx$Y  #Y: GDP
x <- Yx$x  #x: IIP

# Calling the function
res <- denton(Y,
              x,
              ta = 1,   # Type of aggregation
              d = 2,    # Minimizing the volatility of d-differenced series
              sc = 4,   # Frequency conversion
              op1 = 1)  # Variant: 1=additive, 2=proportional 

# -----------------------------------------------------------------------
# Graphs

source('copy_low.R')

# Sample conversion
sc <- 4

# Converting to time series object 
Y_ts <- ts(copy_low(Y, 2, sc), start = 1, frequency = sc) 
x_ts <- ts(x, start = 1, frequency = sc)
y_ts <- ts(res$y, start = 1, frequency = sc)
u_ts <- ts(res$u, start = 1, frequency = sc)

# Graphs
windows()
par(mfrow=c(2,1))
plot.ts(cbind(x_ts, y_ts, Y_ts),
        main = '',
        xlab = 'Time',
        ylab = '',
        col = c('red', 'red', 'blue'),
        lty = c(2,1,1),
        plot.type = "single")
legend('top', 
       legend=c('Indicator', 'Estimate', 'Benchmark'),
       horiz = TRUE,
       col = c('red', 'red', 'blue'),
       lty = c(2,1,1),
       cex = 0.8,
       bty = 'n')
plot.ts(u_ts,
        main = '',
        xlab = 'Time',
        ylab = '')
legend('top', 
       legend = 'Residual',
       col = 'black',
       lty = 1,
       bty = 'n')
