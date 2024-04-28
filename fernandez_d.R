# Demo of fernandez()
#         Temporal disaggregation with indicators
#         Fernandez method

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('fernandez.R')

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
# Intercept
opC <- -1

# Calling the function: output is loaded in a structure called res
res <- fernandez(Y, 
                 x, 
                 ta, 
                 sc, 
                 opC)

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
