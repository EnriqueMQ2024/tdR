# Demo of copy_low(): from low-frequency format to a high-frequency format

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('copy_low.R')

# Data (from Bournay-Laroque, 1979)
load('output_iip_FRANCE.Rdata')
Y <- Yx$Y  #Y: GDP

# Type of interpolation
ta <- 1

# Sample conversion
sc <- 4

# Calling function
y <- copy_low(Y, 
              ta, 
              sc)

# Converting to time series object 
Y_ts <- ts(Y, start = 1949, frequency = 1)
y_ts <- ts(y, start = c(1949,1), frequency = 4)

# Graph: annual case and (interpolated) quarterly case 
windows()
par(mfrow = c(2,1))
plot.ts(Y_ts,
        main = 'Value Added. Textile industries: Annual',
        xlab = 'Years',
        ylab = '',
        col = 'blue')
plot.ts(y_ts,
        main = 'Value Added. Textile industries: Quarterly copy',
        xlab = 'Quarters',
        ylab = '',
        col = 'red')
