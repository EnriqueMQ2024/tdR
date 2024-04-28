# Demo of di_fonzo(): multivariate temporal disaggregation using Di Fonzo

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('di_fonzo.R')

# Data (Cuevas et al., 2015)
load('regional_employment_spain.Rdata')
Y <- Yxz$Y  #Y: Regional Accounts: employment.SPAIN
x <- Yxz$x  #x: Labor Force Survey, Employment SAC. SPAIN
z <- Yxz$z  #z: Quarterly National Accounts, Employment, SAC. SPAIN

# ---------------------------------------------
# Inputs

# Type of aggregation
ta <- 2
# Frequency conversion
sc <- 4
# Model for the innovations: white noise (0), random walk (1)
op1 <- 1
# Number of high frequency indicators linked to each low frequency aggregate
M <- ncol(Y)
f <- matrix(1, 1, M)

# Multivariate temporal disaggregation
res <- di_fonzo(Y, 
                x, 
                z, 
                ta, 
                sc, 
                op1, 
                f)

# Graphs

colnames(x) <- c('North', 'South', 'Center', 'Archipelagos')

source('copy_low.R')

# Graphs
colnames(x) <- c('North', 'South', 'Center', 'Archipelagos')

source('copy_low.R')
# pdf("di_fonzo_multi_plots.pdf")
windows()
par(mfrow = c(2, 2))
for (j in 1:ncol(x)) {
  plot(x[, j], lty = 3, col = "red", xlab = "Time", ylab = "", main = paste("", colnames(x)[j]), ylim = range(c(x[, j], res$y[, j], Y[, j])), lwd = 2)
  lines(res$y[, j], lty = 1, col = "red", lwd = 2, pch = 1)
  lines(copy_low(Y[, j], 1, sc), lty = 1, col = "blue", lwd = 2)
  legend("topright", legend = c("Tracker", "Estimate", "Benchmark"), col = c("red", "red", "blue"), lwd = 2, cex = 0.75, lty = c(3,1,1))
}
mtext('Di Fonzo method', side = 3, line = -2, cex = 1, outer = TRUE, col = 'blue')
# dev.off()

windows()
par(mfrow = c(2, 2))
for (j in 1:ncol(x)) {
  plot(res$y[, j], type = 'l', col = "black", xlab = "Time", ylab = "", main = paste("", colnames(x)[j]), ylim = range(c(x[, j], res$y[, j], Y[, j])), lwd = 2)
  lines(res$y[, j]-2*res$d_y[, j], col = "red", lwd = 1, lty = 3, pch = 1)
  lines(res$y[, j]+2*res$d_y[, j], col = "red", lwd = 1, lty = 3, pch = 1)
}
mtext('Di Fonzo method', side = 3, line = -2, cex = 1, outer = TRUE, col = 'blue')
