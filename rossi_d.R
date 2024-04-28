# Demo of the Rossi method: two-step multivariate temporal disaggregation with transversal constraint

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('rossi.R')

# Data (Cuevas et al., 2015)
load('regional_employment_spain.Rdata')
Y <- Yxz$Y  #Y: Regional Accounts: employment.SPAIN
x <- Yxz$x  #x: Labor Force Survey, Employment SAC. SPAIN
z <- Yxz$z  #z: Quarterly National Accounts, Employment, SAC. SPAIN

#--------------------------------------------------------
#       General parameters

# Type of aggregation
ta <- 2

# Frequency conversion 
sc <- 4

# Step 1: temporal disaggregation method
op_method <- 3

# Multivariate temporal disaggregation
res <- rossi(Y, 
             x, 
             z, 
             ta, 
             sc, 
             op_method)

# Graphs
colnames(x) <- c('North', 'South', 'Center', 'Archipelagos')

source('copy_low.R')
# pdf("denton_multi_plots.pdf")
windows()
par(mfrow = c(2, 2))
for (j in 1:ncol(x)) {
  plot(x[, j], lty = 3, col = "red", xlab = "Time", ylab = "", main = paste("", colnames(x)[j]), ylim = range(c(x[, j], res$y[, j], Y[, j])), lwd = 2)
  lines(res$y[, j], lty = 1, col = "red", lwd = 2, pch = 1)
  lines(copy_low(Y[, j], 1, sc), lty = 1, col = "blue", lwd = 2)
  legend("topright", legend = c("Tracker", "Estimate", "Benchmark"), col = c("red", "red", "blue"), lwd = 2, cex = 0.75, lty = c(3,1,1))
}
# dev.off()
mtext('Rossi method', side = 3, line = -2, cex = 1, outer = TRUE, col = 'blue')

windows()
par(mfrow = c(2, 2))
for (j in 1:ncol(x)) {
  plot(res$y_prelim[, j], lty = 3, col = "red", xlab = "Time", ylab = "", main = paste("", colnames(x)[j]), ylim = range(c(x[, j], res$y[, j], Y[, j])), lwd = 2)
  lines(res$y[, j], lty = 1, col = "red", lwd = 2, pch = 1)
  legend("topright", legend = c("Preliminary", "Final"), col = c("red", "red"), lwd = 2, cex = 0.75, lty = c(3,1))
}
# dev.off()
mtext('Rossi method', side = 3, line = -2, cex = 1, outer = TRUE, col = 'blue')