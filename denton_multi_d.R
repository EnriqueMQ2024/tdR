# Demo of denton_multi(): multivariate temporal disaggregation using Denton

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('denton_multi.R')

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
# Minimizing the volatility of d-differenced series
d <- 2
# Additive (1) or proportional (2) variant [optional, default=1]
op1 <- 1

# Multivariate temporal disaggregation
res <- denton_multi(Y, 
                    x, 
                    z, 
                    ta, 
                    sc, 
                    d, 
                    op1)

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
mtext('Multivariate Denton method', side = 3, line = -2, cex = 1, outer = TRUE, col = 'blue')