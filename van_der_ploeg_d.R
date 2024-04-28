# Demo of van_der_ploeg(): Reconciliation using QL optimization (LS estimation)

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('van_der_ploeg.R')

# Unbalanced cross-section vector
y <- c(220.00, 130.00, 200.00, 100.00, 450.00, 70.00, 120.00, 221.00)
y <- as.matrix(y)
k <- length(y)

# Linear constraints
A <- matrix(c(1	,	 0,
              1	,	 0,
              1	,	 1,
              1	,	 0,
              -1	,	 0,
              -1	,	 0,
              -1	,	 0,
              -1	,	-1),
            ncol = 2,
            byrow = TRUE)

# VCV matrix of estimates
s <- c(10, 2, 25, 55, 0, 15, 10, 12)
Aux1 <- diag(sqrt(s))

# Correlation matrix
C <- matrix(0, nrow = k, ncol = k)
C[1, 3] <- C[3, 1] <- 0.5
C <- C + diag(rep(1, k))

# Final S matrix
S <- Aux1 %*% C %*% Aux1

# van der Ploeg balancing
res <- van_der_ploeg(y,
                     S,
                     A)

# Results

# Check
cat("\n*** INITIAL AND FINAL DISCREPANCIES ***\n\n")
cbind(t(A) %*% y, t(A) %*% res$z)

# Revision (as %)
p <- 100 * ((res$z - y) / y)

# Final results
cat("\n*** INITIAL ESTIMATE, FINAL ESTIMATE, REVISION AS %, INITIAL VARIANCES, FINAL VARIANCES ***\n\n")
cbind(y, res$z, p, diag(S), diag(res$Sz))

# Graphs
sv <- diag(res$Sz)
s <- diag(S)
windows()
par(mfrow=c(4,2))
for (j in 1:k) {
  if (s[j] == 0) {
    x <- seq(min(y[j], res$z[j])*0.9, max(y[j], res$z[j])*1.1, length.out = 1000)
    plot(x, rep(0, length(x)), type = "l", xlab = "", ylab = "", main = paste("Variable", j))
    points(y[j], 0, col = "red", pch = 19, cex = 1.5)
    legend("topright", legend = "Absolute tight prior", pch = 19, col = "red", bty = "n")
  } else {
    x <- seq(min(y[j], res$z[j])*0.8, max(y[j], res$z[j])*1.2, length.out = 1000)
    y0 <- dnorm(x, mean = y[j], sd = sqrt(s[j]))
    y1 <- dnorm(x, mean = res$z[j], sd = sqrt(sv[j]))
    plot(x, y0, type = "l", xlab = "", ylab = "", ylim = c(0, max(y0, y1)), main = paste("Variable", j))
    lines(x, y1, col = "red")
    if (j == 2) {
      legend("topright", legend = c("Prior", "Posterior"), col = c("black", "red"), lty = 1)
    }
  }
}
