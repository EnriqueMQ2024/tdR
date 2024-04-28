# Demo of bal(): Proportional adjustment to a given total

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('bal.R')

# Unbalanced time series (read columnwise)
Y <- matrix(c(120,    20,      55,
              130,    22,      66,
              190,    33,      99,
              170,    35,      110),
            ncol = 3,
            byrow = TRUE)

# Transversal constraint
z <- as.matrix(c(210,
                 208,
                 300,
                 310))

# Balancing
res <- bal(Y,
           z)

# -----------------------------------------------------------------------------------
# Graphs
Yb <- res$Yb
z_ini <- apply(Y, 1, sum)

Y_ts <- ts(ts(Y, start = 2000, frequency = 1))
Yb_ts <- ts(ts(Yb, start = 2000, frequency = 1))
z_ts <- ts(ts(z, start = 2000, frequency = 1))
z_ini_ts <- ts(ts(z_ini, start = 2000, frequency = 1))

# Set up plot
windows()
par(mfrow = c(1, 2))

# Plot
ts.plot(cbind(Y_ts, Yb_ts),
        main = 'Components',  
        sub = '',         # Empty string for submain text
        ylab = '',
        xlab = '',
        lwd = 2,
        lty = c(2,1),
        col = c('blue', 'red'))
# Add legend
legend("topleft", 
       legend = c("Initial", "Adjusted"), 
       col = c('blue', 'red'), 
       lty = 1, 
       lwd = 2,
       cex = 0.75)

ts.plot(cbind(z_ini_ts, z_ts),
        main = 'Total',
        sub = '',         # Empty string for submain text
        ylab = '',
        xlab = '',
        lwd = 2,
        lty = c(2,1),
        col = c('blue', 'red'))
# Add legend
legend("topleft", 
       legend = c("Initial", "Constraint"), 
       col = c('blue', 'red'), 
       lty = 1, 
       lwd = 2,
       cex = 0.75)
