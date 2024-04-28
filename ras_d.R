# Demo of ras(): Bi-proportional reconciliation of 2x2 tables

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('ras.R')

#-------------------------------------------------------------------------
# BENCHMARK (T=0)
#-------------------------------------------------------------------------

# Unbalanced IO matrix
F0 <- matrix(c(50, 30, 20, 100, 50, 50, 0, 20, 30), 
             nrow = 3, 
             byrow = FALSE)

# Total by column
x0 <- matrix(c(200, 300, 200), 
             nrow = 1)

#-------------------------------------------------------------------------
# UPDATE (T=1)
#-------------------------------------------------------------------------
# Total output by row
u <- matrix(c(160, 150, 120), 
            nrow = 3)

# Total intermediate output by column
v <- matrix(c(100, 250, 80), 
            nrow = 1)

# Total output
x1 <- matrix(c(200, 400, 300), 
             nrow = 1)

# Graphics
opG <- 1

# RAS reconciliation
F1 <- ras(F0, 
          x0, 
          x1, 
          v, 
          u, 
          opG)

