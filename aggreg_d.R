# Demo of aggreg(): temporal aggregation matrix

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('aggreg.R')

# Temporal aggregation
ta <- 1

# Frequency conversion
sc <- 4

# Number of low-frequency observations
N <- 3

# Calling function
C <- aggreg(ta, 
            N,
            sc)

# Output
print(C)
