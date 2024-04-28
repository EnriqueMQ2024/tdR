# Demo of dif(): difference operator in matrix form

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Expanded plots window
par(mar=c(1,1,1,1))

# Plugging libraries and functions
source('dif.R')

d <- 2
n <- 5
D <- dif(d, 
         n)

print(D)

