# Demo of vec(): Vectorize matrix stacking columns and
#  desvec(): creating a matrix from a vector

# Clean workspace
rm(list=ls())

# Cleaning console
cat('\014')

# Closing graphic windows
graphics.off()

# Plugging function
source('vec.R')
source('desvec.R')

# Creating matrix 3x3
A <- matrix(1:12, nrow = 3)
print(A)

# Vectorize: 9x1
a <- vec(A)
print(a)

# Des-vectorize as new matrix 
AA <- desvec(a,
             2)
print(AA)
