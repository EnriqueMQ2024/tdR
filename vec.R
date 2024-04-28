vec <- function(A) {
  ' Vectorizing matrix (stacking columns)

  INPUT:
  A : nxm matrix
  OUTPUT:
  a : nm x 1 vector (belonging to class matrix)
  
  Enrique M. Quilis
  Version 1.0 [March 2024]'
  
  # Vectorize the matrix
  a <- c(A)
  # Format as matrix m x 1
  a <- matrix(a, ncol = 1)
  
  # Output
  return(a)
}