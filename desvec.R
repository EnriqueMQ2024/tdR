desvec <- function(a, m){
  ' Des-vectorizing vector (un-stacking columns, inverse of vec())

  INPUT:
  a : nm x 1 vector
  OUTPUT:
  A : n x m matrix (belonging to class matrix)
  
  Enrique M. Quilis
  Version 1.0 [March 2024]'
  
  # Forming matrix
  A <- matrix(a, 
              ncol = m)
  
  # Output
  return(A)
}