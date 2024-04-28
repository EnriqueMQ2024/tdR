dif <- function(d = 1,
                n) {
  'Difference operator (1-B)**d in matrix form nxn

  INPUT:
  d: degree of differencing, must be in [0,2]
  n: dimension of filter matrix

  OUTPUT:
  D: nxn difference operator in matrix form
  
  REQUIRES:
  pracma::

  NOTE: 
  D:nxn --> The d first observations assume d initial conditions = 0
  
  Enrique M. Quilis
  Version 2.0 [February 2024]'
  
  # Transforming d as char type
  dd <- as.character(d)
  
  switch(dd,
         '0' = { # Levels
           D <- diag(n)
         },
         '1' = { # First differences
           aux <- rep(-1, n-1)
           D <- diag(n) + pracma::Diag(aux, -1)
         },
         '2' = { # Second differences
           aux1 <- rep(-2, n-1)
           aux2 <- rep(1, n-2)
           D <- diag(n) + pracma::Diag(aux1, -1) + pracma::Diag(aux2, -2)
         },
         stop('*** ERROR: d MUST BE 0, 1 OR 2 ***')
  )
  
  # Output
  return(D)
}