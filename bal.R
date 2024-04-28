bal <- function(Y,
                z){
  'Proportional adjustment of Y to a given total z
   
  INPUT:  
    Y: nxM matrix - unbalanced time series
    z: nx1 vector - transversal constraint 
  
  OUTPUT: 
    Yb: nxM matrix - balanced time series
  
  Enrique M. Quilis
  Version 1.0 [February 2024]
'
  
  # Plugging functions
  source('vec.R')
  source('desvec.R')
  
  # Data dimension
  n <- nrow(Y)
  m <- ncol(Y)
  nz <- length(z)
  
  # H1 -> Transversal aggregator
  #       Generate H1: n x nM
  H1 <- kronecker(matrix(1, 1, m), diag(n))
  
  #---------------------------------------------------------------
  #       Generate discrepancies disc: n x 1
  disc <- z - H1 %*% (as.vector(Y))
  
  #---------------------------------------------------------------
  #       Generate weights matrix w: nxM
  w <- Y / kronecker(matrix(1, 1, m), apply(Y, 1, sum))
  
  #---------------------------------------------------------------
  #       Generate balanced series in stacked form
  aux <- t(as.vector(Y)) + t(as.vector(w)) * as.vector(kronecker(matrix(1, 1, m), disc))
  aux <- t(aux)
  
  #---------------------------------------------------------------
  #       De-stacking balanced series
  Yb <- matrix(aux, nrow = n, ncol = m, byrow = FALSE)
  
  # Output
  res <- list(Yb = Yb,
              H1 = H1,
              disc = disc,
              w = w,
              aux = aux)
  
  return(res)
}
