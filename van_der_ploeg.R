van_der_ploeg <- function(y, 
                          S, 
                          A, 
                          a = NULL) {
  
  ' Reconciliation by means of QL optimization (LS estimation)

  INPUT:  
    y: kx1 vector of unbalanced variables (initial estimates) [Prior: mean]
    S: kxk VCV of initial estimates [Prior: VCV]
    A: kxm matrix of linear constraints
    a: 1xm vector of autonomous terms related to the linear constraints
  
  OUTPUT:
    res: a list containing the results [Posterior]

  NOTE: The vector a is optional. If it is not explicitly included, the function
  assumes a=0
  
  REFERENCE:
  Van der Ploeg, F. (1982)"Reliability and the adjustment of sequences of large
  economic accounting matrices", Journal of the Royal Statistical Society, series A, 
  145(2), 169-194. 
 
  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Data dimension
  k <- nrow(A)  #Number of variables
  m <- ncol(A)  #Number of constraints
  
  # ------------------------------------------------------------------------
  # Constraints: independent term
  if (is.null(a)) {
    a <- t(as.matrix(rep(0, m)))
  }
  
  # Discrepancy
  disc <- (t(A) %*% y) - t(a)
  
  # ------------------------------------------------------------------------
  # Lagrange multipliers
  AUX <- solve(t(A) %*% S %*% A)  
  lambda <- AUX %*% disc
  
  # ------------------------------------------------------------------------
  # LS balanced estimation [Posterior]
  
  # Levels [Mean]
  SA <- S %*% A
  z <- y - SA %*% lambda
  
  # VCV
  AUX1 <- A %*% AUX %*% t(A)
  Sz <- S - (S %*%  AUX1 %*% S)
  
  # ------------------------------------------------------------------------
  #  LOADING STRUCTURE
  res <- list(disc = disc,
              lambda = lambda,
              z = z,
              Sz = Sz)
  
  # Output
  return(res)
}
