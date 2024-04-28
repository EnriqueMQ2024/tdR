acc <- function(ta, 
                n, 
                sc) {
  ' Generates a temporal accumulation matrix nxn
  
  INPUT:  
  ta: type of temporal aggregation 
   ta=1 -> sum (flow)
   ta=2 -> average (index)
  n: number of high frequency data
  sc: number of high frequency data points to accumulate

  OUTPUT: 
  A: nxn accumulation matrix

  written by:
    Enrique M. Quilis
    Version 1.0 [February 2024]'
  
  # Implicit number of low-frequency data
  N <- trunc(n / sc)
  
  # Remaining last observations (if any)
  npred <- n - sc * N
  
  # Expanding N if npred>0
  if (npred > 0) {
    N <- N + 1
  }
  
  # Auxiliary matrix
  A1 <- matrix(0, nrow = sc, ncol = sc)
  A1[lower.tri(A1, diag = TRUE)] <- 1
  
  # Selecting accumulation or averaging
  if (ta == 1) {
    # Do nothing
  } else if (ta == 2) {
    A1 <- A1 / sc
  } else {
    stop("*** ERROR: ta MUST BE 1 OR 2 ***")
  }
  
  # Accumulation matrix
  A <- kronecker(diag(N), A1)
  
  # Adjusting the accumulation matrix to the real dimension of input data
  A <- A[1:n, 1:n]
  
  # Output
  return(A)
}
