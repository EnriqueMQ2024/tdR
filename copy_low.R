copy_low <- function(Z, 
                     ta, 
                     sc) {
  ' Adapts a low-frequency time series to a high-frequency format
    (copy or interpolation)
  
  INPUT: 
    Z: Nx1 matrix of low frequency (lf) series, columnwise
    ta type of temporal aggregation:
      ta=1 -> copy sc times the lf data
      ta=2 -> copy sc times the mean lf data
      ta=3 -> last element (stock) ---> interpolation
      ta=4 -> first element (stock) ---> interpolation
    sc: number of high frequency data points for each low frequency data point

  OUTPUT: 
    z : nx1 copied/interpolated high frequency time series

  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Plugging function
  source('aggreg_v.R')
  
  # Dimension of input data
  N <- length(Z)
  
  # Generation of disaggregation matrix
  c <- aggreg_v(ta, 
                sc)
  
  # Enforcing c to be 1 x sc
  c <- matrix(data = c, nrow = 1)
  
  # Temporal copy-interpolation matrix
  n <- sc * N
  C <- kronecker(diag(1, N), t(c))
  
  # Transformation
  z <- C %*% Z
  
  # Output
  return(z)
}
