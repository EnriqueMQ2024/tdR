temporal_agg <- function(y,
                         ta,
                         sc){
  'Temporal aggregation of a time series

  INPUT:  
  y: nx1 -> high frequency time series
  ta: type of temporal aggregation 
   ta=1 -> sum (flow)
   ta=2 -> average (index)
   ta=3 -> last element (stock) ---> interpolation
   ta=4 -> first element (stock) ---> interpolation
  n: number of high frequency data
  sc: number of high frequency data points for each low frequency 
  data point (sample conversion)
  
  OUTPUT: 
  Y: Nx1 vector of low frequency data, N=trunc(n/sc)

  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Plugging function  
  source('aggreg.R')
  
  # Number of high frequency data
  n <- length(y)
  
  # Computes the number of low frequency points.
  # Low frequency periods should be complete
  N <- trunc(n / sc)
  C <- aggreg(ta, N, sc)
  
  # Number of required extrapolations (if any)
  # If npred>0, the npred final observations do not play any role
  npred <- n - sc * N  
  
  if (npred > 0) {
    C <- cbind(C, matrix(0, nrow = N, ncol = npred))
  } 
  
  # Temporal aggregation
  Y <- C %*% y
  
  # Output     
  return(Y)
}
