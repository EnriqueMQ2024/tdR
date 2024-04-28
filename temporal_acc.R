temporal_acc <- function(z, 
                         ta, 
                         sc) {
  ' Cumulative high frequency time series

  INPUT:  
  z: nx1 -> high frequency data
  ta: type of temporal aggregation 
   ta=1 -> sum (flow)
   ta=2 -> average (index)
  sc: number of high frequency data points 
  for each low frequency data point (sample conversion)
  
  OUTPUT: 
  za: nx1 accumulated time series
  
  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Plugging function
  source('acc.R')
  
  # Data dimension
  n <- length(z)
  
  # Computing implicit number of low-frequency data
  N <- trunc(n / sc)
  
  # Generating accumulation matrix
  A <- acc(ta, 
           n, 
           sc)
  
  # Computing accumulated time series
  za <- A %*% z
  
  # Output
  return(za)
}
