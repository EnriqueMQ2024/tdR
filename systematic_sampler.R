systematic_sampler <- function(z,
                               sc,
                               code_NA = 0){
  ' Systematic sampling of a high-frequency time series

  OUTPUT: zs: nx1 sampled time series
  
  INPUT:  
  z: nx1 -> vector of high frequency data
  sc: number of high frequency data points for each low frequency data point (sample conversion)
  
  Enrique M. Quilis
  Version 2.1 [February 2024]'
  
  # Data dimension
  n <- length(z)
  
  # Pre-allocation
  zs <- z * code_NA
  
  # Number of observations to be sampled
  N <- trunc(n/sc)
  
  # Index of observations to be sampled
  I <- seq(1,N) * sc
  zs[I] <- z[I]
  
  # Output
  return(zs)
}