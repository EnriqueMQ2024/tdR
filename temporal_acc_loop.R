temporal_acc_loop <- function(z, 
                              ta, 
                              sc) {
  ' Cumulative high frequency time series: looped version

  INPUT:  
  z: nxk -> vector of high frequency data
  ta: type of temporal aggregation 
   ta=1 -> sum (flow)
   ta=2 -> average (index)
  sc: number of high frequency data points for each 
  low frequency data points
  
  OUTPUT: 
  zc: nxk accumulated vector time series
  
  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Data dimension
  n <- length(z)
  
  # Computing implicit number of low-frequency data
  N <- trunc(n / sc)
  
  # Time index for systematic sampling
  ts <- seq(1,N)*sc

  # Pre-allocation of the accumulated series
  zc <- numeric(n)
  zc[1] <- z[1]
  
  # Sampling index: sc, 2*sc, 3*sc, ...
  rho <- rep(1, n)
  rho[ts] <- 0

  # Time loop
  for (t in seq(2,n)){
    zc[t] <- rho[t-1] * zc[t-1] + z[t]
  }
  
  # Output
  return(zc)
}
