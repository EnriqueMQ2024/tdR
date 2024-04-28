hp <- function(z, 
               lambda = 1600) {
  '  Hodrick-Prescott filtering. Two-sided filter in matrix form
  
  INPUT:
  z: nx1 vector of observed time series
  lambda: 1x1 weight in the penalty function
  
  OUTPUT:
  res: a list containing the results
  
  REFERENCE:
  Hodrick, R. and Prescott, E C. (1997). "Postwar U.S. Business Cycles: An Empirical Investigation". 
  Journal of Money, Credit, and Banking. 29 (1): 1-16.
  
  Enrique M. Quilis
  Version 1.0 [March 2024]'
  
  # Plugging functions
  source('dif.R')
  
  # Data dimension
  n <- nrow(z)
  
  # Second difference matrix
  D <- dif(2, n)
  D <- D[3:n, ]  # Removing the first two rows
  
  # Matrix form of the filter (non inverted)
  H <- diag(n) + lambda * t(D) %*% D
  
  # Computing long-term trend
  z_trend <- solve(H, z)
  
  # Computing cycle as the deviation from estimated trend
  z_cycle <- z - z_trend
  
  # Filter in matrix form
  Hhp <- solve(H)
  
  # Generating output
  res <- list(
    trend = z_trend,
    cycle = z_cycle,
    filter = Hhp
  )
  
  return(res)
}
