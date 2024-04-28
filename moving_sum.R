moving_sum <- function(z, 
                       ta,
                       sc){
  
  'Moving sum of sc periods
  
  INPUT:
  z: nx1 vector of high frequency data
  ta: type of temporal aggregation
  ta=1 -> sum (flow)
  ta=2 -> average (index)
  sc: length of the moving sum (first sc-1 observations are NA)

  OUTPUT:
  zS: nx1 time series
  
  LIBRARY: moving_sum_matrix()
  
  NOTE: 
  The U(B)=1+B+B**2+...+B**(h-1) filter performs a moving sum of h consecutive 
  elements of a time series vector. 
  Its matrix form has n columns (since it is applied to the whole vector) 
  and (n-h+1) rows, due to the missing values induced by the absence of (h-1) 
  initial conditions (note that U(B) has degree h-1, not h)
  
  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Plugging functions
  source('moving_sum_matrix.R')
  
  # Dimension of input data
  n <- length(z)
  
  # Calling matrix to perform moving sum
  S <- moving_sum_matrix(sc,
                         n)
  
  # Transforming to moving average (if required)
  switch(ta,
         '1' = {
           # Do nothing
         },
         '2' = {
           # Moving average
           S <- S / sc
         },
         {
           { stop('*** ERROR: ta MUST BE 1 OR 2 ***') }
         }
  )
  
  #Transforming z
  zs <- S %*% z
  
  # Taking into account first (sc-1) missing values as zeros
  aux <- matrix(data = NA, nrow = (sc-1), ncol = 1)
  zs <- rbind(aux, zs)
  
  # Output
  return(zs)
}