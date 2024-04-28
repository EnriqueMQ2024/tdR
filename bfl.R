bfl <- function(Y, 
                ta, 
                d, 
                sc) {
  ' Temporal disaggregation using the Boot-Feibes-Lisman method
  
  INPUT:
  Y: Nx1 low frequency time series
  ta: type of disaggregation
   ta=1 -> sum (flow)
   ta=2 -> average (index)
   ta=3 -> last element (stock) ---> interpolation
   ta=4 -> first element (stock) ---> interpolation
  d: objective function to be minimized: volatility of ...
   d=0 -> levels
   d=1 -> first differences
   d=2 -> second differences
  sc: number of high frequency data points for each low frequency data point (sample conversion)

  OUTPUT:
    res: a list containing the results
  
  REFERENCE:
    Boot, J.C.G., Feibes, W. and Lisman, J.H.C. (1967), "Further methods of derivation of quarterly 
    figures from annual data", Applied Statistics, 16(1), 65-75.

  Enrique M. Quilis
  Version 1.0 [February 2024]'  
  
  # Plugging functions
  source('stram_wei.R')
  
  # Starting timer
  t0 <- Sys.time()
  
  # Data dimension
  N <- length(Y)  # Low frequency
  n <- sc * N     # High frequency
  
  # Generation of VCV matrix of high-frequency stationary series
  v <- diag(n - d)
  
  # Calling Stram-Wei procedure under hypothesis y~I(d)=ARIMA(0,d,0)
  rex <- stram_wei(Y, 
                   ta, 
                   d, 
                   sc, 
                   v)
  
  # Extracting SW estimate
  y <- rex$y
  rm(rex)
  
  # Output
  res <- list(
    meth = 'Boot-Feibes-Lisman',
    N = N,
    ta = ta,
    sc = sc,
    npred = 0,  # No extrapolations are performed
    d = d,
    y = y,
    et = as.numeric(Sys.time() - t0)
  )
  
  return(res)
}
