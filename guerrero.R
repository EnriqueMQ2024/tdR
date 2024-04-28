guerrero <- function(Y, 
                     x, 
                     ta, 
                     sc, 
                     rexw, 
                     rexd, 
                     opC) {
  
  ' Temporal disaggregation using the Guerrero method

 INPUT:
  Y: Nx1 -> vector of low frequency data
  x: nxp -> matrix of high frequency indicators (without intercept)
  ta: type of disaggregation
    ta=1 -> sum (flow)
    ta=2 -> average (index)
    ta=3 -> last element (stock) -> interpolation
    ta=4 -> first element (stock) -> interpolation
  sc: number of high frequency data points for each low frequency data point
  rexw, rexd -> list containing the parameters of the ARIMA model for the indicator
    and the discrepancy, respectively (see the calT() function)
  opC: 1x1 option on the intercept:
    opc = -1 : pretest intercept significance
    opc =  0 : no intercept in hf model
    opc =  1 : intercept in hf model

  OUTPUT:
  A list res containing the results
  
  REFERENCES:
  - Guerrero, V. (1990) "Temporal disaggregation of time series: an 
    ARIMA-based approach", International Statistical Review, 58, 29-46
  - Martinez, J. and V. Guerrero, (1995), "A Recursive ARIMA-Based Procedure for 
    Disaggregating a Time Series Variable Using Concurrent Data", Test, 4(2), 359-76.
  
  Enrique M. Quilis
  Version 1.0 [March 2024]'
  
  # Plugging functions
  source('guerrero_W.R')
  
  # Pretesting intercept
  if (opC == -1) {
    rex <- guerrero_W(Y, x, ta, sc, rexw, rexd, 1)
    ti <- rex$beta_t[1]
    if (abs(ti) < 2) {
      opC <- 0
    } else {
      opC <- 1
    }
  }
  
  # Final estimation
  res <- guerrero_W(Y, x, ta, sc, rexw, rexd, opC)
  
  # Output  
  return(res)
}
