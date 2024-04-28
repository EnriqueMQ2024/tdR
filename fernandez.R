fernandez <- function(Y, 
                      x, 
                      ta, 
                      sc, 
                      opC = 1) {
' Temporal disaggregation using the Fernandez method
  
   INPUT: 
    Y: Nx1 -> vector of low frequency data
    x: nxp -> matrix of high frequency indicators (without intercept)
    ta: type of disaggregation
      ta=1 -> sum (flow)
      ta=2 -> average (index)
      ta=3 -> last element (stock) ---> interpolation
      ta=4 -> first element (stock) ---> interpolation
    sc: number of high frequency data points for each low frequency data point
    type_estim: estimation method: 
      type_estim=0 -> weighted least squares     
      type_estim=1 -> maximum likelihood
    opC: 1x1 option related to intercept
      opc = -1 : pretest intercept significance
      opc =  0 : no intercept in hf model
      opc =  1 : intercept in hf model

  OUTPUT:
  res: a list containing the results
  
  REFERENCE: 
    Fernandez, R.B.(1981) "Methodological note on the estimation of time series", 
    Review of Economic and Statistics, 63(3), 471-478
  
  written by:
  Enrique M. Quilis
  
  Version 1.0 [March 2024] '  
  
  # Plugging functions
  source('fernandez_W.R')
  
  # Pretesting intercept
  if (opC == -1) {
    rex <- fernandez_W(Y, 
                       x, 
                       ta, 
                       sc, 
                       1)
    
    # Intercept: t-ratio
    ti <- rex$beta_t[1]
    if (abs(ti) < 2) {
      opC <- 0
    } else {
      opC <- 1
    }
  }
  
  # Final estimation
  res <- fernandez_W(Y, 
                     x, 
                     ta, 
                     sc, 
                     opC)
  
  # Output
  return(res)
}
