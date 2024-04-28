low_pass_interpolation <- function(Y, 
                                   ta, 
                                   d, 
                                   sc, 
                                   lambda) {
  ' Low-pass interpolation combining filtering (HP) and benchmarking (Denton)
  
  INPUT:  
    Y -> low frequency data time series
    ta -> type of disaggregation
    d -> objective function to be minimized by Denton (seen denton())
    sc -> number of high frequency data points for each low frequency data point
    lambda -> balance between fitness and smoothness, see hp()
  
  OUTPUT:
    res: a list containing the results

  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Plugging functions
  source('hp.R')
  source('denton.R')
  source('copy_low.R')
  
  # Raw interpolation: padding Y with zeros and scaling (Y*s)
  x <- copy_low((sc*Y), 
                3, 
                sc)
  
  # Low-pass smoothing by means of Hodrick-Prescott filter
  rex1 <- hp(x,
             lambda)
  
  # Extracting HP trend
  w <- rex1$trend
  
  # Enforce consistency with low-frequency counterpart by means of benchmarking
  # (Denton, additive variant)
  # Calling the function: output is loaded in a list called rex2
  rex2 <- denton(Y,
                 w,
                 ta,
                 d,
                 sc,
                 op1 = 1)  # Additive Denton
  
  # Extracting final interpolation
  y <- rex2$y
  
  # Output
  res <- list(x = x,
              w = w,
              y = y)
  return(res)
}
