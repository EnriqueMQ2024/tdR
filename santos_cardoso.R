santos_cardoso <- function(Y, 
                           x, 
                           ta, 
                           sc, 
                           type_estim, 
                           opC, 
                           rl) {
  ' Temporal disaggregation using the Santos-Cardoso method

 INPUT:
  Y: Nx1 -> vector of low frequency data
  x: nxp -> matrix of high frequency indicators (without intercept)
  ta: type of disaggregation
    ta=1 -> sum (flow)
    ta=2 -> average (index)
    ta=3 -> last element (stock) -> interpolation
    ta=4 -> first element (stock) -> interpolation
  sc: number of high frequency data points for each low frequency data point
  type_estim: estimation method: 
    type_estim=0 -> Weighted Least Squares (WLS)
    type_estim=1 -> Log-likelihood
  opC: 1x1 option on the intercept:
    opc = -1 : pretest intercept significance
    opc =  0 : no intercept in hf model
    opc =  1 : intercept in hf model
  rl: innovational parameter
    rl = []: 0x0 -> rl=[0.05 0.99], 50 points grid
    rl: 1x1 -> fixed value for the rho parameter
    rl: 1x3 -> [r_min r_max n_grid] search is performed on this range, 
             using a grid with n_grid points

  OUTPUT:
    A list res containing the results
  
  REFERENCES: 
    Santos, J.M.C. and Cardoso, F.(2001) "The Chow-Lin method
  using dynamic models",Economic Modelling, 18, 269-280
    Di Fonzo, T. (2002) "Temporal disaggregation of economic time series: 
  towards a dynamic extension", Dipartimento di Scienze Statistiche, 
  Universita di Padova, Working Paper n. 2002-17
  
  Enrique M. Quilis
  Version 1.0 [March 2024]'
  
  # Plugging functions
  source('santos_cardoso_W.R')
  
  # Pretesting intercept
  if (opC == -1) {
    rex <- santos_cardoso_W(Y, x, ta, sc, type_estim, 1, rl)
    ti <- rex$beta_t[1]
    if (abs(ti) < 2) {
      opC <- 0
    } else {
      opC <- 1
    }
  }
  
  # Final estimation
  res <- santos_cardoso_W(Y, x, ta, sc, type_estim, opC, rl)
  
  # Added for printing output
  res$rho <- res$phi
  
  # Output  
  return(res)
}
