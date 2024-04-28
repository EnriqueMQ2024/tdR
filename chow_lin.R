chow_lin <- function(Y, 
                     x, 
                     ta, 
                     sc, 
                     type_estim, 
                     opC, 
                     rl) {
  '  Temporal disaggregation using the Chow-Lin method
  
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
    rl: innovational parameter
      rl = []: 0x0 -> rl=[0.05 0.99], 50 points grid
      rl: 1x1 -> fixed value of rho parameter
      rl: 1x3 -> [r_min r_max n_grid] search is performed
                  on this range, using a n_grid points grid
   
    OUTPUT:
      res: a list containing the results

    REFERENCES: 
    - Chow, G. and Lin, A.L. (1971) "Best linear unbiased 
    distribution and extrapolation of economic time series by related 
    series", Review of Economic and Statistics, 53(4), 372-375.
    - Bournay, J. and Laroque, G. (1979) "Reflexions sur la methode 
    d_elaboration des comptes trimestriels", Annales de l_INSEE, 36, 3-30.
 
    Enrique M. Quilis
    Version 1.0 [February 2024]'
  
  # Plugging functions
  source('chow_lin_W.R')
  
  # Data dimension
  N <- length(Y)    # Size of low-frequency input
  n <- nrow(x)      # Size of p high-frequency inputs (without intercept)
  p <- ncol(x)
  
  # Pretesting intercept
  if (opC == -1) {
    rex <- chow_lin_W(Y, x, ta, sc, type_estim, 1, rl)
    ti <- rex$beta_t[1]
    if (abs(ti) < 2) {
      opC <- 0
    } else {
      opC <- 1
    }
  }
  
  # Final estimation
  res <- chow_lin_W(Y, x, ta, sc, type_estim, opC, rl)
  
  # Output
  return(res)
}
