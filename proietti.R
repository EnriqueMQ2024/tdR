proietti <- function(Y, 
                     x, 
                     ta, 
                     sc, 
                     type_estim, 
                     opC, 
                     rl) {
  
  ' Temporal disaggregation using the Proietti ADL(1,1)-based method

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
    
  REQUIRED: 
    forecast
  
  REFERENCES:
    Proietti, T. (2006) "Temporal disaggregation by state space methods:
    dynamic regression methods revisited", Econometrics Journal, 9, 357-372
  
  Enrique M. Quilis
  Version 1.0 [March 2024]'
  
  # Plugging functions
  source('santos_cardoso.R')
  
  # Only one indicator is considered
  p <- ncol(x)
  
  if (p > 1) {
    stop('*** Sorry: only one indicator!! ***')
  }
  
  # Estimating initial condition for x by means of backcasting
  
  # Flipping x
  xa <- rev(x)
  
  # ARIMA estimation and forecast
  rex <- forecast::auto.arima(xa, seasonal = FALSE)
  rexf <- forecast::forecast(rex, 1)

  # Forming x as x(t) and x(t-1), completing x(t-1) using the backcast
  x <- cbind(x, c(rexf$mean, x[1:(length(x)-1)]))
  
  # Pretesting intercept
  if (opC == -1) {
    tex <- santos_cardoso(Y, x, ta, sc, type_estim, 1, rl)
    ti <- tex$beta_t[1]
    if (abs(ti) < 2) {
      opC <- 0
    } else {
      opC <- 1
    }
  }
  
  # Final estimation
  res <- santos_cardoso(Y, x, ta, sc, type_estim, opC, rl)
  res$meth <- 'Proietti'
  
  # Long-run gain
  res$beta_lr <- (sum(res$beta[2:3]) / (1-res$phi))
  
  # Output
  return(res)
}
