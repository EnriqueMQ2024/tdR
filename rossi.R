rossi <- function(Y, 
                  x, 
                  z, 
                  ta, 
                  sc, 
                  op_method)
{
  'Multivariate temporal disaggregation using the Rossi method

  INPUT:
    Y: NxM -> M series of low frequency data with N observations
    x: nxM -> M series of high frequency data with n observations
    z: nx1 -> high frequency transversal constraint
    ta: type of disaggregation
      ta=1 -> sum (flow)
      ta=2 -> average (index)
      ta=3 -> last element (stock) ---> interpolation
      ta=4 -> first element (stock) ---> interpolation
    sc: number of high frequency data points for each low frequency data point
    op_method: univariate temporal disaggregation method for preliminary estimates
      1 -> Fernandez
      2 -> Chow-Lin (optimized for rl=[], see chow_lin() function)
      3 -> Litterman (optimized for rl=[], see litterman() function)
      Intercept is pretested: opC = -1 and MLE

  OUTPUT:
    res: a list containing the results
  
  NOTE:
    Requires pracma:: 
  
  REFERENCEs:
  - Rossi, N. (1982) "A note on the estimation of disaggregate
    time series when the aggregate is known", Review of Economics and Statistics,
    64(4), 695-696.
  - Di Fonzo, T. (1994) "Temporal disaggregation of a system of
    time series when the aggregate is known: optimal vs. adjustment methods",
    INSEE-Eurostat Workshop on Quarterly National Accounts, Paris, December.

  Enrique M. Quilis
  Version 1.0 [April 2024]'
  
  # Starting timer
  t0 <- Sys.time()
  
  # Plugging functions
  source('chow_lin.R')
  source('fernandez.R')
  source('litterman.R')
  source('vec.R')
  source('desvec.R')
  
  #--------------------------------------------------------
  #       Initial assignment
  
  x_ini <- x
  rm(x)
  
  #--------------------------------------------------------
  #       Checking dimension
  
  dim_Y <- dim(Y)
  N <- dim_Y[1]
  M <- dim_Y[2]
  
  dim_x_ini <- dim(x_ini)
  n <- dim_x_ini[1]
  
  # Number of extrapolations (must be zer0)
  npred <- n - sc*N
  if (npred != 0){
    stop('*** Check data dimension: no extrapolations are allowed!! ***')
  }
  
  # =========================================================================
  # STEP 1: PRELIMINARY ESTIMATES BY MEANS OF UNIVARIATE METHODS
  # =========================================================================
  
  x <- matrix(0, nrow = n, ncol = M)
  e <- matrix(0, nrow = n, ncol = M)
  opC <- -1
  rl <- NULL
  type_estim <- 1 #MLE
  
  for (j in 1:M) {
    # Enforcing appropriate matrix form
    Y1 <- as.matrix(Y[,j])
    x1 <- as.matrix(x_ini[,j])
    
    # Selecting method for preliminary estimation
    oop_method <- as.character(op_method)
    switch (oop_method,
            '1' = {
              rex <- chow_lin(Y1, x1, ta, sc, type_estim, opC, rl) # ML estimation
            },
            '2' = {
              rex <- fernandez(Y1, x1, ta, sc, opC)
            },
            '3' = {
              rex <- litterman(Y1, x1, ta, sc, type_estim, opC, rl) # ML estimation
            }
    )
    # Storing results
    x[,j] <- rex$y
    e[,j] <- rex$u
  }
  
  # VCV of residuals
  S <- cov(e)
  
  # =========================================================================
  # STEP 2: FINAL ESTIMATES: MULTIVARIATE SOLUTION
  # =========================================================================
  
  # CONSTRAINT MATRICES
  # Required:
  # H1 ---> transversal
  # H2 ---> longitudinal
  
  # Generate H1: (n-h1) x nM
  H1 <- kronecker(matrix(1, 1, M), diag(n))
  
  # Generate H2: NM x nM
  C <- aggreg(ta,
              N,
              sc)
  
  H2 <- kronecker(diag(M), C)
  
  # Generate H: (n+NM) x nM
  H <- rbind(H1, 
             H2)
  
  # PREPARING DATA MATRICES
  # x_diag
  # Y_big,  Y_e
  # X_diag, X_e
  
  #--------------------------------------------------------
  #       Generate x_big: nM x 1
  
  x_big <- vec(x)
  
  #--------------------------------------------------------
  #       Generate Y_big: NM x 1
  
  Y_big <- vec(Y)
  
  # --------------------------------------------------------
  #       Generate Y_e: n+NM x 1
  #
  #     	It is column vector containing the transversal
  #       constraint and all the observations
  #       on the low frequency series
  #  		  according to: Y_e = [ z Y1 Y2 ... YM]' = [z Y_big]'
  
  Y_e <- rbind(z,
               Y_big)
  
  # =========================================================================
  # STEP 3: FINAL SOLUTION
  # =========================================================================
  
  U_e <- Y_e - H %*% x_big
  
  # Calculate W
  W <- kronecker(S, diag(n))
  
  # Calculate the inverse of W
  Wi <- solve(W)
  
  # Calculate Vi using pinv
  Vi <- pracma::pinv(H %*% Wi %*% t(H))
  
  # Calculate y_big
  y_big <- x_big + Wi %*% t(H) %*% Vi %*% U_e
  
  # Series y columnwise y: nxM
  y <- desvec(y_big, M)
  
  # Output
  res <- list(
    Y_e = Y_e,
    x_big = x_big,
    meth = 'Multivariate Rossi',
    op_method = op_method,
    N = N,
    n = n,
    npred = npred,
    ta = ta,
    sc = sc,
    y_prelim = x,
    y = y,
    z = z,
    et = difftime(Sys.time(), t0, units = "secs")
  )
  
  return(res)
}
