denton_multi <- function(Y, 
                         x, 
                         z, 
                         ta, 
                         sc, 
                         d, 
                         op1 = 1) {
  'Multivariate temporal disaggregation using the Denton method

  INPUT:
    Y: NxM -> M series of low frequency data with N observations
    x: nxM -> M series of high frequency data with n observations
    z: nx1 -> high frequency transversal constraint
    ta: type of disaggregation
      ta=1 -> sum (flow)
      ta=2 -> average (index)
      ta=3 -> last element (stock) ---> interpolation
      ta=4 -> first element (stock) ---> interpolation
    d: objective function to be minimized: volatility of ...
     d=1 -> first differences
     d=2 -> second differences
    sc: number of high frequency data points for each low frequency data point
    op1: additive variant (op1=1) or proportional variant(op1=2). Default: op1=1

  OUTPUT:
    res: a list containing the results
  
  NOTE:
    Requires pracma:: 
  
  REFERENCE:
    Di Fonzo, T. (1994) "Temporal disaggregation of a system of time series when the 
    aggregate is known: optimal vs. adjustment methods", INSEE-Eurostat Workshop on 
    Quarterly National Accounts, Paris, December.
  
  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Plugging functions
  source('dif.R')
  source('vec.R')
  source('desvec.R')
  source('aggreg_v.R')
  source('aggreg.R')
  
  # Timer
  t0 <- Sys.time()
  
  # Data dimension
  N <- nrow(Y); M <- ncol(Y)
  n <- nrow(x)
  
  # --------------------------------------------------------------------
  # CONSTRAINT MATRICES
  #     H1 ---> transversal; n x nM
  #     H2 ---> longitudinal: NM x nM
  
  # H1: transversal
  H1 <- kronecker(matrix(1, nrow = 1, ncol = M), diag(n))
  
  # H2: longitudinal
  c <- aggreg_v(ta, 
                sc)
  
  C <- aggreg(ta, 
              N, 
              sc)
  
  if (n > sc * N) {
    npred <- n - (sc*N)  # Number of required extrapolations
    C <- cbind(C, matrix(0, nrow = N, ncol = npred))
  } else {
    npred <- 0
  }
  
  H2 <- kronecker(diag(M), C)
  
  # H: Combined constraint matrix: (n+NM) x nM
  H <- rbind(H1, H2)
  
  # --------------------------------------------------------------------
  # PREPARING DATA MATRICES
  
  # x_big: nM x 1
  x_big <- vec(x)
  
  # Y_big: NM x 1
  Y_big <- vec(Y)
  
  # Y_e: (n+NM) x 1
  Y_e <- rbind(z, Y_big)
  
  # Filtering matrices
  D1 <- dif(d, n)
  D1 <- D1[(d+1):n, ]  #Difference operator without initial conditions
  
  AUX1 <- cbind(matrix(0, nrow = d, ncol = (n-(sc*d))), kronecker(diag(d), t(c)))
  D <- rbind(D1, AUX1)
  DD <- t(D) %*% D
  
  # Transforming op1 as char type
  opp1 <- as.character(op1)
  switch(opp1,
         #Additive variant
         '1' = {
           Wi <- kronecker(diag(M), solve(DD)) 
         },
         #Proportional variant
         '2' = {
           x_diag <- diag(x_big)
           Wi <- x_diag %*% kronecker(diag(M), solve(DD)) %*% x_diag
         },
         stop("*** op1 must be 1 or 2 ***")
  )
  
  # --------------------------------------------------------------------
  # ESTIMATION
  
  # Generalized inverse
  Vi <- pracma::pinv(H %*% Wi %*% t(H))
  
  # Residuals
  U_e <- Y_e - H %*% x_big
  
  # Estimating hf time series
  y_big <- x_big + Wi %*% t(H) %*% Vi %*% U_e
  
  # Arranging results as matrix
  y <- desvec(y_big, M)
  
  # Output
  res <- list(
    Y_e = Y_e,
    x_big = x_big,
    meth = 'Multivariate Denton',
    meth1 = (if (op1 == 1) 'Additive variant' else 'Proportional variant'),
    N = N,
    n = n,
    npred = npred,
    ta = ta,
    sc = sc,
    d = d,
    y = y,
    z = z,
    et = difftime(Sys.time(), t0, units = "secs")
  )
  
  return(res)
}
