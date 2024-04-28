di_fonzo <- function(Y, 
                     x, 
                     z, 
                     ta, 
                     sc, 
                     op1, 
                     f = rep(1, ncol(Y))) {
  
  'Multivariate temporal disaggregation with transversal constraint.
   Method: Di Fonzo

  INPUT: 
    Y: NxM  -> M series of low frequency data with N observations
    x: nxm  -> m series of high frequency data with n observations, m>=M. See (*)
    z: nzx1 -> high frequency transversal constraint with nz obs.
    ta: type of disaggregation
      1 -> sum (flow)
      2 -> average (index)
      3 -> last element (stock) ---> interpolation
      4 -> first element (stock) ---> interpolation
    sc: number of high frequency data points for each low frequency data point
    op1: model for the high frequency innvations
      0 -> multivariate white noise
      1 -> multivariate random walk
    (*) Default:
    f: 1xM -> Set the number of high frequency indicators linked to each low frequency 
    variable. If f is explicitly included, the high frequency indicators should be placed in
    consecutive columns
   
  OUTPUT:
    A list res containing the results
     
  NOTE: 
    Extrapolation is automatically performed when n>sN.
    If n=nz>sN restricted extrapolation is applied.
    Finally, if n>nz>sN extrapolation is perfomed in constrained form for the first nz-sN 
    observatons and in free form for the last n-nz observations.
  REQUIRED:
    pracma::

  REFERENCE: 
    Di Fonzo, T.(1990) "The estimation of M disaggregate time
    series when contemporaneous and temporal aggregates are known", Review
    of Economics and Statistics, 72(1), 178-182
    
  written by:
  Enrique M. Quilis
   
  Version 1.0 [March 2024] '
  
  # Initialize timer
  t0 <- Sys.time()
  
  # Plugging functions
  source('dif.R')
  source('vec.R')
  source('desvec.R')
  source('aggreg_v.R')
  source('aggreg.R')
  
  # Data dimension
  N <- nrow(Y);   M <- ncol(Y)  #Low frequency inputs
  n <- nrow(x);   m <- ncol(x)  #High frequency inputs
  nz <- length(z)               #High frequency transversal constraint
  
  # Number of extrapolations
  h1 <- n - nz
  h2 <- n - (sc*N)
  
  # Checking f
  if (missing(f)) {
    f <- matrix(1, 1, M)
  }
  
  # CONSTRAINT MATRICES
  # Required:
  # H1 ---> transversal
  # H2 ---> longitudinal
  
  # Generate H1: (n-h1) x nM
  H1 <- kronecker(matrix(1, 1, M), cbind(diag(n - h1), matrix(0, n - h1, h1)))
  
  # Generate H2: NM x nM
  C <- aggreg(ta,
              N,
              sc)
  C <- cbind(C, matrix(0, N, h2))
  H2 <- kronecker(diag(M), C)
  
  # Generate H: (n-h1+NM) x nM
  H <- rbind(H1, H2)
  
  # PREPARING DATA MATRICES
  # x_diag
  # Y_big,  Y_e
  # X_diag, X_e
  
  # Generate x_diag: nM x M+m
  af <- cumsum(f)  #Location of last column for each block of regressors
  
  # Pre-allocation
  x_diag <- matrix(0, (n*M), (sum(f)+M))
  
  # Setting initial conditions for limits (rows: a,b; columns: c, d)
  a <- 1; b <- n;
  c <- 1; d <- af[1]+1
  cc <- 1; dd <- af[1]
  
  # Allocation loop
  for (j in 1:M) {
    # Expand matrix to include intercept dummy
    x_aux <- cbind(matrix(1, n, 1), x[, cc:dd])
    mm <- ncol(x_aux)
    x_diag[a:b, c:d] <- x_aux
    # Updating dimension of x_diag to perfom insertion
    a <- b + 1; b <- b + n
    c <- d + 1; d <- d + mm
    cc <- dd + 1; dd <- dd + 1
  }
  
  # Generate X_diag: NM x M+m
  X_diag <- H2 %*% x_diag
  
  # Generate X_e: (n-h1+NM) x M+m
  X_e <- H %*% x_diag
  
  # Generate Y_big: NM x 1
  Y_big <- vec(Y)
  
  # Generate Y_e: (n-h1+NM) x 1
  Y_e <- rbind(z, Y_big)
  
  # PRELIMINARY ESTIMATION OF SIGMA
  BETA <- solve(t(X_diag) %*% X_diag) %*% t(X_diag) %*% Y_big
  U_big <- Y_big - X_diag %*% BETA
  U <- desvec(U_big, M)
  SIGMA <- cov(U)
  
  # APPLYING DI FONZO PROCEDURE
  # High frequency VCV matrix v: nM x nM
  opp1 <- as.character(op1)
  switch(opp1,
         '0' = {
           v <- kronecker(SIGMA, diag(n))
         },
         '1' = {
           print('Random Walk')
           D <- dif(1,n)
           DDi <- solve(t(D) %*% D)
           v <- kronecker(SIGMA, DDi)
         }
  )
  
  # Low frequency VCV matrix V: (n-h1+NM) x (n-h1+NM) and its generalized inverse
  V <- H %*% v %*% t(H)
  Vi <- pracma::pinv(V)
  
  # Generation of distribution filter L: nM x (n-h1+NM)
  L <- v %*% t(H) %*% Vi
  
  # GLS estimation of beta in a SURE context
  beta <- solve(t(X_e) %*% Vi %*% X_e) %*% t(X_e) %*% Vi %*% Y_e
  U_e <- Y_e - X_e %*% beta
  
  # Estimation of high frequency series
  y_big <- x_diag %*% beta + L %*% U_e
  y <- desvec(y_big, M)
  
  # VCV matrix of estimations y: nM x nM
  sigma_y <- (diag(n * M) - L %*% H) %*% v + 
    (x_diag - L %*% X_e) %*% solve(t(X_e) %*% Vi %*% X_e) %*% t(x_diag - L %*% X_e)
  
  # Vector format of std. dev.
  d_y_big <- sqrt(diag(sigma_y))
  
  # Std. dev. series in column format dt_y: n x M
  d_y <- desvec(d_y_big, M)
  
  # Output
  res <- list(
    meth = 'Multivariate Di Fonzo',
    method = switch(op1, 'White Noise shocks', 'Random Walk shocks'),
    N = N,
    n = n,
    nz = nz,
    npred = h2,
    ta = ta,
    sc = sc,
    op1 = op1,
    x_diag = x_diag,
    # Esimates
    beta = beta,
    y = y,
    d_y = d_y,
    et = difftime(Sys.time(), t0, units = "secs")
  )

  return(res)
}

