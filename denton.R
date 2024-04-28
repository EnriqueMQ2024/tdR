denton <- function(Y, 
                   x, 
                   ta, 
                   d, 
                   sc, 
                   op1) {
  'Temporal disaggregation using the Denton method

  INPUT:
    Y: Nx1 vector of low frequency data
    x: nx1 vector of low frequency data
    ta: type of disaggregation
       ta=1 -> sum (flow)
      ta=2 -> average (index)
      ta=3 -> last element (stock) ---> interpolation
      ta=4 -> first element (stock) ---> interpolation
    d: objective function to be minimized: volatility of ...
      d=1 -> first differences
      d=2 -> second differences
    sc: number of high frequency data points for each low frequency data point
    op1: additive variant (op1=1) or proportional variant(op1=2)

  OUTPUT:
    res: a list containing the results
  
  NOTE: 
    D1 and D2 are nxn matrices

  REFERENCE:
    Denton, F.T. (1971) "Adjustment of monthly or quarterly
    series to annual totals: an approach based on quadratic minimization",
    Journal of the American Statistical Society, 66(333), 99-102.

  Enrique M. Quilis
  Version 1.2 [February 2024]'
  
  # Starting timer
  t0 <- Sys.time()
  
  # Checking d: must be 1 or 2
  if (!(d %in% c(1, 2))) {
    stop('*** d must be 1 or 2 ***')
  }
  
  # Plugging functions
  source('aggreg.R')
  source('dif.R')
  
  # Data dimension
  N <- length(Y)
  n <- length(x)
  
  # Defining combination of variant (additive or proportional) and 
  # degree of differencing (d=1 or d=2)
  FLAX <- matrix(c(1, 2, 3, 4), ncol = 2, byrow = TRUE)
  flax <- FLAX[op1, d]
  
  # Generation of aggregation matrix C: Nxn
  C <- aggreg(ta, 
              N, 
              sc)
  
  # Expanding the aggregation matrix to perform extrapolation if needed
  if (n > sc * N) {
    npred <- (n - sc * N)  # Number of required extrapolations
    C <- cbind(C, matrix(0, nrow = N, ncol = npred))
  } else {
    npred <- 0
  }
  
  # Temporal aggregation of the indicator: Nx1
  X <- C %*% x
  
  # Computing low frequency residuals: Nx1
  U <- Y - X
  
  # Computing Q matrix
  switch(flax,
         {
           # Additive first differences: d=1
           D1 <- dif(1, n)
           Q <- solve(t(D1) %*% D1)
         },
         {
           # Additive second differences: d=2
           D2 <- dif(2, n)
           Q <- solve(t(D2) %*% D2)
         },
         {
           # Proportional first differences: d=1
           x <- as.numeric(x)
           D1 <- dif(1, n)
           Q <- diag(x) %*% solve(t(D1) %*% D1) %*% diag(x)
         },
         {
           # Proportional second differences: d=2
           x <- as.numeric(x)
           D2 <- dif(2, n)
           Q <- diag(x) %*% solve(t(D2) %*% D2) %*% diag(x)
         }
  )
  
  # High frequency residuals
  u <- Q %*% t(C) %*% solve(C %*% Q %*% t(C)) %*% U
  
  # High frequency estimator
  y <- x + u
  
  # Loading the structure
  res <- list(
    meth = 'Denton',
    meth1 = ifelse(op1 == 1, 'Additive variant', 'Proportional variant'),
    N = N,
    n = n,
    ta = ta,
    sc = sc,
    npred = npred,
    d = d,
    y = y,
    x = x,
    U = U,
    u = u,
    et = as.numeric(Sys.time() - t0)
  )
  
  # Output
  return(res)
}
