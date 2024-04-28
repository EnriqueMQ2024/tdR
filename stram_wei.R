stram_wei <- function(Y, 
                      ta, 
                      d, 
                      sc, 
                      v) {
  ' Temporal disaggregation using the Stram-Wei method.
  
  INPUT:
    Y: Nx1 low frequency time series
    ta: type of disaggregation
      ta=1 -> sum (flow)
      ta=2 -> average (index)
      ta=3 -> last element (stock) ---> interpolation
      ta=4 -> first element (stock) ---> interpolation
    d: number of unit roots (high frequency time series)
    sc: number of high frequency data points for each low frequency data point (sample conversion)
    v: (n-d)x(n-d) VCV matrix of high frequency stationary series
  
  OUTPUT:
    res: a list containing the results
  
  REFERENCE:
    Stram, D.O. and Wei, W.W.S. (1986) "A methodological note on the disaggregation 
    of time series totals", Journal of Time Series Analysis, 7(4), 293-302.
  
  Enrique M. Quilis
  Version 1.0 [February 2024] '
  
  # Plugging functions
  source('aggreg_v.R')
  source('aggreg.R')
  source('dif.R')
  source('moving_sum_matrix.R')
  
  # Starting timer
  t0 <- Sys.time()
  
  # Data dimension
  N <- length(Y)
  
  # Number of high frequency observations
  n <- sc * N  
  
  # Check dimension of v
  nv <- nrow(v)
  mv <- ncol(v)
  
  if (nv != (n - d) | mv != (n - d)) {
    stop(' *** INCORRECT DIMENSION OF v MATRIX *** ')
  }
  
  # -----------------------------------------------------------------
  # Computation of H1 matrix
  # Generation of aggregation matrix C = I(N) <kron> c
  c <- aggreg_v(ta, 
                sc)
  # Enforcing c to be of type 'matrix'
  c <- matrix(data = c, nrow = 1)
  
  # Difference matrix
  D1 <- dif(d, 
            n)
  D1 <- D1[(d + 1):n, ]  # Difference operator without initial conditions
  
  AUX1 <- cbind(matrix(0, nrow = d, ncol = (n-sc*d)), 
                kronecker(diag(d), c))
  
  H1 <- rbind(D1,               
              AUX1)
  
  # -----------------------------------------------------------------
  # Computation of H2 matrix
  S <- moving_sum_matrix(sc,
                         n)
  S <- rbind(matrix(0, nrow = (sc-1), ncol = n),
             S)
  
  # Ensuring switch input
  dd <- as.character(d)
  if (ta == 3 | ta == 4) {
    switch(dd,
           '0' = { SS <- diag(n-d) },
           '1' = { SS <- S },
           '2' = { SS <- S %*% S }
    )
  } else {
    switch(dd,
           '0' = { SS <- S },
           '1' = { SS <- S %*% S },
           '2' = { SS <- S %*% S %*% S }
    )
  }
  
  taa <- as.character(ta)
  switch(ta,
         '1' = { IN <- aggreg(3, N, sc) },
         '2' = { IN <- (1 / sc) * aggreg(3, N, sc) },
         '3' = { IN <- aggreg(3, N, sc) },
         `4` = { IN <- aggreg(4, N, sc) }
  )
  
  # Generation of Cd matrix
  Cd <- IN %*% SS
  Cd <- Cd[(d + 1):N, (d + 1):n]  # Deleting initial observations
  
  D2 <- dif(d, N)
  D2 <- D2[(d+1):N, ]  # Difference operator without initial conditions
  
  AUX2 <- cbind(matrix(0, nrow = d, ncol = (N - d)),  diag(d))
  
  H2 <- rbind(v %*% t(Cd) %*% solve(Cd %*% v %*% t(Cd)) %*% D2,
              AUX2)
  
  # Computation of H matrix and high-frequency estimate
  H <- solve(H1) %*% H2
  y <- H %*% Y
  
  # Output
  res <- list(
    meth = 'Stram-Wei',
    N = N,
    ta = ta,
    sc = sc,
    pred = 0, # No extrapolations are performed
    d = d,
    H = H,
    y = y,
    et = as.numeric(Sys.time() - t0)
  )
  
  return(res)
}
