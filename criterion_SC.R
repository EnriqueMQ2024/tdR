criterion_SC <- function(Y, 
                         x, 
                         ta, 
                         type_estim, 
                         opC, 
                         rl, 
                         X, 
                         C, 
                         N, 
                         n, 
                         p) {
  
  'Estimation of optimal innovational parameter by means of a grid search on the objective function: 
  Log-Likelihood (type_estim=1) or Weighted Least Squares (type_estim=0)
  
  NOTE:
    Requires pracma::  

  Enrique M. Quilis
  Version 1.0 [March 2024]'
  
  # Grid search parameters
  r <- seq(rl[1], rl[2], length.out = rl[3])
  nr <- length(r)
  
  # Auxiliary matrices useful to simplify computations
  I <- diag(1, n)
  aux <- rep(1, n-1)
  LL <- pracma::Diag(-aux, k=-1)
  
  wls <- numeric(nr)
  loglik <- numeric(nr)
  val <- numeric(nr)
  
  # Evaluation of the objective function in the grid
  q <- matrix(0, n, 1)
  
  
  for (h in 1:nr) {
    D_phi <- I + r[h] * LL
    D_phi[1, 1] <- sqrt(1 - r[h]**2)
    iD_phi <- solve(D_phi)
    
    # Expanded set of regressors: high and low frequency
    q[1] <- r[h]
    z <- cbind(x, q)   # Truncation remainder q is included here
    z_phi <- iD_phi %*% z
    Z_phi <- C %*% z_phi
    
    # GLS estimator of gamma
    w <- solve(t(D_phi) %*% D_phi)
    W <- C %*% w %*% t(C)
    iW <- solve(W)
    gamma <- solve(t(Z_phi) %*% iW %*% Z_phi) %*% (t(Z_phi) %*% iW %*% Y)  # gamma GLS
    U <- Y - Z_phi %*% gamma  # Low frequency residuals
    wls[h] <- t(U) %*% iW %*% U  # Weighted least squares
    sigma_a <- wls[h] / (N - p - 1)  # sigma_a estimator (p+1 due to lagged endogenous)
    
    # Likelihood function
    loglik[h] <- (-N / 2) * log(2 * pi * sigma_a) - (1 / 2) * log(det(W)) - N / 2
    
    # Objective function
    val[h] <- (1 - type_estim) * (-wls[h]) + type_estim * loglik[h]
  }
  
  # Determination of optimal phi
  hmax <- which.max(val)
  phi <- r[hmax]
  
  # Loading the structure
  res <- list(phi = phi, val = val, wls = wls, loglik = loglik, r = r)
  
  # Output
  return(res)
}
