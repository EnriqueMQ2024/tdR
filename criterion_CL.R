criterion_CL <- function(Y, 
                         x, 
                         ta, 
                         type_estim, 
                         opC, 
                         rl, 
                         X, 
                         C, 
                         N, 
                         n) {
  # Evaluate the objective function for Chow-Lin method
  # Output: res: a list with ...
  #   res$rho     -> optimal rho parameter
  #   res$val     -> objective function to be optimized
  #   res$wls     -> weighted least squares o.f.
  #   res$loglik  -> log likelihood o.f.
  #   res$r       -> range of rho to perform grid search
  
  # Estimation of optimal innovational parameter by means of a 
  # grid search on the objective function: likelihood (type_estim=1) 
  # or weighted least squares (type_estim=0)
  
  'NOTE:
    Requires pracma::  '
  
  # Parameters of grid search
  r <- seq(rl[1], rl[2], length.out = rl[3])
  nr <- length(r)
  
  # Auxiliary matrix useful to simplify computations
  I <- diag(n)
  aux <- rep(1, n-1)
  LL <- pracma::Diag(-aux, k=-1)
  wls <- numeric(nr)
  loglik <- numeric(nr)
  val <- numeric(nr)
  
  # Evaluation of the objective function in the grid
  for (h in 1:nr) {
    Aux <- I + r[h] * LL
    Aux[1, 1] <- sqrt(1 - r[h]^2)
    w <- solve(t(Aux) %*% Aux)  # High frequency VCV matrix (without sigma_a)
    W <- C %*% w %*% t(C)  # Low frequency VCV matrix (without sigma_a)
    iW <- solve(W)
    beta <- solve(t(X) %*% iW %*% X) %*% t(X) %*% iW %*% Y  # beta estimator
    U <- Y - X %*% beta  # Low frequency residuals
    wls[h] <- t(U) %*% iW %*% U  # Weighted least squares
    sigma_a <- as.numeric(wls[h] / N)          # sigma_a estimator as scalar
    # Likelihood function
    loglik[h] <- (-N/2) * log(2 * pi * sigma_a) - (1/2) * log(det(W)) - (N/2)
    # Objective function 
    val[h] <- (1 - type_estim) * (-wls[h]) + type_estim * loglik[h]
  }
  
  # Determination of optimal rho
  hmax <- which.max(val)
  rho <- r[hmax]
  
  # Output
  res <- list(
    rho = rho,
    val = val,
    wls = wls,
    loglik = loglik,
    r = r
  )
  
  return(res)
}
