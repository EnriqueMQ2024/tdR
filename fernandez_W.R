fernandez_W <- function(Y, 
                        x, 
                        ta, 
                        sc, 
                        opC) {
  ' See master function fernandez() for detailed information'
  
  'NOTE:
    Requires pracma::  '
  
  # Timer
  t0 <- Sys.time()
  
  # Plugging function
  source('aggreg.R')
  
  # Size of the problem
  N <- nrow(Y)      # Low-frequency input: number of observations
  n <- nrow(x)      # High-frequency input: number of observations
  p <- ncol(x)      # Number of hf indicators (without intercept)
  
  # Preparing the X matrix: including an intercept if opC==1
  if (opC == 1) {
    e <- rep(1, n)
    x <- cbind(e, x)   # Expanding the hf data matrix to include an intercept
    p <- p + 1         # Number of hf indicators (plus intercept)
  }

  # Generating the aggregation matrix
  C <- aggreg(ta, 
              N, 
              sc)

  # Expanding the aggregation matrix to perform extrapolation if needed.
  if (n > sc * N) {
    # Number of extrapolations 
    npred <- n - sc * N   
    C <- cbind(C, matrix(0, nrow = N, ncol = npred))
  } else {
    npred <- 0
  }
  
  # Temporal aggregation of the indicators
  X <- C %*% x
  I <- diag(1, n)
  aux <- rep(1, n-1)
  LL <- pracma::Diag(-aux, k=-1)
  Aux <- I + LL

  w <- solve(t(Aux) %*% Aux)        # High frequency VCV matrix (without sigma_a)
  W <- C %*% w %*% t(C)             # Low frequency VCV matrix (without sigma_a)
  Wi <- solve(W)
  
  # Beta
  beta <- solve(t(X) %*% Wi %*% X) %*% t(X) %*% Wi %*% Y  # beta estimator
  
  # Residuals
  U <- Y - X %*% beta                         # Low frequency residuals
  wls <- t(U) %*% Wi %*% U                    # Weighted least squares
  sigma_a <- as.numeric(wls / (N-p))          # sigma_a estimator as scalar
  L <- w %*% t(C) %*% Wi            # Filtering matrix
  u <- L %*% U                      # High freq. residuals
  
  # Temporally disaggregated time series
  y <- x %*% beta + u
  
  # Information criteria
  # Note: p is NOT expanded to include the innovational parameter (rho=1 here)
  aic <- log(sigma_a) + (2 * p / N)
  bic <- log(sigma_a) + (log(N) * p / N)
  
  # VCV matrix of high frequency estimates
  sigma_beta <- sigma_a * solve((t(X) %*% Wi %*% X))

  # VCV of high frequency estimates
  VCV_y1 <- sigma_a * (I - L %*% C) %*% w
  VCV_y2 <- (x - L %*% X) %*% sigma_beta %*% t(x - L %*% X)
  VCV_y <- VCV_y1 + VCV_y2
  
  d_y <- sqrt(diag(VCV_y))   # Std. dev. of high frequency estimates
  y_lo <- y - d_y            # Lower lim. of high frequency estimates
  y_hi <- y + d_y            # Upper lim. of high frequency estimates
  
  # Loading the structure
  res <- list(
    meth = 'Fernandez',
    ta = ta,
    type = 2,
    N = N,
    n = n,
    npred = npred,
    opC = opC,
    sc = sc,
    p = p,
    Y = Y,
    x = x,
    y = y,
    y_dt = d_y,
    y_lo = y_lo,
    y_hi = y_hi,
    u = u,
    U = U,
    beta = beta,
    beta_sd = sqrt(diag(sigma_beta)),
    beta_t = beta / sqrt(diag(sigma_beta)),
    rho = 1.00,
    sigma_a = sigma_a,
    aic = aic,
    bic = bic,
    et = difftime(Sys.time(), t0, units = "secs")
  )
  
  # Output
  return(res)
}
