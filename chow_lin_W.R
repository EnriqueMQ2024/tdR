chow_lin_W <- function(Y, 
                       x, 
                       ta, 
                       sc, 
                       type_estim, 
                       opC, 
                       rl) {
  ' See master function chow_lin() for detailed information'
  
  'NOTE:
    Requires pracma::  '
  
  # Plugging functions
  source('aggreg.R')
  source('criterion_CL.R')
  
  # Data dimension
  N <- nrow(Y)    # Size of low-frequency input
  n <- nrow(x)    # Size of p high-frequency inputs (without intercept); 
  p <- ncol(x)
  
  # Preparing the X matrix: including an intercept if opC==1
  if (opC == 1) {
    e <- rep(1, n)
    x <- cbind(e, x)    # Expanding the regressor matrix
    p <- p + 1    # Number of p high-frequency inputs (plus intercept)
  }
  
  # Generating the aggregation matrix
  C <- aggreg(ta, 
              N, 
              sc)
  
  # Expanding the aggregation matrix to perform extrapolation if needed.
  if (n > sc * N) {
    npred <- n - sc * N    # Number of required extrapolations
    C <- cbind(C, matrix(0, nrow = N, ncol = npred))
  } else {
    npred <- 0
  }
  
  # Temporal aggregation of the indicators
  X <- C %*% x
  
  # Optimization of objective function (Lik. or WLS)
  nrl <- length(rl)
  
  # Transforming op1 as char type
  opp1 <- as.character(nrl)
  switch(opp1,
         '0' = {
           rl <- c(0.05, 0.99, 50)
           rex <- criterion_CL(Y, x, ta, type_estim, opC, rl, X, C, N, n)
           rho <- rex$rho
           r <- rex$r
         },
         '1' = {
           rho <- rl
           type_estim <- 4
         },
         '3' = {
           rex <- criterion_CL(Y, x, ta, type_estim, opC, rl, X, C, N, n)
           rho <- rex$rho
           r <- rex$r
         },
         {
           stop('*** Grid search on rho is improperly defined. Check rl ***')
         }
  )
  
  # Final estimation with optimal rho
  
  # Auxiliary matrix useful to simplify computations
  I <- diag(n)
  aux <- rep(1, n-1)
  LL <- pracma::Diag(-aux, k=-1)
  Aux <- I + rho * LL
  Aux[1, 1] <- sqrt(1 - rho^2)
  w <- solve(t(Aux) %*% Aux)    # High frequency VCV matrix (without sigma_a)
  W <- C %*% w %*% t(C)         # Low frequency VCV matrix (without sigma_a)
  Wi <- solve(W)
  beta <- solve(t(X) %*% Wi %*% X) %*% t(X) %*% Wi %*% Y    # beta estimator
  U <- Y - X %*% beta    # Low frequency residuals
  wls <- t(U) %*% Wi %*% U    # Weighted least squares
  sigma_a <- as.numeric(wls / N)          # sigma_a estimator as scalar
  L <- w %*% t(C) %*% Wi    # Filtering matrix
  u <- L %*% U
  
  # Temporally disaggregated time series
  y <- x %*% beta + u
  
  # Information criteria
  aic <- log(sigma_a) + 2 * (p + 1) / N
  bic <- log(sigma_a) + log(N) * (p + 1) / N
  
  # VCV matrix of high frequency estimates
  sigma_beta <- sigma_a * solve(t(X) %*% Wi %*% X)
  VCV_y1 <- sigma_a * (diag(n) - L %*% C) %*% w
  VCV_y2 <- (x - L %*% X) %*% sigma_beta %*% t(x - L %*% X)    # If beta is fixed, this should be zero
  VCV_y <- VCV_y1 + VCV_y2
  
  d_y <- sqrt(diag(VCV_y))    # Std. dev. of high frequency estimates
  y_lo <- y - d_y    # Lower lim. of high frequency estimates
  y_hi <- y + d_y    # Upper lim. of high frequency estimates
  
  # Weighted least squares
  wls <- t(U) %*% Wi %*% U    # Weighted least squares
  
  # Likelihood
  loglik <- (-N / 2) * log(2 * pi * sigma_a) - (1 / 2) * log(det(W)) - (N / 2)
  
  # Output
  res <- list(
    meth = 'Chow-Lin',
    ta = ta,
    N = N,
    n = n,
    npred = npred,
    sc = sc,
    type_estim = type_estim,
    p = p,
    opC = opC,
    rl = rl,
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
    rho = rho,
    sigma_a = sigma_a,
    aic = aic,
    bic = bic,
    val = if (nrl == 1) NULL else rex$val,
    wls = if (nrl == 1) wls else rex$wls,
    loglik = if (nrl == 1) loglik else rex$loglik,
    r = if (nrl == 1) NULL else r
  )
  
  return(res)
}
