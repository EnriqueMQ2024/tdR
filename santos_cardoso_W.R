santos_cardoso_W <- function(Y, 
                             x, 
                             ta, 
                             sc, 
                             type_estim, 
                             opC, 
                             rl) {
  ' See master function santos_cardoso() for detailed information
  
  NOTE:
  Requires pracma::
  
  Enrique M. Quilis
  Version 1.0 [March 2024]'
  
  # Plugging functions
  source('aggreg.R')
  source('criterion_SC.R')
  
  # Initializing timer
  t0 <- Sys.time()  
  
  # Data dimension
  N <- length(Y) # Size of low-frequency input
  n <- nrow(x)   # Number of rows in high-frequency indicators matrix
  p <- ncol(x)   # Number of columns in high-frequency indicators matrix
  
  # Preparing the X matrix: including an intercept if opC==1
  if (opC == 1) {
    e <- rep(1, n)
    x <- cbind(e, x) # Expanding the regressor matrix
    p <- p + 1       # Number of high-frequency inputs (plus intercept)
  }
  
  # Generating the aggregation matrix
  C <- aggreg(ta, 
              N, 
              sc)
  
  # Expanding the aggregation matrix to perform extrapolation if needed
  if (n > sc * N) {
    npred <- n - (sc*N)  # Number of required extrapolations
    C <- cbind(C, matrix(0, nrow = N, ncol = npred))
  } else {
    npred <- 0
  }
  
  # Temporal aggregation of the indicators
  X <- C %*% x
  
  # Optimization of the objective function: Log-Likelihood or WLS
  nrl <- length(rl)
  
  opp1 <- as.character(nrl)
  switch(opp1,
         '0' = {
           rl <- c(0.05, 0.99, 50)
           rex <- criterion_SC(Y, x, ta, type_estim, opC, rl, X, C, N, n, p)
           phi <- rex$phi
           r <- rex$r
         },
         '1' = {
           phi <- rl
           type_estim <- 4
         },
         '3' = {
           rex <- criterion_SC(Y, x, ta, type_estim, opC, rl, X, C, N, n, p)
           phi <- rex$phi
           r <- rex$r         },
         {
           stop('*** Grid search on phi is improperly defined. Check rl ***')
         }
  )
  
  # Final estimation with optimal phi
  
  # Auxiliary matrices useful to simplify computations
  I <- diag(1, n)
  aux <- rep(1, n-1)
  LL <- pracma::Diag(-aux, k=-1)
  
  # Generation of the quasi-difference matrix D_phi
  D_phi <- I + phi*LL
  D_phi[1, 1] <- sqrt(1-phi**2)
  iD_phi <- solve(D_phi)
  
  # Truncation remainder: q parameter
  q <- matrix(0, n, 1)
  q[1] <- phi
  
  # Expanded set of regressors: high and low frequency
  z <- cbind(x, q)
  z_phi <- iD_phi %*% z
  Z_phi <- C %*% z_phi
  
  # GLS estimator of gamma
  w <- solve(t(D_phi) %*% D_phi)
  W <- C %*% w %*% t(C)
  iW <- solve(W)
  
  #gamma GLS: includes an estimate of the remainder (initial condition) q[0]
  gamma <- solve(t(Z_phi) %*% iW %*% Z_phi) %*% (t(Z_phi) %*% iW %*% Y)  #gamma GLS
  
  U <- Y - Z_phi %*% gamma                # Low frequency residuals
  scp <- t(U) %*% iW %*% U                # Weighted least squares
  
  sigma_a <- as.numeric(scp/(N-p-1))
  
  L <- w %*% t(C) %*% iW             # Filtering matrix
  u <- L %*% U                       # High frequency residuals
  
  # Temporally disaggregated time series
  y <- z_phi %*% gamma + u
  
  # Information criteria
  aic <- log(sigma_a) + (2 * ((p + 1) / N))
  bic <- log(sigma_a) + (log(N) * ((p + 1) / N))
  
  # VCV matrix of high frequency estimates
  sigma_gamma <- sigma_a * (solve(t(Z_phi) %*% iW %*% Z_phi))

  # Parameters: s.e. ant t-ratios
  gamma_se = sqrt(diag(sigma_gamma))
  gamma_t = gamma / gamma_se

  # Parameters: Long run estimates
  beta_lr = gamma[1:p] / (1-phi)
  
  # VCV of y
  VCV_y <- sigma_a * (diag(n) - L %*% C) %*% w + 
    (z_phi - L %*% Z_phi) %*% sigma_gamma %*% t(z_phi - L %*% Z_phi)
  
  d_y <- sqrt(diag(VCV_y))   # Std. dev. of high frequency estimates
  y_lo <- y - d_y            # Lower lim. of high frequency estimates
  y_hi <- y + d_y            # Upper lim. of high frequency estimates
  
  # Remainder (estimate of initial condition y[0])
  qq <- c(gamma[p+1], gamma_se[p+1], gamma_t[p+1])
  
  # Loading the structure
  res <- list(
    meth = 'Santos-Cardoso',
    ta = ta,
    type_estim = type_estim,
    N = N,
    n = n,
    npred = npred,
    sc = sc,
    p = p,
    opC = opC,
    rl = rl,
    Y = Y,
    x = x,
    y = y,
    y_se = d_y,
    y_lo = y_lo,
    y_hi = y_hi,
    u = u,
    U = U,
    gamma = gamma,
    gamma_se = gamma_se,
    gamma_t = gamma_t,
    qq = qq,
    phi = phi,
    beta = gamma[1:p],
    beta_se = gamma_se[1:p],
    beta_t = gamma_t[1:p],
    beta_lr = beta_lr,
    sigma_a = sigma_a,
    aic = aic,
    bic = bic
  )
  
  if (nrl != 1) {
    res$val <- rex$val
    res$wls <- rex$wls
    res$loglik <- rex$loglik
    res$r <- r
  }
  
  res$et <- as.numeric(Sys.time()) - as.numeric(t0)
  
  # Output
  return(res)
}
