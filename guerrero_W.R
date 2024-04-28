guerrero_W <- function(Y, 
                       x, 
                       ta, 
                       sc, 
                       rexw, 
                       rexd, 
                       opC) {
  
  ' See the master function guerrero() for additional information'
  
  # Setting timer
  t0 <- Sys.time()
  
  # Plugging functions
  source('aggreg.R')
  source('calT.R')
  source('numpar.R')
  source('ols_regress.R')
  
  # Receiving inputs
  # Size of low-frequency input
  N <- nrow(Y)
  # Size of m high-frequency inputs (without intercept)
  n <- nrow(x)
  p <- ncol(x)
  
  # Number of parameters in ARIMA model for w
  rw <- numpar(rexw)
  
  # Number of parameters in ARIMA model for discrepancy
  rd <- numpar(rexd)
  
  # w: Scaling of indicators
  # Preparing the X matrix: including an intercept if opC==1
  if (opC == 1) {
    e <- rep(1, n)
    x <- cbind(e, x)  # Expanding the regressor matrix
    p <- p + 1  # Number of p high-frequency inputs (plus intercept)
  }
  
  # Generating the aggregation matrix
  C <- aggreg(ta, 
              N, 
              sc)
  
  # Expanding the aggregation matrix to perform extrapolation if needed.
  if (n > sc * N) {
    npred <- n - sc * N  # Number of required extrapolations
    C <- cbind(C, matrix(0, nrow = N, ncol = npred))
  } else {
    npred <- 0
  }
  
  # Temporal aggregation of the indicators
  X <- C %*% x
  
  # Derivation of intermediate high-freq. series: w
  # w (scaled indicator) is derived by means of OLS applied
  # to the low-freq. data Y and X = Cx
  rex_reg <- ols_regress(Y, X)
  beta    <- rex_reg$beta
  
  # Scaled indicator
  w <- x %*% beta
  
  # PSI: Computing PSI matrix from psi-weights of ARIMA model of w. Information contained in structure rexw
  PSI <- calT(rexw, 
              sc, 
              n)
  
  # k_test: Testing the compatibility of Y and W = Cw. See Guerrero (1990), p. 38-39.
  # Under null of compatibility k_test ---> chi-square with N-p df
  k_test <- t(Y - C %*% w) %*% solve(C %*% PSI %*% t(PSI) %*% t(C)) %*% (Y - C %*% w) / rexw$sigma
  
  # y1: Preliminary estimation (assuming P=I)
  A1 <- (PSI %*% t(PSI) %*% t(C)) %*% solve(C %*% PSI %*% t(PSI) %*% t(C))
  y1 <- w + A1 %*% (Y - C %*% w)
  
  # b: Innovations of the preliminary discrepancy. 
  # This time series is  the basis for setting P=I, by means of testing H0: b = white noise
  b <- solve(PSI) %*% (y1 - w)
  
  # P: Computing P matrix from psi-weights of ARIMA model of y1-w. 
  # Information contained in structure rexd
  PSIh <- calT(rexd, sc, n)
  if ((rd + rexd$d + rexd$bd) == 0) {  # Discrepancy is white noise
    P <- diag(n)
  } else {
    P <- solve(PSI) %*% PSIh %*% t(PSIh) %*% solve(t(PSI))  # Note: PSIh*PSIh'=PSI*P*PSI'
  }
  
  # y: Final estimation (including P)
  A <- (PSI %*% P %*% t(PSI) %*% t(C)) %*% (solve(C %*% PSI %*% P %*% t(PSI) %*% t(C)))
  y <- w + A %*% (Y - C %*% w)
  
  # Computing sigma: 1x1
  u <- y - w
  sig <- t(u) %*% solve(PSI %*% P %*% t(PSI)) %*% u / (n - rd)
  sig <- as.numeric(sig)
  
  # VCV matrix of high frequency estimates
  VCV_y <- sig *(diag(n) - A %*% C) %*% (PSI %*% P %*% t(PSI))
  
  y_se <- sqrt(diag(VCV_y))  # Std. dev. of high frequency estimates
  y_lo <- y - y_se  # Lower lim. of high frequency estimates
  y_hi <- y + y_se  # Upper lim. of high frequency estimates
  
  # Information criteria
  aic <- log(sig) + (2*p / N)
  bic <- log(sig) + (log(N)*p / N)
  
  # Loading the structure
  res <- list(
    meth = 'Guerrero',
    ta = ta,
    type = 2,  # For output homogeneity with other BLUE functions
    N = N,
    n = n,
    npred = npred,
    sc = sc,
    p = p,
    opC = opC,
    Y = Y,
    x = x,
    w = w,
    y1 = y1,
    y = y,
    y_se = y_se,
    y_lo = y_lo,
    y_hi = y_hi,
    PSI = PSI,
    PSIh = PSIh,
    rexw = rexw,  # Scaled indicator: ARIMA model
    rexd = rexd,  # Discrepancy: ARIMA model
    delta = (y1 - w),
    u = u,
    U = C %*% u,
    beta = beta,
    beta_se = rex_reg$beta_se,
    beta_t = rex_reg$beta_t,
    rho = 1.00, # For output homogeneity with other BLUE functions
    sigma_a = sig,
    aic = aic,
    bic = bic,
    k_test = k_test,  # Test of compatibility statistic
    et = as.numeric(difftime(Sys.time(), t0, units = "secs"))  # Elapsed time
  )
  
  # Output
  return(res)
}
