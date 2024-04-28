ols_regress <- function(Y, 
                        X) {
  
  'OLS estimates of linear regression model Y=Xb+U
  
  INPUT: 
    Y: Nx1 ---> response data
    X: nxp ---> regressors (without intercept)
  OUTPUT: 
    res: a list containing the results
  
  Enrique M. Quilis
  Version 1.0 [March 2024]'
  
  # Data dimension: Nxp
  N <- length(Y)
  p <- ncol(X)
  
  # OLS Estimates: beta
  iXX <- solve(t(X) %*% X)
  beta <- iXX %*% (t(X) %*% Y)
  # Residuals
  e <- Y - X %*% beta
  
  # OLS Estimates: sigma
  sig <- (t(e) %*% e) / (N-p)
  sig <- as.numeric(sig)
  
  # VCV matrix of beta
  S_beta <- sig * iXX
  # Selecting s.e. of beta
  beta_se <- sqrt(diag(S_beta))
  
  # t-ratios of beta
  beta_t <- beta / beta_se
  
  # Creating result list
  res <- list(beta = beta, 
              beta_se = beta_se, 
              beta_t = beta_t)
  
  # Output
  return(res)
}
