calT <- function(rex, 
                 sc, 
                 n) {
  ' Psi-weights from an ARIMA model in matrix form

  INPUT: 
    rex: list that contains the AR and MA operators of the ARIMA model, 
         both regular and seasonal, as well as the orders of the difference operator
    sc: number of high frequency data points for each low frequency data point
    n: number of data points to compute

  OUTPUT:
    PSI: nxn lower triangular matrix, PSI(i,i)=1 for all i,
    psi-weights are columnwise allocated
        e.g., first column is [1 phi(1) phi(2) ... phi(n-1)]
        
  REQUIRED: signal, pracma

  Enrique M. Quilis
  Version 1.0 [March 2024]
' 
  
  # Receiving input
  d <- rex$d
  bd <- rex$bd
  ar_reg <- rex$ar_reg
  ar_sea <- rex$ar_sea
  ma_reg <- rex$ma_reg
  ma_sea <- rex$ma_sea
  
  # Regular differences
  aux <- c(1, -1)
  dd <- as.character(d)
  switch(dd,
         '0' = { d_reg <- 1},
         '1' = { d_reg <- aux},
         '2' = { d_reg <- convolve(aux, rev(aux), type = 'open')}
  )
  rm(aux)
  
  # Seasonal differences
  aux <- c(rep(0, (sc+1)))
  aux[1] <- 1;  aux[(sc+1)] <- -1
  bbd <- as.character(bd)
  switch(bbd,
         '0' = { d_sea <- 1},
         '1' = { d_sea <- aux},
         '2' = { d_sea <- convolve(aux, rev(aux), type = 'open')}
  )
  rm(aux)
  
  # AR operators
  ard <- convolve(d_reg, rev(d_sea), type = 'open')
  ar1 <- convolve(ar_reg, rev(ar_sea), type = 'open')
  ar <- convolve(ar1, rev(ard), type = 'open')
  
  # MA operators
  ma <- convolve(ma_reg, rev(ma_sea), type = 'open')
  
  # Computing psi-weights of ARMA model
  impz_output <- signal::impz(ma, ar, n)
  phi <- impz_output$x[-1]  # Skipping first element (normalized to 1)
  
  # Generating PSI: nxn lower triangular matrix. 
  # Diagonal elements = 1
  # Psi-weights are columnwise allocated.
  PSI <- diag(1, nrow = n, ncol = n)
  u <- 1
  while (u < n)
    for (u in 1:(n-1)) {
      aux <- phi[u] * (vector(mode = 'numeric',(n-u))+1)
      PSI <- PSI + pracma::Diag(aux, -u)
      u <- u + 1
    }
  
  # Output
  return(PSI)
}
