numpar <- function(rex) {
  # Determines the number of non-zero values of ARIMA model
  
  # Plugging functions
  source('conta.R')
  
  # Initial value
  r <- 0
  
  # Regular AR operator
  aux <- rex$ar_reg
  r <- r + conta(aux, 0)
  rm(aux)
  
  # Regular MA operator
  aux <- rex$ma_reg
  r <- r + conta(aux, 0)
  rm(aux)
  
  # Seasonal AR operator
  aux <- rex$ar_sea
  r <- r + conta(aux, 0)
  rm(aux)
  
  # Seasonal MA operator
  aux <- rex$ma_sea
  r <- r + conta(aux, 0)
  rm(aux)
  
  # Excluding first elements of the four filters (AR and MA) used
  r <- r - 4
  
  # Output
  return(r)
}
