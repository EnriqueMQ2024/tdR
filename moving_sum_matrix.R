moving_sum_matrix <- function(h, 
                              n) {
  'Moving sum of h periods during n observations (auxiliary matrix)
  
  INPUT:
  h: size of consecutive aggregation
  n: dimension of filter matrix
  
  OUTPUT:
  S: (n-h)xn moving aggregation matrix
  
  NOTE: 
  The U(B)=1+B+B**2+...+B**(h-1) filter performs a moving sum of h consecutive 
  elements of a time series vector. 
  Its matrix form has n columns (since it is applied to the whole vector) 
  and (n-h+1) rows, due to the missing values induced by the absence of (h-1) 
  initial conditions (note that U(B) has degree h-1, not h)
  
  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Initialization
  c <- rep(1, h)
  
  # Loop
  S <- matrix(0, nrow = (n - h + 1), ncol = n)
  for (i in 1:(n - h + 1)) {
    S[i, (i):(i + h - 1)] <- c
  }
  
  # Output
  return(S)
}
