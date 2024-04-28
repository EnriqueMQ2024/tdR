conta <- function(aux, 
                  f = 0) {
  # Determines the number of non-f elements in the polynomial aux
  
  r <- 0
  u <- length(aux)
  
  for (i in 1:u) {
    if (aux[i] != f) {
      r <- r + 1
    }
  }
  
  # Output
  return(r)
}
