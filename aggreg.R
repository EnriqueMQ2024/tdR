aggreg <- function(ta, 
                   N, 
                   sc) {
  'Generates a temporal aggregation matrix

  INPUT: 
  ta: type of temporal aggregation
         ta=1 -> sum (flow)
         ta=2 -> average (index)
         ta=3 -> last element (stock) ---> interpolation
         ta=4 -> first element (stock) ---> interpolation
         N: number of low frequency data
         sc: number of high frequency data points
             for each low frequency data point (sample conversion)

  OUTPUT: 
  C: N x n temporal aggregation matrix, n=sc*N

  Enrique M. Quilis
  Version 1.0 [February 2024]'
  
  # Plugging function  
  source('aggreg_v.R')
  
  # Generation of aggregation matrix C = I(N) <kron> c
  c <- aggreg_v(ta, 
                sc)
  
  # Enforcing c to be of type 'matrix'
  c <- matrix(data = c, nrow = 1)
  
  C <- kronecker(diag(N), c)
  
  # Output   
  return(C)
}
