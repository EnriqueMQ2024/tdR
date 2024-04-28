aggreg_v <- function(ta, 
                     sc) {
  'Generates a temporal aggregation vector

  INPUT:  
  ta: type of temporal aggregation 
   ta=1 -> sum (flow)
   ta=2 -> average (index)
  n: number of high frequency data
  sc: number of high frequency data points 
  for each low frequency data point (sample conversion)
  
  OUTPUT: c: 1 x sc temporal aggregation vector
  
  Enrique M. Quilis
  Version 1.0 [February 2024]
'  
  
  # Generation of aggregation vector c
  switch(ta,
         '1' = { c <- rep(1, sc) },
         '2' = { c <- rep(1/sc, sc) },
         '3' = { c <- numeric(sc); c[sc] <- 1 },
         '4' = { c <- numeric(sc); c[1] <- 1 },
         { stop('*** ERROR: ta MUST BE 1, 2, 3 OR 4 ***') }
  )
  
  # Output 
  return(c)
}
