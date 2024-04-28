ras <- function(F0, 
                x0, 
                x1, 
                v, 
                u, 
                opG = 0) {
  ' Bi-proportional adjustment of Input-Output matrices
 
 INPUT: 
    F0: kxk -> benchmark matrix
    x0: 1xk -> benchmark output (by cols)
    x1: 1xk -> updated output (by cols)
    v: 1xk -> updated F totals (by cols)
    u: kx1 -> updated F totals (by rows)
    opG: 1x1 -> convergence plot (optional, default=0)
          
 OUTPUT:
   F1: kxk -> updated (balanced) matrix
 
 REFERENCE: 
 Bacharach, M. (1965) "Estimating non-negative matrices from 
 marginal data", International Economic Review, 6(3), 294-310
  
 Enrique M. Quilis
 Version 1.0 [March 2024]'
  
  # Output on screen
  cat("*** RAS Algorithm *** \n")
  
  # Max number of iterations
  max_iter <- 1000L
  
  # Vector of deltas (discrepancies)
  d <- numeric(max_iter)
  
  # Tolerance
  tol <- 1.0000e-007
  
  #-------------------------------------------------------------------------
  # BENCHMARK
  #-------------------------------------------------------------------------
  # Number of products
  k <- length(x0)
  
  # Number of cells in F
  n <- k**2
  
  # Unit vectors
  i <- as.matrix(rep(1, k))
  
  #-------------------------------------------------------------------------
  # INITIAL ESTIMATES
  #-------------------------------------------------------------------------
  # Benchmark coefficient matrix
  A0 <- F0 / kronecker(i, x0)
  
  # Initial matrix estimate
  F1 <- A0 * kronecker(i, x1)
  
  #-------------------------------------------------------------------------
  # BI-PROPORTIONAL ADJUSTMENT LOOP
  #-------------------------------------------------------------------------
  iter <- 1
  delta <- tol * 1.50
  while ((delta > tol) && (iter < max_iter)) {
    # Row adjustment
    u1 <- matrix(rowSums(F1), nrow = k)
    r1 <- u / u1
    F2 <- F1 * kronecker(t(i), r1)
    # Column adjustment
    v1 <- colSums(F2)
    s1 <- v / v1
    F2 <- F2 * kronecker(i, s1)
    # Computing measure of convergence
    delta <- sum(abs(F2 - F1)) / n
    d[iter] <- delta
    # If convergence failed update F matrix
    F1 <- F2
    iter <- iter + 1
  }
  
  # Final message
  cat(sprintf("   Number of iterations -->  %4d\n", iter))
  
  #-------------------------------------------------------------------------
  # OPTIONAL GRAPHICAL OUTPUT
  #-------------------------------------------------------------------------
  if (opG == 1) {
    windows()
    plot(1:iter, d[1:iter], type = "o", col = "red", xlab = "Iterations", 
         ylab = expression(paste("Measure of convergence: ", delta)), 
         main = 'Convergence')
    grid()
  }
  
  # Output
  return(F1)
}
