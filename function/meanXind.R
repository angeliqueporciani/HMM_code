# recursively calculates the mean across elements of a list
# ex. A <- list(a = 1:3, b = 4:6, c = 7:9)
# meanXind(A) produces c(mean(1,4,7), mean(2,5,8), mean(3,6,9))
# output: c(4,5,6)
 
meanXind <- function(A = list, quantile = NULL) {
  
  A <- matrix(unlist(A), nrow = length(A), byrow = T)
  m <- colMeans(A)
  pp <- apply(X = A, MARGIN = c(2), FUN = quantile, probs = quantile)
  B <- list(mean = m, quant = pp)
  
  return(B)
  
}
