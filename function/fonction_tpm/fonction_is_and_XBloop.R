
is.momentuHMM <- function(x)
  inherits(x,"momentuHMM")

is.miSum <- function(x)
  inherits(x,"miSum")

XBloop_rcpp <- function(DM, Xvec, nbObs, nr, nc, circularAngleMean, consensus, rindex, cindex, nbStates) {
  .Call('_momentuHMM_XBloop_rcpp', PACKAGE = 'momentuHMM', DM, Xvec, nbObs, nr, nc, circularAngleMean, consensus, rindex, cindex, nbStates)
}
