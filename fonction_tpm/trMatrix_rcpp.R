#' Transition probability matrix
#'
#' Computation of the transition probability matrix, as a function of the covariates and the regression
#' parameters. Written in C++. Used in \code{\link{viterbi}}.
#'
#' @param nbStates Number of states
#' @param beta Matrix of regression parameters
#' @param covs Matrix of covariate values
#' @param betaRef Indices of reference elements for t.p.m. multinomial logit link.
#'
#' @return Three dimensional array \code{trMat}, such that \code{trMat[,,t]} is the transition matrix at
#' time t.
trMatrix_rcpp <- function(nbStates, beta, covs, betaRef) {
  .Call('_momentuHMM_trMatrix_rcpp', PACKAGE = 'momentuHMM', nbStates, beta, covs, betaRef)
}