#' @importFrom survival untangle.specials
#' @importFrom prodlim strip.terms

cosinorCos<-function(x,period){
  cos(2*pi*x/period)
}
cosinorSin<-function(x,period){
  sin(2*pi*x/period)
}

