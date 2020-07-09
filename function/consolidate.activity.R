# take a list of a time series of activity scores
# in which each element of the list is an individual
# and integrate the activity scores at each time point across individuals
# using the function defined by the FUN argument (default is the mean)

consolidate.activity <- function(activity = list, FUN = mean, smooth.by = 0) {
  
  FUN <- match.fun(FUN)
  if (!is.vector(activity) || is.object(activity)) 
    activity <- as.list(activity)
  
  require(TSstudio)
  
  for (i in seq_along(activity)) {
    activity[[i]] <- ts_ma(activity[[i]], n = smooth.by, plot = FALSE)[[1]]
  }
  
  x <- activity[[1]]
  #x <- vector(mode = 'numeric', length = length(activity[[1]]))
  y <- vector(mode = 'numeric', length = length(activity))
  
  for(i in 1:length(x)) {
    for(j in 1:length(y)) {
      y[j] <- activity[[j]][i]
    }
    x[i] <- FUN(y)
  }
  
  return(x)
  
}