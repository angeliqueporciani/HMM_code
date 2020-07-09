xtsList2df <- function(activity = list) {
  
  require(xts)
  
  activityDF <- list()
  
  for (i in seq_along(activity))
  {
    activityDF[[i]] <- data.frame(
      Time = index(activity[[i]]),
      cbind.data.frame(xtsAttributes(activity[[i]])),
      Activity = as.integer(activity[[i]][, 1])
    )
  }
  
  activityDF <- do.call(rbind.data.frame, activityDF)
  
  return(activityDF)
  
}