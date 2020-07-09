AvgActByHour <- function(list, start = NULL, end = NULL) {
  
  start <- ymd_hms(start)
  end <- ymd_hms(end)
  
  A <- vector("list", length(list))
  tmax <- as.numeric(as.period(interval(start, end), "hours"))/(60*60)
  
  for(i in seq_along(list)) {
    for(j in 1:tmax){
      A[[i]][j] <- mean(list[[i]][paste0(start + hours(j-1), "/", start + hours(j))])
    }
  }
  
  A <- structure(A, start = start, end = end)
  
  return(A)
}
