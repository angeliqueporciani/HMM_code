plot.AvgActByHour <- function(AvgActByHour, ...) {
  
  start <- attr(AvgActByHour, "start")
  end <- attr(AvgActByHour, "end")
  tmax <- as.numeric(as.period(interval(start, end), "hours"))/(60*60)
  
  maxAct <- max(unlist(AvgActByHour))
  
  x <- start + hours(1:tmax)
  
  plot(x, AvgActByHour[[1]],
       type = "n",
       ylim = c(0, maxAct),
       bty = "n",
       las = 1,
       xlab = "",
       ylab = "Average Hourly Activity",
       ...)
  
  for(i in seq_along(AvgActByHour)) {
    lines(x, AvgActByHour[[i]], col = "grey75")
  }
  
  lines(x, meanXind(AvgActByHour), col = "blue", lwd = 2)
  
}
