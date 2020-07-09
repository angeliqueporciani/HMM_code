plot.raw.activity <- function(activity, LtoD = "18:00:00", DtoL = "06:00:00", mov.average = 1) {
  
  require(tidyverse); require(dygraphs)
  
  x <- activity
  
  DD_periods <- extract.timepoints(x, from = LtoD, to = DtoL, tz = .sys.timezone)
  
  add_shades <- function(x, periods, ...) {
    for(period in periods) {
      x <- dyShading(x, from = period$from , to = period$to, ...)
    }
    x
  }
  
  dygraph(x) %>%
    dyOptions(fillGraph = TRUE, fillAlpha = 0.4, stepPlot = TRUE) %>%
    dyAxis("y", label = "Average (raw) No. Recordings / Time Unit") %>%
    dyRoller(rollPeriod = mov.average) %>%  add_shades(DD_periods, color = "rgb(178,178,178)") %>%
    dyRangeSelector(height=20)
}