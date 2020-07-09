# This function is part of the `abcdee` R package <https://www.r-project.org/>.
# It extracts endpoints (e.g. Zeitgeber times) from a {xts} time series object
# Copyright (C) 2019 Carlo Costantini

# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation, either version 3 of the License, or any later version.

# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along with this program.
# If not, see <https://www.gnu.org/licenses/>.

extract.timepoints <- function(series, from = "00:00:00", to = "23:59:59", tz = .sys.timezone) {
  
  if(!class(series) == "xts") stop() # check first that time series is of class {xts}
  
  require(lubridate)
  
  d <- unique(date(series))
  T1 <- as.POSIXct(paste(d, rep(from, length(d))), tz = tz)
  T2 <- as.POSIXct(paste(d, rep(to, length(d))), tz = tz)
  tps <- list()
  for(i in 1:(length(d)-1)) {
    tps[[i]] <- list(from = T1[i], to = T2[i+1])
  }
  
  return(tps)
}