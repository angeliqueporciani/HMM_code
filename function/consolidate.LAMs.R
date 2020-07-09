# function to consolidate several LAM dataframes and their corresponding metadata into a single list of class XTS
# metadata are attached to each single LAM channel (an XTS object) as attributes
# the LAM dataframes must be saved previously in a list object that is called as the first argument of the function

# brute force approach (sorting order of monitor and channel numbers must coincide in LAMs list and Metadata dataframe)
# a safer approach would be to create a key field with Monitor and Channel Number merged on both objects
# and use join function to merge objects using the key field

consolidate.LAMs <- function(LAMs = list, Metadata = data.frame) {
  require(xts)

  activity <- list()
  for (i in 1:length(LAMs)) {
    for (j in 1:32) {
      activity[[32 * (i - 1) + j]] <- xts(LAMs[[i]][10 + j], order.by = LAMs[[i]]$DateTime)
      names(activity[[32 * (i - 1) + j]]) <- paste0("M", i, ".", names(LAMs[[i]][10 + j]))
    }
  }

  for (i in 1:dim(Metadata)[2]) {
    for (j in 1:dim(Metadata)[1]) {
      attr(activity[[j]], names(Metadata)[i]) <- Metadata[j, i]
    }
  }

  return(activity)
}