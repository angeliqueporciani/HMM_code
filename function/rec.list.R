# function to create recursive nested list structure
# (source <https://stackoverflow.com/questions/17567172/nested-lists-how-to-define-the-size-before-entering-data>)
rec.list <- function(len) {
  if (length(len) == 1) {
    vector("list", len)
  } else {
    lapply(1:len[1], function(...) rec.list(len[-1]))
  }
}