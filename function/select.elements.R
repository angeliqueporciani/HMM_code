select.elements <- function(list, attribute, value) {
  Filter(function(x) attr(x, which = attribute, exact = TRUE) == value, list)
}