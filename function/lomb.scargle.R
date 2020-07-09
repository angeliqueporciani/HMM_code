lomb.scargle <- function(xts_obj) {
  
  require(lomb)
  
  lsp <- lsp(x = as.numeric(xts_obj), type = "period", plot = FALSE)
  df <- data.frame(lsp$scanned/60, lsp$power - lsp$sig.level)
  names(df) <- c("Period", "Power")
  return(df)
  
}